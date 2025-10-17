#import "SCShim.h"
#import <Foundation/Foundation.h>
#import <ScreenCaptureKit/ScreenCaptureKit.h>
#import <CoreMedia/CoreMedia.h>
#import <CoreVideo/CoreVideo.h>
#import <AppKit/AppKit.h>
#import <Cocoa/Cocoa.h>

static SCStream *gStream = nil;
static id gOut = nil;

static SCShareableContent* fetchContent(NSError **outErr) {
  __block SCShareableContent *content = nil;
  __block NSError *err = nil;
  dispatch_semaphore_t sem = dispatch_semaphore_create(0);
  [SCShareableContent getShareableContentWithCompletionHandler:^(SCShareableContent *c, NSError *e){
    content = c; err = e;
    dispatch_semaphore_signal(sem);
  }];
  dispatch_semaphore_wait(sem, DISPATCH_TIME_FOREVER);
  if (outErr) *outErr = err;
  return content;
}

void sc_list_windows(WindowCB cb, void* user) {
  NSError *err = nil;
  SCShareableContent *content = fetchContent(&err);
  if (!content) {
    fprintf(stderr, "sc_list_windows: failed to fetch content: %s\n",
            err ? err.localizedDescription.UTF8String : "unknown error");
    return;
  }

  for (SCWindow *w in content.windows) {
    if (!w.isOnScreen) continue;                          // skip off-screen / hidden
    if (w.frame.size.width < 1 || w.frame.size.height < 1) continue; // skip zero-sized
    if (w.owningApplication == nil) continue;             // skip orphan
    // skip untitled transient windows.
    if (w.title == nil || [w.title length] == 0) continue;

    const char* t = w.title.UTF8String;
    cb((uint64_t)w.windowID, t, user);
  }
}

static int bounds_for_window(uint64_t wid, SCRect* out) {
  if (!out) return -1;

  CFArrayRef arr = CGWindowListCopyWindowInfo(kCGWindowListOptionAll, kCGNullWindowID);
  if (!arr) return -2;

  int rc = -3;
  CFIndex n = CFArrayGetCount(arr);
  for (CFIndex i = 0; i < n; i++) {
    CFDictionaryRef d = (CFDictionaryRef)CFArrayGetValueAtIndex(arr, i);
    CFNumberRef num = (CFNumberRef)CFDictionaryGetValue(d, kCGWindowNumber);
    int64_t id64 = 0;
    if (num) CFNumberGetValue(num, kCFNumberSInt64Type, &id64);
    if ((uint64_t)id64 != wid) continue;

    CFDictionaryRef b = (CFDictionaryRef)CFDictionaryGetValue(d, kCGWindowBounds);
    if (!b) break;

    CGRect r;
    if (!CGRectMakeWithDictionaryRepresentation(b, &r)) break;

    // r is in global screen space, origin at top-left (y down)
    // Adding a small inset so we don't fully occlude GPU-backed windows
    const int inset = 1;
    if (r.size.width > 2*inset && r.size.height > 2*inset) {
        out->x = (int)llround(r.origin.x) + inset;
        out->y = (int)llround(r.origin.y) + inset;
        out->w = (int)llround(r.size.width)  - 2*inset;
        out->h = (int)llround(r.size.height) - 2*inset;
    } else {
        out->x = (int)llround(r.origin.x);
        out->y = (int)llround(r.origin.y);
        out->w = (int)llround(r.size.width);
        out->h = (int)llround(r.size.height);
    }
    rc = 0;
    break;
  }
  CFRelease(arr);
  return rc;
}

int sc_get_window_rect(uint64_t wid, SCRect* out) {
  return bounds_for_window(wid, out);
}


int sc_get_display_rect(uint64_t display_id, SCRect* out) {
  if (!out) return -1;

  CGDirectDisplayID did = (CGDirectDisplayID)display_id;

  // Validate the display id
  uint32_t cnt = 0;
  if (CGGetOnlineDisplayList(0, NULL, &cnt) != kCGErrorSuccess) return -2;
  if (cnt == 0) return -3;

  CGDirectDisplayID list[32];
  uint32_t n = MIN(cnt, (uint32_t)(sizeof(list)/sizeof(list[0])));
  if (CGGetOnlineDisplayList(n, list, &cnt) != kCGErrorSuccess) return -4;

  bool found = false;
  for (uint32_t i = 0; i < cnt; ++i) {
    if (list[i] == did) { found = true; break; }
  }
  if (!found) return -5;

  // Report pixel size; anchor at (0,0) (top-left origin for your API)
  out->x = 0;
  out->y = 0;
  out->w = (int)CGDisplayPixelsWide(did);
  out->h = (int)CGDisplayPixelsHigh(did);
  return 0;
}

@interface _SCOut : NSObject <SCStreamOutput>
@property(nonatomic, assign) FrameCB fcb;
@property(nonatomic, assign) StopCB scb;
@property(nonatomic, assign) void* user;
@end
@implementation _SCOut
- (void)stream:(SCStream *)stream didOutputSampleBuffer:(CMSampleBufferRef)sb ofType:(SCStreamOutputType)type {
  if (type != SCStreamOutputTypeScreen) return;
  CVImageBufferRef img = CMSampleBufferGetImageBuffer(sb);
  if (!img) return;
  CVPixelBufferLockBaseAddress(img, kCVPixelBufferLock_ReadOnly);
  const uint8_t* p = (const uint8_t*)CVPixelBufferGetBaseAddress(img);
  size_t w = CVPixelBufferGetWidth(img);
  size_t h = CVPixelBufferGetHeight(img);
  size_t stride = CVPixelBufferGetBytesPerRow(img);
  if (self.fcb) self.fcb(p,(int)w,(int)h,(int)stride,self.user);
  CVPixelBufferUnlockBaseAddress(img, kCVPixelBufferLock_ReadOnly);
}
@end

void sc_stop(void) {
  if (!gStream) return;
  [gStream stopCaptureWithCompletionHandler:^(__unused NSError *e) {}];
  gStream = nil;
  gOut = nil;
}

int sc_start_window(uint64_t wid, FrameCB fcb, StopCB on_stop, void* user) {
  @autoreleasepool {
    NSError *err = nil;
    SCShareableContent *content = fetchContent(&err);
    if (!content) return -1;

    SCWindow *target = nil;
    for (SCWindow* w in content.windows) if ((uint64_t)w.windowID == wid) { target = w; break; }
    if (!target) return -2;

    // host screen + scale
    NSPoint center = NSMakePoint(NSMidX(target.frame), NSMidY(target.frame));
    NSScreen *host = nil;
    for (NSScreen *s in [NSScreen screens]) { if (NSPointInRect(center, s.frame)) { host = s; break; } }
    if (!host) host = [NSScreen mainScreen];
    CGFloat scale = host.backingScaleFactor ?: 1.0;

    NSInteger pxW = (NSInteger)llround(target.frame.size.width  * scale);
    NSInteger pxH = (NSInteger)llround(target.frame.size.height * scale);
    if (pxW <= 0 || pxH <= 0) return -5;

    SCContentFilter *filter = [[SCContentFilter alloc] initWithDesktopIndependentWindow:target];
    SCStreamConfiguration *cfg = [SCStreamConfiguration new];
    cfg.pixelFormat = kCVPixelFormatType_32BGRA;
    cfg.width  = pxW;
    cfg.height = pxH;
    cfg.showsCursor = NO;
    cfg.capturesAudio = NO;

    _SCOut *out = [_SCOut new];
    out.fcb = fcb; out.scb = on_stop; out.user = user;
    gOut = out;

    gStream = [[SCStream alloc] initWithFilter:filter configuration:cfg delegate:nil];
    if (!gStream) { gOut = nil; return -3; }

    // use main queue first to rule out thread/GC issues
    if (![gStream addStreamOutput:out type:SCStreamOutputTypeScreen
              sampleHandlerQueue:dispatch_get_main_queue() error:&err]) {
      gStream = nil; gOut = nil; return -4;
    }

    [gStream startCaptureWithCompletionHandler:^(NSError *e){
      if (e && on_stop) on_stop(user);
    }];
    return 0;
  }
}

int sc_preflight_screen(void) {
  return CGPreflightScreenCaptureAccess() ? 1 : 0;
}

int sc_request_screen(void) {
  // Triggers the OS prompt if not granted. Blocks until user responds.
  return CGRequestScreenCaptureAccess() ? 1 : 0;
}

int sc_tcc_reset(const char* bundle_id_or_null) {
  @autoreleasepool {
    NSString *tool = @"/usr/bin/tccutil";
    if (![[NSFileManager defaultManager] isExecutableFileAtPath:tool]) return 2;

    NSMutableArray<NSString*> *args = [NSMutableArray arrayWithObjects:@"reset", @"ScreenCapture", nil];
    if (bundle_id_or_null && bundle_id_or_null[0] != '\0') {
      [args addObject:[NSString stringWithUTF8String:bundle_id_or_null]];
    }
    NSTask *task = [NSTask new];
    task.launchPath = tool;
    task.arguments = args;
    @try {
      [task launch];
      [task waitUntilExit];
      return task.terminationStatus; // 0 on success
    } @catch (__unused NSException *e) {
      return 3;
    }
  }
}

static CGWindowID topWindowAt(CGPoint p) {
  CFArrayRef list = CGWindowListCopyWindowInfo(kCGWindowListOptionOnScreenOnly |
                                               kCGWindowListExcludeDesktopElements,
                                               kCGNullWindowID);
  if (!list) return 0;
  CGWindowID best = 0;
  CFIndex n = CFArrayGetCount(list);
  for (CFIndex i = 0; i < n; ++i) {
    CFDictionaryRef d = CFArrayGetValueAtIndex(list, i);
    // layer 0 = normal app windows
    int layer = 0;
    CFNumberRef layerNum = CFDictionaryGetValue(d, kCGWindowLayer);
    if (layerNum) CFNumberGetValue(layerNum, kCFNumberIntType, &layer);
    if (layer != 0) continue;

    CGRect r;
    if (CGRectMakeWithDictionaryRepresentation(CFDictionaryGetValue(d, kCGWindowBounds), &r)) {
      if (CGRectContainsPoint(r, p)) {
        CFNumberRef widNum = CFDictionaryGetValue(d, kCGWindowNumber);
        if (widNum) { CFNumberGetValue(widNum, kCGWindowIDCFNumberType, &best); break; }
      }
    }
  }
  CFRelease(list);
  return best;
}

static inline void runOnMainSync(void (^block)(void)) {
  if ([NSThread isMainThread]) { block(); }
  else { dispatch_sync(dispatch_get_main_queue(), block); }
}

int sc_pick_application(PickCB cb, void *user) {
  __block int rc = -1;

  runOnMainSync(^{
    if (NSApp == nil) [NSApplication sharedApplication];
    [NSApp activateIgnoringOtherApps:YES];

    NSError *err = nil;
    SCShareableContent *content = fetchContent(&err);
    if (!content) { rc = -1; return; }

    // Build candidate list (exclude our own process)
    pid_t selfPid = getpid();
    NSMutableArray<SCRunningApplication*> *apps = [NSMutableArray array];
    for (SCRunningApplication *a in content.applications) {
      if (a.processID == selfPid) continue;
      // Optional: skip background-only processes
      if (a.applicationName.length == 0) continue;
      [apps addObject:a];
    }
    if (apps.count == 0) { rc = -1; return; }

    // Sort by app name
    [apps sortUsingComparator:^NSComparisonResult(SCRunningApplication *a, SCRunningApplication *b) {
      NSString *an = a.applicationName ?: @"";
      NSString *bn = b.applicationName ?: @"";
      return [an caseInsensitiveCompare:bn];
    }];

    // UI
    NSAlert *alert = [[NSAlert alloc] init];
    alert.messageText = @"Pick an application to capture";
    alert.informativeText = @"Fullscreen apps are supported.";
    [alert addButtonWithTitle:@"Capture"];
    [alert addButtonWithTitle:@"Cancel"];

    NSPopUpButton *popup = [[NSPopUpButton alloc] initWithFrame:NSMakeRect(0,0,460,26)
                                                     pullsDown:NO];

    for (SCRunningApplication *a in apps) {
      NSString *name = a.applicationName ?: @"(Unnamed App)";
      // Show bundle id as hint if available
      NSString *hint = a.bundleIdentifier ?: @"";
      NSString *label = hint.length ? [NSString stringWithFormat:@"%@ — %@", name, hint] : name;
      [popup addItemWithTitle:label];
      NSMenuItem *item = popup.lastItem;
      item.representedObject = @( (uint64_t)a.processID ); // PID as ID
    }

    NSView *container = [[NSView alloc] initWithFrame:NSMakeRect(0,0,480,32)];
    [container addSubview:popup];
    alert.accessoryView = container;

    // Make the modal sit on top and visible across Spaces
    NSWindow *aw = alert.window;
    aw.level = NSStatusWindowLevel;
    aw.collectionBehavior |= NSWindowCollectionBehaviorCanJoinAllSpaces;
    [aw center];
    [aw makeKeyAndOrderFront:nil];
    [aw orderFrontRegardless];
    [NSApp activateIgnoringOtherApps:YES];

    NSModalResponse resp = [alert runModal];
    if (resp != NSAlertFirstButtonReturn) { rc = -1; return; }

    NSMenuItem *sel = popup.selectedItem;
    if (!sel || !sel.representedObject) { rc = -1; return; }

    uint64_t pid = [(NSNumber*)sel.representedObject unsignedLongLongValue];

    // Find chosen app to get a stable title (name)
    SCRunningApplication *chosen = nil;
    for (SCRunningApplication *a in apps) { if ((uint64_t)a.processID == pid) { chosen = a; break; } }
    NSString *title = chosen ? (chosen.applicationName ?: @"") : @"";
    const char *cTitle = title.UTF8String ?: "";

    if (cb) cb(pid, cTitle, user);
    rc = 0;
  });

  return rc;
}

int sc_pick_window(PickCB cb, void *user) {
  __block int rc = -1;

  runOnMainSync(^{
    if (NSApp == nil) [NSApplication sharedApplication];
    [NSApp activateIgnoringOtherApps:YES];

    NSError *err = nil;
    SCShareableContent *content = fetchContent(&err);
    if (!content) { rc = -1; return; }

    NSMutableArray<SCWindow*> *wins = [NSMutableArray array];
    pid_t selfPid = getpid();
    for (SCWindow *w in content.windows) {
      if (!w.isOnScreen) continue;
      if (w.frame.size.width < 1 || w.frame.size.height < 1) continue;
      if (w.owningApplication == nil) continue;
      [wins addObject:w];
    }
    if (wins.count == 0) { rc = -1; return; }

    [wins sortUsingComparator:^NSComparisonResult(SCWindow *a, SCWindow *b) {
      NSString *an = a.owningApplication.applicationName ?: @"";
      NSString *bn = b.owningApplication.applicationName ?: @"";
      NSComparisonResult r = [an caseInsensitiveCompare:bn];
      if (r != NSOrderedSame) return r;
      NSString *at = a.title.length ? a.title : @"(Untitled)";
      NSString *bt = b.title.length ? b.title : @"(Untitled)";
      return [at caseInsensitiveCompare:bt];
    }];

    NSAlert *alert = [[NSAlert alloc] init];
    alert.messageText = @"Pick a window to capture";
    alert.informativeText = @"Choose from currently visible windows.";
    [alert addButtonWithTitle:@"Capture"];
    [alert addButtonWithTitle:@"Cancel"];

    NSPopUpButton *popup = [[NSPopUpButton alloc] initWithFrame:NSMakeRect(0,0,460,26)
                                                     pullsDown:NO];

    for (SCWindow *w in wins) {
      NSString *app = w.owningApplication.applicationName ?: @"";
      NSString *title = w.title.length ? w.title : @"(Untitled)";
      NSString *label = [NSString stringWithFormat:@"%@ — %@", app, title];
      [popup addItemWithTitle:label];
      NSMenuItem *item = popup.lastItem;
      item.representedObject = @((uint64_t)w.windowID);
    }

    NSView *container = [[NSView alloc] initWithFrame:NSMakeRect(0,0,480,32)];
    [container addSubview:popup];
    alert.accessoryView = container;

    // Make sure it’s on top
    NSWindow *aw = alert.window;
    aw.level = NSStatusWindowLevel;
    aw.collectionBehavior |= NSWindowCollectionBehaviorCanJoinAllSpaces;
    [aw center];
    [aw makeKeyAndOrderFront:nil];
    [aw orderFrontRegardless];
    [NSApp activateIgnoringOtherApps:YES];

    NSModalResponse resp = [alert runModal];
    if (resp != NSAlertFirstButtonReturn) { rc = -1; return; }

    NSMenuItem *sel = popup.selectedItem;
    if (!sel || !sel.representedObject) { rc = -1; return; }

    uint64_t wid = [(NSNumber*)sel.representedObject unsignedLongLongValue];

    // Title fetching
    SCWindow *chosen = nil;
    for (SCWindow *w in wins) { if ((uint64_t)w.windowID == wid) { chosen = w; break; } }
    NSString *title = chosen ? (chosen.title.length ? chosen.title : @"(Untitled)") : @"";
    const char *cTitle = title.UTF8String ?: "";

    if (cb) cb(wid, cTitle, user);
    rc = 0;
  });

  return rc;
}

void setup_overlay(void* nswindow, int passthrough) {
  NSWindow *w = (__bridge NSWindow *)nswindow;
  if (!w) return;
  [w setLevel:NSFloatingWindowLevel];
  [w setCollectionBehavior:
     NSWindowCollectionBehaviorCanJoinAllSpaces |
     NSWindowCollectionBehaviorFullScreenAuxiliary |
     NSWindowCollectionBehaviorTransient];
  if (passthrough) [w setIgnoresMouseEvents:YES];
  [w setHidesOnDeactivate:NO];
}


static NSString* nameForDisplay(CGDirectDisplayID did) {
  for (NSScreen *s in [NSScreen screens]) {
    NSNumber *num = s.deviceDescription[@"NSScreenNumber"];
    if (num && num.unsignedIntValue == did) {
      if (@available(macOS 10.15, *)) {
        if ([s respondsToSelector:@selector(localizedName)]) {
          NSString *n = s.localizedName;
          if (n.length) return n;
        }
      }
      return @"Display";
    }
  }
  return @"Display";
}

int sc_pick_display(PickCB cb, void *user) {
  __block int rc = -1;

  runOnMainSync(^{
    if (NSApp == nil) [NSApplication sharedApplication];
    [NSApp activateIgnoringOtherApps:YES];

    NSError *err = nil;
    SCShareableContent *content = fetchContent(&err);
    if (!content) { rc = -1; return; }

    NSArray<SCDisplay*> *displays = content.displays;
    if (displays.count == 0) { rc = -1; return; }

    // Build popup list
    NSAlert *alert = [[NSAlert alloc] init];
    alert.messageText = @"Pick a display to capture";
    alert.informativeText = @"Choose one of the available displays.";
    [alert addButtonWithTitle:@"Capture"];
    [alert addButtonWithTitle:@"Cancel"];

    NSPopUpButton *popup = [[NSPopUpButton alloc] initWithFrame:NSMakeRect(0,0,480,26)
                                                     pullsDown:NO];

    for (SCDisplay *d in displays) {
      CGDirectDisplayID did = d.displayID; // stable numeric id
      NSString *dispName = nameForDisplay(did);
      // Include resolution & scale if available
      CGFloat scale = 1.0;
      for (NSScreen *s in [NSScreen screens]) {
        NSNumber *num = s.deviceDescription[@"NSScreenNumber"];
        if (num && num.unsignedIntValue == did) { scale = s.backingScaleFactor ?: 1.0; break; }
      }
      NSString *label = [NSString stringWithFormat:@"%@ — %dx%d @%.1fx",
                         dispName, (int)d.width, (int)d.height, (double)scale];
      [popup addItemWithTitle:label];
      popup.lastItem.representedObject = @((uint64_t)did);
    }

    NSView *container = [[NSView alloc] initWithFrame:NSMakeRect(0,0,500,32)];
    [container addSubview:popup];
    alert.accessoryView = container;

    // Show alert above everything (incl. overlay)
    NSWindow *aw = alert.window;
    aw.level = NSStatusWindowLevel;
    aw.collectionBehavior |= NSWindowCollectionBehaviorCanJoinAllSpaces |
                             NSWindowCollectionBehaviorTransient;
    [aw center];
    [aw makeKeyAndOrderFront:nil];
    [aw orderFrontRegardless];
    [NSApp activateIgnoringOtherApps:YES];

    NSModalResponse resp = [alert runModal];
    if (resp != NSAlertFirstButtonReturn) { rc = -1; return; }

    NSMenuItem *sel = popup.selectedItem;
    if (!sel || !sel.representedObject) { rc = -1; return; }

    uint64_t did = [(NSNumber*)sel.representedObject unsignedLongLongValue];
    NSString *title = sel.title ?: @"Display";
    const char *cTitle = title.UTF8String ?: "";

    if (cb) cb(did, cTitle, user);
    rc = 0;
  });

  return rc;
}
