#import "SCShim.h"
#import <Foundation/Foundation.h>
#import <ScreenCaptureKit/ScreenCaptureKit.h>
#import <CoreMedia/CoreMedia.h>
#import <CoreVideo/CoreVideo.h>
#import <AppKit/AppKit.h>

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

int sc_get_window_rect(uint64_t wid, SCRect* out) {
  if (!out) return -1;

  NSError *err = nil;
  SCShareableContent *content = fetchContent(&err);
  if (!content) return -2;

  SCWindow *target = nil;
  for (SCWindow* w in content.windows)
    if ((uint64_t)w.windowID == wid) { target = w; break; }
  if (!target) return -3;

  // Pick hosting screen by window center
  NSPoint center = NSMakePoint(NSMidX(target.frame), NSMidY(target.frame));
  NSScreen *host = nil;
  for (NSScreen *s in [NSScreen screens]) {
    if (NSPointInRect(center, s.frame)) { host = s; break; }
  }
  if (!host) host = [NSScreen mainScreen];

  NSScreen *primary = [NSScreen mainScreen];

  // Frames in global points; Y up.
  NSRect f = target.frame;
  CGFloat scaleHost = host.backingScaleFactor ?: 1.0;

  int xpx = (int)llround(f.origin.x * scaleHost);
  int wpx = (int)llround(f.size.width * scaleHost);
  int hpx = (int)llround(f.size.height * scaleHost);
  int ypx = (int)llround(f.origin.y * scaleHost);

  out->x = xpx;
  out->y = ypx;
  out->w = wpx;
  out->h = hpx;
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

int sc_pick_window(PickCB cb, void* user) {
  // Wait for a clean press: ensure button up first
  while (CGEventSourceButtonState(kCGEventSourceStateCombinedSessionState, kCGMouseButtonLeft))
    usleep(1000);

  // Now wait for press
  while (!CGEventSourceButtonState(kCGEventSourceStateCombinedSessionState, kCGMouseButtonLeft))
    usleep(1000);

  // On press, read global mouse location (top-left origin)
  CGEventRef e = CGEventCreate(NULL);
  if (!e) return -1;
  CGPoint p = CGEventGetLocation(e);
  CFRelease(e);

  CGWindowID wid = topWindowAt(p);
  if (!wid) return -1;

  CFNumberRef widNum = CFNumberCreate(kCFAllocatorDefault, kCFNumberSInt32Type, &wid);
  if (!widNum) return -1;

  // Array of one CFNumber
  const void* vals[1] = { widNum };
  CFArrayRef one = CFArrayCreate(kCFAllocatorDefault, vals, 1, &kCFTypeArrayCallBacks);

  // Query window description(s)
  CFArrayRef info = one ? CGWindowListCreateDescriptionFromArray(one) : NULL;

  char titleBuf[512] = {0};
  if (info && CFArrayGetCount(info) > 0) {
    CFDictionaryRef d = CFArrayGetValueAtIndex(info, 0);
    CFTypeRef name = d ? CFDictionaryGetValue(d, kCGWindowName) : NULL;
    if (name && CFGetTypeID(name) == CFStringGetTypeID()) {
      CFStringGetCString((CFStringRef)name, titleBuf, sizeof(titleBuf), kCFStringEncodingUTF8);
    }
  }

  // Cleanup
  if (info) CFRelease(info);
  if (one)  CFRelease(one);
  CFRelease(widNum);

  // Use titleBuf (stable C string)
  if (cb) cb((uint64_t)wid, titleBuf[0] ? titleBuf : "", user);
  return 0;
}
