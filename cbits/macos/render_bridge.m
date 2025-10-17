#define GL_SILENCE_DEPRECATION 1
#import "render_bridge.h"
#import <Foundation/Foundation.h>
#import <AppKit/AppKit.h>
#import <ScreenCaptureKit/ScreenCaptureKit.h>
#import <CoreVideo/CoreVideo.h>
#import <CoreMedia/CoreMedia.h>
#import <OpenGL/gl3.h>
#import <OpenGL/CGLCurrent.h>
#import <IOSurface/IOSurface.h>
#import <stdatomic.h>

@interface _RBOut : NSObject <SCStreamOutput> {
@public
    _Atomic(CVPixelBufferRef) latest;   // real field, not a property, we need atomic access...
}
@end

static inline void runOnMainSync(void (^block)(void)) {
  if ([NSThread isMainThread]) { block(); }
  else { dispatch_sync(dispatch_get_main_queue(), block); }
}

@implementation _RBOut
- (void)dealloc {
    CVPixelBufferRef prev =
        atomic_exchange_explicit(&latest, (CVPixelBufferRef)NULL, memory_order_acq_rel);
    if (prev) CFRelease(prev);
}
- (void)stream:(SCStream*)s didOutputSampleBuffer:(CMSampleBufferRef)sb ofType:(SCStreamOutputType)t {
    if (t != SCStreamOutputTypeScreen) return;
    CVImageBufferRef img = CMSampleBufferGetImageBuffer(sb);
    if (!img) return;
    CVPixelBufferRef pb = (CVPixelBufferRef)CFRetain(img);
    CVPixelBufferRef prev =
        atomic_exchange_explicit(&latest, pb, memory_order_acq_rel);
    if (prev) CFRelease(prev);
#if !__has_feature(objc_arc)
  [super dealloc];
#endif
}
@end

typedef struct RB {
  SCStream *stream;
  _RBOut   *out;
  int       gap_px;
} RB;

static NSScreen* screenForWindow(SCWindow* w) {
  NSPoint c = NSMakePoint(NSMidX(w.frame), NSMidY(w.frame));
  for (NSScreen* s in [NSScreen screens]) if (NSPointInRect(c, s.frame)) return s;
  return [NSScreen mainScreen];
}

static SCShareableContent* fetchContent(NSError** outErr) {
  __block SCShareableContent* content = nil;
  __block NSError* err = nil;
  dispatch_semaphore_t sem = dispatch_semaphore_create(0);
  [SCShareableContent getShareableContentWithCompletionHandler:^(SCShareableContent* c, NSError* e){
    content = c; err = e; dispatch_semaphore_signal(sem);
  }];
  dispatch_semaphore_wait(sem, DISPATCH_TIME_FOREVER);
  if (outErr) *outErr = err;
  return content;
}

RB* rb_create_window(void) {
  RB* rb = (RB*)calloc(1, sizeof(RB));
  rb->gap_px = 1;
  return rb;
}

RB* rb_create_display(void) {
  RB* rb = (RB*)calloc(1, sizeof(RB));
  rb->gap_px = 0;
  return rb;
}

void rb_destroy(RB* rb) {
  if (!rb) return;
  // stopCapture releases latest
  if (rb->stream || rb->out) {
    runOnMainSync(^{
      if (rb->stream) {
        [rb->stream stopCaptureWithCompletionHandler:^(__unused NSError* e){}];
        rb->stream = nil;
      }
      if (rb->out) {
        CVPixelBufferRef prev = atomic_exchange_explicit(&rb->out->latest, (CVPixelBufferRef)NULL, memory_order_acq_rel);
        if (prev) CFRelease(prev);
        rb->out = nil;
      }
    });
  }
  free(rb);
}

int rb_start_capture_window(unsigned long long window_id, RB* rb, char* title_buf, size_t title_cap) {
  if (!rb || !title_buf || title_cap == 0) return -1;
  __block int rc = 0;
  runOnMainSync(^{
    NSError* err = nil;
    SCShareableContent* content = fetchContent(&err);
    if (!content) { rc = -2; return; }

    SCWindow* target = nil;
    for (SCWindow* w in content.windows) if ((uint64_t)w.windowID == window_id) { target = w; break; }
    if (!target) { rc = -3; return; }

    // Build title with fallbacks
    NSString *title = target.title;
    if (title.length == 0) {
      title = target.owningApplication.applicationName ?: @"unknown";
    }
    // Copy UTF-8 safely
    BOOL ok = [title getCString:title_buf
                      maxLength:title_cap
                       encoding:NSUTF8StringEncoding];
    if (!ok) {
      // truncate manually if needed
      NSData *d = [title dataUsingEncoding:NSUTF8StringEncoding] ?: [NSData data];
      size_t n = MIN((size_t)d.length, title_cap ? title_cap - 1 : 0);
      memcpy(title_buf, d.bytes, n);
      title_buf[n] = 0;
    }

    NSScreen* host = screenForWindow(target);
    CGFloat scale = host.backingScaleFactor ?: 1.0;
    NSInteger pxW = llround(target.frame.size.width  * scale);
    NSInteger pxH = llround(target.frame.size.height * scale);
    if (pxW <= 0 || pxH <= 0) { rc = -4; return; }

    SCContentFilter* filter = [[SCContentFilter alloc] initWithDesktopIndependentWindow:target];
    SCStreamConfiguration* cfg = [SCStreamConfiguration new];
    cfg.pixelFormat = kCVPixelFormatType_32BGRA;
    cfg.width  = pxW;
    cfg.height = pxH;
    cfg.showsCursor = NO;
    cfg.capturesAudio = NO;

    _RBOut* out = [_RBOut new];
    rb->out = out;

    SCStream* stream = [[SCStream alloc] initWithFilter:filter configuration:cfg delegate:nil];
    rb->stream = stream;
    if (!stream) { rb->out = nil; rc = -5; return; }

    dispatch_queue_t q = dispatch_queue_create("rb.sc.stream", DISPATCH_QUEUE_SERIAL);
    if (![stream addStreamOutput:out type:SCStreamOutputTypeScreen sampleHandlerQueue:q error:&err]) {
      rb->stream = nil; rb->out = nil; rc = -6; return;
    }
    [stream startCaptureWithCompletionHandler:^(__unused NSError* e){}];
  });
  return rc;
}

void rb_stop_capture(RB* rb) {
  if (!rb) return;
  runOnMainSync(^{
    if (rb->stream) {
      [rb->stream stopCaptureWithCompletionHandler:^(__unused NSError* e){}];
      rb->stream = nil;
    }
    if (rb->out) {
      CVPixelBufferRef prev = atomic_exchange_explicit(&rb->out->latest, (CVPixelBufferRef)NULL, memory_order_acq_rel);
      if (prev) CFRelease(prev);
      rb->out = nil;
    }
  });
}

int rb_poll_frame(RB* rb, RBFrame* out) {
  if (!rb || !out || !rb->out) return -1;
  CVPixelBufferRef pb = atomic_exchange_explicit(&rb->out->latest, (CVPixelBufferRef)NULL, memory_order_acq_rel);
  if (!pb) return 0;

  const int g = rb->gap_px;
  size_t w0 = CVPixelBufferGetWidth(pb);
  size_t h0 = CVPixelBufferGetHeight(pb);
  OSType fmt = CVPixelBufferGetPixelFormatType(pb);

  size_t w = (w0 > 2*g) ? (w0 - 2*g) : 0;
  size_t h = (h0 > 2*g) ? (h0 - 2*g) : 0;

  IOSurfaceRef surf = CVPixelBufferGetIOSurface(pb);
  if (surf && fmt == kCVPixelFormatType_32BGRA) {
    out->kind    = RB_IOSURFACE;
    out->width   = (int)w;
    out->height  = (int)h;
    out->stride  = 0;
    out->payload = surf;     // borrowed via pb
    out->handle  = pb;       // retained
    return 1;
  }

  out->kind    = RB_BYTES;
  out->width   = (int)w;
  out->height  = (int)h;
  out->stride  = (int)CVPixelBufferGetBytesPerRow(pb);
  out->payload = NULL;       // base set in upload
  out->handle  = pb;         // retained
  return 1;
}

void rb_release_frame(RB* rb, RBFrame* fr) {
  (void)rb;
  if (fr && fr->handle) {
    CVPixelBufferRef pb = (CVPixelBufferRef)fr->handle;
    CFRelease(pb);
    fr->handle = NULL;
  }
}

static int uploadIOSurface(const RBFrame* fr) {
  IOSurfaceRef surf = (IOSurfaceRef)fr->payload;
  if (!surf) return -1;

  GLint binding = 0; glGetIntegerv(GL_TEXTURE_BINDING_RECTANGLE, &binding);
  if (binding == 0) return -2;

  CGLContextObj cgl = CGLGetCurrentContext();
  if (!cgl) return -3;

  CGLError e = CGLTexImageIOSurface2D(cgl, GL_TEXTURE_RECTANGLE, GL_RGBA,
                                      fr->width, fr->height,
                                      GL_BGRA, GL_UNSIGNED_INT_8_8_8_8_REV,
                                      surf, 0);
  if (e != kCGLNoError) return -4;

  glTexParameteri(GL_TEXTURE_RECTANGLE, GL_TEXTURE_BASE_LEVEL, 0);
  glTexParameteri(GL_TEXTURE_RECTANGLE, GL_TEXTURE_MAX_LEVEL, 0);
  glTexParameteri(GL_TEXTURE_RECTANGLE, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_RECTANGLE, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_RECTANGLE, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_RECTANGLE, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  return 0;
}

static int uploadBytes(const RBFrame* fr) {
  CVPixelBufferRef pb = (CVPixelBufferRef)fr->handle;
  if (!pb) return -1;
  if (CVPixelBufferGetPixelFormatType(pb) != kCVPixelFormatType_32BGRA) return -2;

  if (CVPixelBufferLockBaseAddress(pb, kCVPixelBufferLock_ReadOnly) != kCVReturnSuccess) return -3;
  const uint8_t* base = (const uint8_t*)CVPixelBufferGetBaseAddress(pb);
  size_t stride = CVPixelBufferGetBytesPerRow(pb);

  glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
  glPixelStorei(GL_UNPACK_ROW_LENGTH, (GLint)(stride / 4)); // 4 bytes/pixel

  glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0,
                  fr->width, fr->height,
                  GL_BGRA, GL_UNSIGNED_BYTE, base);

  glPixelStorei(GL_UNPACK_ROW_LENGTH, 0);
  CVPixelBufferUnlockBaseAddress(pb, kCVPixelBufferLock_ReadOnly);
  return 0;
}

int rb_upload_to_bound_texture(RB* rb, const RBFrame* fr) {
  (void)rb;
  if (!fr) return -1;

  GLint rectBinding = 0;
  glGetIntegerv(GL_TEXTURE_BINDING_RECTANGLE, &rectBinding);

  if (rectBinding != 0 && fr->kind == RB_IOSURFACE) {
    return uploadIOSurface(fr);              // GL_TEXTURE_RECTANGLE path
  }
  return uploadBytes(fr);                    // GL_TEXTURE_2D path
}

void rb_set_gap(RB* rb, int px) { if (rb) rb->gap_px = px; }

static NSString* rb_displayName(CGDirectDisplayID did) {
  for (NSScreen *s in [NSScreen screens]) {
    NSNumber *num = s.deviceDescription[@"NSScreenNumber"];
    if (num && num.unsignedIntValue == did) {
      if (@available(macOS 10.15, *)) {
        if ([s respondsToSelector:@selector(localizedName)] && s.localizedName.length)
          return s.localizedName;
      }
      break;
    }
  }
  return @"Display";
}

int rb_start_capture_display(unsigned long long display_id,
                             void *overlay_nswindow,
                             RB *rb,
                             char *title_buf,
                             size_t title_cap)
{
  if (!rb || !title_buf || title_cap == 0 || !overlay_nswindow) return -1;

  __block int rc = 0;
  runOnMainSync(^{
    NSError *err = nil;
    SCShareableContent *content = fetchContent(&err);
    if (!content) { rc = -2; return; }


    CGDirectDisplayID did = (CGDirectDisplayID)display_id;

    // Find SCDisplay by id
    SCDisplay *target = nil;
    for (SCDisplay *d in content.displays) {
      if ((uint64_t)d.displayID == (uint64_t)display_id) { target = d; break; }
    }
    if (!target) { rc = -3; return; }

    // Build a human title
    NSString *title = rb_displayName(did);
    if (title.length == 0) title = @"Display";
    // copy to out
    BOOL ok = [title getCString:title_buf maxLength:title_cap encoding:NSUTF8StringEncoding];
    if (!ok) {
      NSData *d = [title dataUsingEncoding:NSUTF8StringEncoding] ?: [NSData data];
      size_t n = MIN((size_t)d.length, title_cap ? title_cap - 1 : 0);
      memcpy(title_buf, d.bytes, n);
      title_buf[n] = 0;
    }

    // Dimensions
    CGDisplayModeRef mode = CGDisplayCopyDisplayMode(did);
    // We make sure to get pixel dimensions, otherwise backing scale may make
    // things complicated and require special care in the GL upload path.
    size_t pxW = CGDisplayModeGetPixelWidth(mode);
    size_t pxH = CGDisplayModeGetPixelHeight(mode);
    if (mode) CFRelease(mode);
    if (pxW <= 0 || pxH <= 0) {
      CGSize sz = CGDisplayScreenSize(did);
      CGRect b = CGDisplayBounds(did);
      pxW = (NSInteger)CGRectGetWidth(b);
      pxH = (NSInteger)CGRectGetHeight(b);
    }
    if (pxW <= 0 || pxH <= 0) { rc = -4; return; }

    NSArray<SCWindow*> *exclude = nil;
    NSWindow *ow = (__bridge NSWindow *)overlay_nswindow;
    CGWindowID ownID = (CGWindowID)ow.windowNumber;
    // Map to SCWindow in current content
    for (SCWindow *w in content.windows) {
      if ((CGWindowID)w.windowID == ownID) { exclude = @[w]; break; }
    }

    // Content filter for the display
    SCContentFilter *filter = nil;
    filter = [[SCContentFilter alloc] initWithDisplay:target excludingWindows:exclude];

    // Stream config
    SCStreamConfiguration *cfg = [SCStreamConfiguration new];
    cfg.pixelFormat = kCVPixelFormatType_32BGRA;
    cfg.width  = pxW;
    cfg.height = pxH;
    cfg.showsCursor = NO;
    cfg.capturesAudio = NO;

    printf("rb: display %llu -> %zux%zu\n", (uint64_t)display_id, (size_t)pxW, (size_t)pxH);

    _RBOut *out = [_RBOut new];
    rb->out = out;

    SCStream *stream = [[SCStream alloc] initWithFilter:filter configuration:cfg delegate:nil];
    rb->stream = stream;
    if (!stream) { rb->out = nil; rc = -5; return; }

    dispatch_queue_t q = dispatch_queue_create("rb.sc.stream", DISPATCH_QUEUE_SERIAL);
    if (![stream addStreamOutput:out type:SCStreamOutputTypeScreen sampleHandlerQueue:q error:&err]) {
      rb->stream = nil; rb->out = nil; rc = -6; return;
    }
    [stream startCaptureWithCompletionHandler:^(__unused NSError* e){}];
  });
  return rc;
}
