#define GL_SILENCE_DEPRECATION 1
#if __has_feature(objc_arc)
  #define BRIDGE_RETAIN(x)   (__bridge_retained void *)(x)
  #define BRIDGE_ASSIGN(T,p) ((__bridge T)(p))
  #define RELEASE_STORED(p)  do { if (p) CFRelease((CFTypeRef)(p)); } while (0)
#else
  #define BRIDGE_RETAIN(x)   ((void *)[(x) retain])
  #define BRIDGE_ASSIGN(T,p) ((T)(p))
  #define RELEASE_STORED(p)  do { if (p) [(id)(p) release]; } while (0)
#endif

#import <Foundation/Foundation.h>
#import <dispatch/dispatch.h>
#import <stdatomic.h>

#import <AVFoundation/AVFoundation.h>
#import <AudioToolbox/AudioToolbox.h>

#import <ScreenCaptureKit/ScreenCaptureKit.h>
#import <CoreMedia/CoreMedia.h>
#import <Accelerate/Accelerate.h>

#import "audio_bridge.h"

@interface _AP : NSObject
@property(nonatomic,strong) AVAudioEngine* eng;
@property(nonatomic,strong) AVAudioPlayerNode* node;
@property(nonatomic,strong) AVAudioMixerNode*  mix;
@end
@implementation _AP @end

struct APHandle { void *o; }; // retained _AP* as CF-style pointer

static inline void onMainSync(void (^block)(void)) {
  if ([NSThread isMainThread]) { block(); }
  else { dispatch_sync(dispatch_get_main_queue(), block); }
}
static inline void onMainAsync(void (^block)(void)) {
  if ([NSThread isMainThread]) { block(); }
  else { dispatch_async(dispatch_get_main_queue(), block); }
}

static AVAudioFormat* makeFmt(int channels, int sampleRate) {
  return [[AVAudioFormat alloc] initStandardFormatWithSampleRate:(double)sampleRate
                                                         channels:(AVAudioChannelCount)channels];
}


APHandle* ap_create(void) {
  __block APHandle* h = NULL;
  onMainSync(^{
    _AP* o = [_AP new];
    o.eng = [AVAudioEngine new];
    o.node = [AVAudioPlayerNode new];
    [o.eng attachNode:o.node];
    [o.eng connect:o.node to:o.eng.mainMixerNode format:nil];

    NSError* err = nil;
    [o.eng startAndReturnError:&err];
    if (err) { return; }

    h = calloc(1, sizeof(APHandle));
    h->o = BRIDGE_RETAIN(o);
  });
  return h;
}

void ap_destroy(APHandle* h) {
  if (!h) return;
  onMainSync(^{
    _AP* o = (__bridge _AP*)h->o;
    [o.node stop];
    [o.eng  stop];
  });
  RELEASE_STORED(h->o);
  free(h);
}

void ap_stop_all(APHandle* h) {
  if (!h) return;
  onMainAsync(^{
      _AP* o = (__bridge _AP*)h->o;
      [o.node stop];
      });
}

int ap_play_pcm(APHandle* h, const void* bytes, int byteCount,
                int channels, int sampleRate, int bitsPerSample, float gain)
{
  if (!h || !h->o || !bytes)                     return -1; // bad handle/ptr
  if (byteCount <= 0 || channels <= 0)           return -2; // bad sizes
  if (bitsPerSample != 8 && bitsPerSample != 16) return -3; // unsupported bps
  if (sampleRate <= 0)                            return -4; // bad rate

  __block int rc = 0;
  onMainSync(^{
    _AP* obj = BRIDGE_ASSIGN(_AP*, h->o);
    if (!obj || !obj.eng || !obj.node) { rc = -5; return; }
    printf("ap_play_pcm: %d bytes, %d ch, %d Hz, %d bps, gain=%f\n",
           byteCount, channels, sampleRate, bitsPerSample, gain);
    AVAudioFormat* fmt = makeFmt(channels, sampleRate);
    if (!fmt) { rc = -5; return; }

    const int bytesPerSample = bitsPerSample / 8;
    const int bytesPerFrame  = channels * bytesPerSample;
    if (bytesPerFrame <= 0) { rc = -6; return; }

    AVAudioFrameCount frames = (AVAudioFrameCount)(byteCount / bytesPerFrame);
    if (frames == 0) { rc = -7; return; }

    AVAudioPCMBuffer* buf =
      [[AVAudioPCMBuffer alloc] initWithPCMFormat:fmt frameCapacity:frames];
    if (!buf || !buf.floatChannelData) { rc = -8; return; }
    buf.frameLength = frames;

    float* outL = buf.floatChannelData[0];
    float* outR = (channels > 1) ? buf.floatChannelData[1] : NULL;

    if (bitsPerSample == 8) {
      printf("  (8-bit samples)\n");
      const uint8_t* p = (const uint8_t*)bytes;
      for (AVAudioFrameCount i = 0; i < frames; i++) {
        const int idx = (int)i * channels;
        float l = ((int)p[idx + 0] - 128) / 128.0f;
        outL[i] = l * gain;
        if (outR && channels > 1) {
          float r = ((int)p[idx + 1] - 128) / 128.0f;
          outR[i] = r * gain;
        }
      }
    } else { // 16-bit little-endian
      printf("  (16-bit samples)\n");
      const int16_t* p = (const int16_t*)bytes;
      for (AVAudioFrameCount i = 0; i < frames; i++) {
        const int idx = (int)i * channels;
        float l = p[idx + 0] / 32768.0f;
        outL[i] = l * gain;
        if (outR && channels > 1) {
          float r = p[idx + 1] / 32768.0f;
          outR[i] = r * gain;
        }
      }
    }

    printf("  scheduling %u frames...\n", (unsigned)buf.frameLength);
    if (![obj.node isPlaying]) [obj.node play];
    [obj.node scheduleBuffer:buf completionHandler:nil];
    rc = 0;
  });
  return rc;
}

@interface _AROut : NSObject <SCStreamOutput>
@property(atomic, assign) double band0; // bass
@property(atomic, assign) double band1; // mids
@property(atomic, assign) double band2; // highs
@end

@implementation _AROut
- (void)stream:(SCStream *)s didOutputSampleBuffer:(CMSampleBufferRef)sb ofType:(SCStreamOutputType)t {
  if (t != SCStreamOutputTypeAudio || !sb) return;

  CMFormatDescriptionRef fmt = CMSampleBufferGetFormatDescription(sb);
  if (!fmt) return;

  const AudioStreamBasicDescription *asbd =
    CMAudioFormatDescriptionGetStreamBasicDescription(fmt);
  if (!asbd) return;

  const double sr = asbd->mSampleRate;
  const UInt32 ch = asbd->mChannelsPerFrame;
  if (sr <= 0 || ch == 0) return;

  // --- Fetch AudioBufferList safely (size query, then allocate) ---
  AudioBufferList *abl = NULL;
  CMBlockBufferRef blockBuf = NULL;
  size_t ablSize = 0;

  OSStatus os = CMSampleBufferGetAudioBufferListWithRetainedBlockBuffer(
    sb, &ablSize, NULL, 0,
    kCFAllocatorDefault, kCFAllocatorDefault,
    kCMSampleBufferFlag_AudioBufferList_Assure16ByteAlignment,
    &blockBuf);
  if (os != noErr || ablSize == 0) { if (blockBuf) CFRelease(blockBuf); return; }

  abl = (AudioBufferList *)malloc(ablSize);
  if (!abl) { if (blockBuf) CFRelease(blockBuf); return; }

  os = CMSampleBufferGetAudioBufferListWithRetainedBlockBuffer(
    sb, NULL, abl, ablSize,
    kCFAllocatorDefault, kCFAllocatorDefault,
    kCMSampleBufferFlag_AudioBufferList_Assure16ByteAlignment,
    &blockBuf);
  if (os != noErr || abl->mNumberBuffers == 0) {
    if (blockBuf) CFRelease(blockBuf);
    free(abl);
    return;
  }

  // --- Determine frame count robustly ---
  const UInt32 bytesPerSample = (asbd->mBitsPerChannel + 7) / 8;
  if (bytesPerSample == 0) { CFRelease(blockBuf); free(abl); return; }

  UInt32 frames = 0;
  const BOOL nonInterleaved = (asbd->mFormatFlags & kAudioFormatFlagIsNonInterleaved) != 0;
  if (nonInterleaved) {
    frames = abl->mBuffers[0].mDataByteSize / bytesPerSample; // 1 ch per buffer
  } else {
    UInt32 bytesPerFrame = (UInt32)asbd->mBytesPerFrame;
    if (bytesPerFrame == 0) bytesPerFrame = bytesPerSample * ch;
    frames = abl->mBuffers[0].mDataByteSize / bytesPerFrame;
  }
  if (frames == 0) { CFRelease(blockBuf); free(abl); return; }

  // --- Mix down to mono float ---
  NSMutableData *monoF = [NSMutableData dataWithLength:(size_t)frames * sizeof(float)];
  float *mf = (float *)monoF.mutableBytes;
  if (!mf) { CFRelease(blockBuf); free(abl); return; }

  const BOOL isFloat = (asbd->mFormatFlags & kAudioFormatFlagIsFloat) != 0;

  if (isFloat) {
    if (nonInterleaved) {
      memset(mf, 0, frames * sizeof(float));
      for (UInt32 b = 0; b < abl->mNumberBuffers; b++) {
        const float *buf = (const float *)abl->mBuffers[b].mData;
        if (!buf) continue;
        vDSP_vadd(mf, 1, buf, 1, mf, 1, frames);
      }
      float denom = (float)MAX(1, abl->mNumberBuffers);
      vDSP_vsdiv(mf, 1, &denom, mf, 1, frames);
    } else {
      const float *in = (const float *)abl->mBuffers[0].mData;
      if (!in) { CFRelease(blockBuf); free(abl); return; }
      for (UInt32 i = 0; i < frames; i++) {
        float acc = 0.f;
        for (UInt32 c = 0; c < ch; c++) acc += in[i * ch + c];
        mf[i] = acc / (float)ch;
      }
    }
  } else {
    // assume 16-bit signed
    if (nonInterleaved) {
      memset(mf, 0, frames * sizeof(float));
      for (UInt32 b = 0; b < abl->mNumberBuffers; b++) {
        const int16_t *buf = (const int16_t *)abl->mBuffers[b].mData;
        if (!buf) continue;
        for (UInt32 i = 0; i < frames; i++) mf[i] += (float)buf[i] / 32768.f;
      }
      float denom = (float)MAX(1, abl->mNumberBuffers);
      vDSP_vsdiv(mf, 1, &denom, mf, 1, frames);
    } else {
      const int16_t *in = (const int16_t *)abl->mBuffers[0].mData;
      if (!in) { CFRelease(blockBuf); free(abl); return; }
      for (UInt32 i = 0; i < frames; i++) {
        int acc = 0;
        for (UInt32 c = 0; c < ch; c++) acc += in[i * ch + c];
        mf[i] = ((float)acc / (float)ch) / 32768.f;
      }
    }
  }

  CFRelease(blockBuf);
  free(abl);

  // --- FFT (real → complex), Hann window, power→dB ---
  int n = 1; while (n < (int)frames) n <<= 1;
  if (n > 2048) n = 2048;
  if (n < 16) return;

  DSPSplitComplex sc = {0};
  sc.realp = (float *)malloc(sizeof(float) * (size_t)(n / 2));
  sc.imagp = (float *)malloc(sizeof(float) * (size_t)(n / 2));
  if (!sc.realp || !sc.imagp) { free(sc.realp); free(sc.imagp); return; }

  vDSP_DFT_Setup setup = vDSP_DFT_zrop_CreateSetup(NULL, (vDSP_Length)n, vDSP_DFT_FORWARD);
  if (!setup) { free(sc.realp); free(sc.imagp); return; }

  float *win   = (float *)malloc(sizeof(float) * (size_t)n);
  float *temp  = (float *)malloc(sizeof(float) * (size_t)n);
  float *zeros = (float *)calloc((size_t)n, sizeof(float));
  if (!win || !temp || !zeros) {
    free(win); free(temp); free(zeros);
    vDSP_DFT_DestroySetup(setup);
    free(sc.realp); free(sc.imagp);
    return;
  }

  const int copyN = MIN(n, (int)frames);
  memcpy(temp, mf, sizeof(float) * (size_t)copyN);
  if (copyN < n) memset(temp + copyN, 0, sizeof(float) * (size_t)(n - copyN));

  // Hann window (normalized) and its mean-square for power normalization
  vDSP_hann_window(win, (vDSP_Length)n, vDSP_HANN_NORM);
  vDSP_vmul(temp, 1, win, 1, temp, 1, (vDSP_Length)n);

  float winMS = 0.f; // mean-square of window
  {
    float sumsq = 0.f;
    vDSP_svesq(win, 1, &sumsq, (vDSP_Length)n);   // sum of w^2
    winMS = sumsq / (float)n;
    if (winMS <= 0.f) winMS = 1.f;                // safety
  }

  vDSP_DFT_Execute(setup, temp, zeros, sc.realp, sc.imagp);

  // Magnitude spectrum
  float *mag = (float *)malloc(sizeof(float) * (size_t)(n / 2));
  if (!mag) {
    free(win); free(temp); free(zeros);
    vDSP_DFT_DestroySetup(setup);
    free(sc.realp); free(sc.imagp);
    return;
  }
  vDSP_zvabs(&sc, 1, mag, 1, (vDSP_Length)(n / 2));

  // Helper: average POWER in [lo,hi] Hz → dB
  // We normalize by bin count, N (roughly), and Hann mean-square to stabilize across N.
  float (^bandDb)(float,float) = ^float(float lo, float hi){
    int i0 = (int)floorf(lo * (float)n / (float)sr);
    int i1 = (int)ceilf (hi * (float)n / (float)sr);
    if (i0 < 1) i0 = 1;                     // skip DC
    if (i1 <= i0) i1 = i0 + 1;
    if (i1 > n/2) i1 = n/2;
    const int count = i1 - i0;

    float sumsq = 0.f;
    vDSP_svesq(mag + i0, 1, &sumsq, (vDSP_Length)count);   // Σ |X[k]|^2

    // Rough power normalization: average per bin, compensate Hann energy
    float pwr = (sumsq / (float)count);
    pwr /= (float)n;            // scale down with FFT length
    pwr /= winMS;               // remove window energy bias

    const float eps = 1e-12f;
    return 10.f * log10f(pwr + eps);
  };

  // Bands in dB
  double b = bandDb(  20.f,   250.f);
  double m = bandDb( 250.f,  2000.f);
  double h = bandDb(2000.f,  8000.f);

  self.band0 = isfinite(b) ? b : 0.0;
  self.band1 = isfinite(m) ? m : 0.0;
  self.band2 = isfinite(h) ? h : 0.0;

  // Cleanup
  free(mag);
  vDSP_DFT_DestroySetup(setup);
  free(zeros);
  free(temp);
  free(win);
  free(sc.realp);
  free(sc.imagp);
}
@end

struct ARHandle {
  SCStream* stream;
  _AROut*   out;
};

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

ARHandle* ar_create(void){ return (ARHandle*)calloc(1,sizeof(ARHandle)); }

void ar_destroy(ARHandle* h){
  if (!h) return;
  dispatch_sync(dispatch_get_main_queue(), ^{
    if (h->stream) { [h->stream stopCaptureWithCompletionHandler:^(__unused NSError* e){}]; h->stream=nil; }
    h->out = nil;
  });
  free(h);
}

int ar_start(ARHandle* h, unsigned long long windowID) {
  if (!h) return -1;
  __block int rc = 0;
  onMainSync(^{
    NSError* err = nil;
    SCShareableContent* content = fetchContent(&err);
    if (!content) { rc = -2; return; }

    SCContentFilter* filter = nil;

    if (windowID == 0) {
      // System/display capture: pick the primary display
      SCDisplay* disp = content.displays.firstObject;
      if (!disp) { rc = -3; return; }
      // capture entire display (video optional), audio on
      if (@available(macOS 14.0, *)) {
        filter = [[SCContentFilter alloc] initWithDisplay:disp excludingWindows:@[]];
      } else {
        filter = [[SCContentFilter alloc] initWithDisplay:disp excludingWindows:@[]];
      }
    } else {
      // Window capture
      SCWindow* target = nil;
      for (SCWindow* w in content.windows)
        if ((uint64_t)w.windowID == windowID) { target = w; break; }
      if (!target) { rc = -3; return; }
      filter = [[SCContentFilter alloc] initWithDesktopIndependentWindow:target];
    }

    SCStreamConfiguration* cfg = [SCStreamConfiguration new];
    cfg.capturesAudio = YES;
    cfg.showsCursor  = NO;      // irrelevant for audio but fine

    _AROut* out = [_AROut new];
    h->out = out;               // <- ensure out is set before add

    h->stream = [[SCStream alloc] initWithFilter:filter configuration:cfg delegate:nil];
    if (!h->stream) { h->out = nil; rc = -4; return; }

    dispatch_queue_t q = dispatch_queue_create("rb.sc.audio", DISPATCH_QUEUE_SERIAL);
    if (![h->stream addStreamOutput:out type:SCStreamOutputTypeAudio sampleHandlerQueue:q error:&err]) {
      h->stream = nil; h->out = nil; rc = -5; return;
    }

    [h->stream startCaptureWithCompletionHandler:^(__unused NSError* e){}];
  });
  return rc;
}

void ar_stop(ARHandle* h) {
  if (!h) return;
  onMainSync(^{
    if (h->stream) { [h->stream stopCaptureWithCompletionHandler:^(__unused NSError* e){}]; h->stream=nil; }
    h->out=nil;
  });
}

int ar_current_fft(ARHandle* h, double out3[3]) {
  if (!h || !h->out || !out3) return 0;
  out3[0] = h->out.band0;
  out3[1] = h->out.band1;
  out3[2] = h->out.band2;
  return 1;
}
