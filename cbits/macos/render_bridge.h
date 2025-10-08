#include <stdint.h>
#ifdef __cplusplus
extern "C" {
#endif
typedef struct RB RB;

// Frame kinds.
enum { RB_BYTES = 0, RB_IOSURFACE = 1, RB_D3D11 = 2, RB_DMABUF = 3 };

// Descriptor for “latest frame”.
typedef struct {
  int kind; // RB_BYTES / RB_IOSURFACE / …
  int width, height;
  int stride;          // bytes/row if BYTES, else 0
  const void *payload; // BYTES: uint8_t*; IOSURFACE: IOSurfaceRef; D3D11:
                       // ID3D11Texture2D*; DMABUF: opaque
  void
      *handle; // platform frame handle to release later (e.g. CVPixelBufferRef)
} RBFrame;

// Lifecycle + capture.
RB *rb_create(void);
int rb_start_capture(RB *rb, uint64_t window_id); // non-blocking start
void rb_stop_capture(RB *rb);
void rb_destroy(RB *rb);

// Per-frame polling:
// returns 1 if a new frame is available and fills out, 0 if none, <0 on error
int rb_poll_frame(RB *rb, RBFrame *out);

// Must be called exactly once after we are done with the frame.
void rb_release_frame(RB *rb, RBFrame *);

// Upload the frame into the CURRENTLY BOUND GL texture efficiently
// and sets needed pixel-store internally. returns 0 on success.
int rb_upload_to_bound_texture(RB *rb, const RBFrame *);

// Set overlay gap/prefs:
// e.g. keep a one pixel gap to avoid competely occluding the captured window.
void rb_set_gap(RB *rb, int px);
#ifdef __cplusplus
}
#endif
