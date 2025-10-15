#ifndef SCSHIM_H
#define SCSHIM_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef void (*WindowCB)(uint64_t wid, const char *title, void *user);
typedef void (*FrameCB)(const uint8_t *data, int w, int h, int stride,
                        void *user);
typedef void (*StopCB)(void *user);

int sc_preflight_screen(void);                   // 1 if allowed, 0 if not
int sc_request_screen(void);                     // blocks, 1 if granted
int sc_tcc_reset(const char *bundle_id_or_null); // 0 ok, else nonzero

// Enumerate capturable windows
void sc_list_windows(WindowCB cb, void *user);

typedef void (*PickCB)(uint64_t wid, const char *title, void *user);
int sc_pick_window(PickCB cb, void *user);

// Start capture for a window id. Returns 0 on success.
int sc_start_window(uint64_t wid, FrameCB fcb, StopCB on_stop, void *user);

// Stop current capture
void sc_stop(void);

typedef struct {
  int x;
  int y;
  int w;
  int h;
} SCRect;

// Returns 0 on success. x,y are in *top-left* origin pixel coords, w,h in
// pixels.
int sc_get_window_rect(uint64_t wid, SCRect *out);

#ifdef __cplusplus
}
#endif

#endif // SCSHIM_H
