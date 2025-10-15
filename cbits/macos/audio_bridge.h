// cbits/macos/audio_bridge.h
#pragma once
#ifdef __cplusplus
extern "C" {
#endif

typedef struct APHandle APHandle; // player
typedef struct ARHandle ARHandle; // recorder

APHandle *ap_create(void);
void ap_destroy(APHandle *);
int ap_play_pcm(APHandle *, const void *bytes, int byteCount,
                int channels,      // 1 or 2
                int sampleRate,    // e.g. 44100
                int bitsPerSample, // 8 or 16
                float gain);       // 0..1
void ap_stop_all(APHandle *);

// windowID: same CGWindowID we already use. If 0, default to system audio
// fallback (mic).
ARHandle *ar_create(void);
void ar_destroy(ARHandle *);
int ar_start(ARHandle *, unsigned long long windowID);
void ar_stop(ARHandle *);

// write 3 doubles: bass, mid, high. Returns 1 if a fresh snapshot was
// available, 0 otherwise.
int ar_current_fft(ARHandle *, double out3[3]);

#ifdef __cplusplus
}
#endif
