// ftl.h - tiny FreeType loader shim (C99)
// link with: -lfreetype
#pragma once
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct FTL_Context FTL_Context;
typedef struct FTL_Face FTL_Face;

// Tightly packed 8-bit coverage bitmap and metrics (all in pixels).
typedef struct {
  uint8_t *pixels; // size = width * height (caller frees via ftl_free_glyph)
  int width;       // bitmap width  (>=0)
  int height;      // bitmap height (>=0)
  int bearingX;    // left bearing  (pixels, can be negative)
  int bearingY;    // top bearing   (pixels)
  int advance;     // horizontal advance (pixels)
  uint32_t codepoint;
} FTL_Glyph;

// Lifetime
FTL_Context *ftl_create(void);
void ftl_destroy(FTL_Context *ctx);

// Open/close a face at the desired pixel height (width=0 -> auto).
FTL_Face *ftl_face_open(FTL_Context *ctx, const char *filepath,
                        unsigned pixel_height);
void ftl_face_close(FTL_Face *face);

// Render a Unicode codepoint to an 8-bit bitmap + metrics.
// Returns 1 on success, 0 on failure (glyph missing etc).
int ftl_render_glyph(FTL_Face *face, uint32_t codepoint, int outline_px,
                     FTL_Glyph *out);

// Free out->pixels allocated by ftl_render_glyph.
void ftl_free_glyph(FTL_Glyph *g);

#ifdef __cplusplus
}
#endif
