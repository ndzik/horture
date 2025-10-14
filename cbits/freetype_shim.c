#include "freetype_shim.h"
#include <ft2build.h>
#include <stdlib.h>
#include <string.h>
#include FT_FREETYPE_H
#include FT_GLYPH_H

struct FTL_Context {
  FT_Library lib;
};
struct FTL_Face {
  FT_Face face;
};

FTL_Context *ftl_create(void) {
  FTL_Context *ctx = (FTL_Context *)calloc(1, sizeof(FTL_Context));
  if (!ctx)
    return NULL;
  if (FT_Init_FreeType(&ctx->lib) != 0) {
    free(ctx);
    return NULL;
  }
  return ctx;
}

void ftl_destroy(FTL_Context *ctx) {
  if (!ctx)
    return;
  if (ctx->lib)
    FT_Done_FreeType(ctx->lib);
  free(ctx);
}

FTL_Face *ftl_face_open(FTL_Context *ctx, const char *filepath,
                        unsigned pixel_height) {
  if (!ctx || !filepath || !pixel_height)
    return NULL;
  FT_Face f = NULL;
  if (FT_New_Face(ctx->lib, filepath, 0, &f) != 0)
    return NULL;
  // height in pixels; width=0 -> computed from height/aspect
  FT_Set_Pixel_Sizes(f, 0, pixel_height);

  FTL_Face *out = (FTL_Face *)calloc(1, sizeof(FTL_Face));
  if (!out) {
    FT_Done_Face(f);
    return NULL;
  }
  out->face = f;
  return out;
}

void ftl_face_close(FTL_Face *ff) {
  if (!ff)
    return;
  if (ff->face)
    FT_Done_Face(ff->face);
  free(ff);
}

static void *xmalloc(size_t n) {
  void *p = malloc(n);
  return p;
}

int ftl_render_glyph(FTL_Face *ff, uint32_t codepoint, FTL_Glyph *out) {
  if (!ff || !ff->face || !out)
    return 0;

  // Load & render with default (normal) target
  if (FT_Load_Char(ff->face, codepoint, FT_LOAD_RENDER))
    return 0;

  FT_GlyphSlot g = ff->face->glyph;
  FT_Bitmap *bm = &g->bitmap;

  // We only handle 8-bit gray output; other modes (e.g. LCD) are converted.
  if (bm->pixel_mode != FT_PIXEL_MODE_GRAY) {
    // Convert to 8-bit gray via FT_Glyph and FT_Glyph_To_Bitmap
    FT_Glyph glyph;
    if (FT_Get_Glyph(g, &glyph) != 0)
      return 0;
    if (FT_Glyph_To_Bitmap(&glyph, FT_RENDER_MODE_NORMAL, NULL, 1) != 0) {
      FT_Done_Glyph(glyph);
      return 0;
    }
    FT_BitmapGlyph bg = (FT_BitmapGlyph)glyph;
    bm = &bg->bitmap;
  }

  const int w = (int)bm->width;
  const int h = (int)bm->rows;

  out->codepoint = codepoint;
  out->width = w;
  out->height = h;
  out->bearingX = g->bitmap_left;
  out->bearingY = g->bitmap_top;
  out->advance = g->advance.x;
  out->pixels = NULL;

  if (w <= 0 || h <= 0) {
    // Space or empty glyph
    return 1;
  }

  // Normalize to tightly packed top->bottom buffer (w*h), regardless of FT
  // pitch sign
  out->pixels = (uint8_t *)xmalloc((size_t)w * (size_t)h);
  if (!out->pixels)
    return 0;

  const int src_pitch = bm->pitch; // bytes per row; can be negative
  const uint8_t *src0 = (const uint8_t *)bm->buffer;

  for (int row = 0; row < h; ++row) {
    const int src_index =
        (src_pitch >= 0)
            ? row * src_pitch
            : (h - 1 - row) * (-src_pitch); // handle negative pitch (bottom-up)
    const uint8_t *src = src0 + src_index;
    memcpy(out->pixels + (size_t)row * (size_t)w, src, (size_t)w);
  }

  return 1;
}

void ftl_free_glyph(FTL_Glyph *g) {
  if (!g)
    return;
  free(g->pixels);
  g->pixels = NULL;
}
