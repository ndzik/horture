#include "freetype_shim.h"
#include <ft2build.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include FT_FREETYPE_H
#include FT_GLYPH_H
#include FT_STROKER_H

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

static inline int blit_channel(uint8_t *dst, size_t pitchRG, int left, int top,
                               FT_BitmapGlyph bg, const FT_Bitmap *bm, int chan,
                               uint32_t codepoint) {
  const uint8_t *src0 = (const uint8_t *)bm->buffer;
  const int sp = bm->pitch; // may be negative
  const int dx = bg->left - left;
  const int dy = top - bg->top; // dst is top-origin

  // FT grayscale normalization
  const float scale =
      (bm->num_grays > 1) ? 255.0f / (float)(bm->num_grays - 1) : 1.0f;

  int wrote = 0;
  for (int row = 0; row < (int)bm->rows; ++row) {
    const int sindex =
        (sp >= 0) ? (row * sp) : ((int)bm->rows - 1 - row) * (-sp);
    const uint8_t *src = src0 + sindex;

    uint8_t *drow = dst + (size_t)(dy + row) * pitchRG + (size_t)(dx * 2);

    for (int col = 0; col < (int)bm->width; ++col) {
      const uint8_t s = src[col];
      const uint8_t v = (uint8_t)lrintf((float)s * scale); // 0..255

      if (chan == 0) { // R = fill
        drow[col * 2] = v;
      } else { // G = outline
        uint8_t *g = &drow[col * 2 + 1];
        if (v > *g) {
          *g = v;
          wrote++;
        }
      }
    }
  }

  return 0;
}

int ftl_render_glyph(FTL_Face *ff, uint32_t codepoint, int outline_px,
                     FTL_Glyph *out) {
  if (!ff || !ff->face || !out)
    return 0;
  FT_Face face = ff->face;

  // Load as outline (no render yet)
  if (FT_Load_Char(face, codepoint, FT_LOAD_DEFAULT))
    return 0;

  FT_Glyph base = NULL;
  if (FT_Get_Glyph(face->glyph, &base))
    return 0;

  // Create stroked glyph
  FT_Stroker stroker = NULL;
  if (FT_Stroker_New(face->glyph->library, &stroker)) {
    FT_Done_Glyph(base);
    return 0;
  }
  FT_Stroker_Set(stroker, (FT_Fixed)(outline_px * 64), FT_STROKER_LINECAP_ROUND,
                 FT_STROKER_LINEJOIN_ROUND, 0);

  FT_Glyph stroke = base; // will be replaced
  if (FT_Glyph_Copy(base, &stroke)) {
    FT_Stroker_Done(stroker);
    FT_Done_Glyph(base);
    return 0;
  }
  // outside stroke (border = 1)
  if (FT_Glyph_StrokeBorder(&stroke, stroker, 0, 0)) {
    FT_Stroker_Done(stroker);
    FT_Done_Glyph(stroke);
    FT_Done_Glyph(base);
    return 0;
  }
  FT_Stroker_Done(stroker);

  // Render both to 8-bit grayscale bitmaps
  if (FT_Glyph_To_Bitmap(&base, FT_RENDER_MODE_NORMAL, NULL, 1) ||
      FT_Glyph_To_Bitmap(&stroke, FT_RENDER_MODE_NORMAL, NULL, 1)) {
    FT_Done_Glyph(stroke);
    FT_Done_Glyph(base);
    return 0;
  }

  FT_BitmapGlyph fbg = (FT_BitmapGlyph)base;   // fill
  FT_BitmapGlyph sbg = (FT_BitmapGlyph)stroke; // outline
  FT_Bitmap *fbm = &fbg->bitmap;
  FT_Bitmap *sbm = &sbg->bitmap;

  // Sanity: convert non-GRAY formats if needed (rare here).
  if (fbm->pixel_mode != FT_PIXEL_MODE_GRAY ||
      sbm->pixel_mode != FT_PIXEL_MODE_GRAY) {
    FT_Done_Glyph(stroke);
    FT_Done_Glyph(base);
    return 0;
  }

  // Compute union box in glyph space (y-up),
  // left/top come from BitmapGlyph’s bearing for each.
  int left = (fbg->left < sbg->left) ? fbg->left : sbg->left;
  int right = ((fbg->left + (int)fbm->width) > (sbg->left + (int)sbm->width))
                  ? (fbg->left + (int)fbm->width)
                  : (sbg->left + (int)sbm->width);
  int top = (fbg->top > sbg->top) ? fbg->top : sbg->top;
  int bottom = ((fbg->top - (int)fbm->rows) < (sbg->top - (int)sbm->rows))
                   ? (fbg->top - (int)fbm->rows)
                   : (sbg->top - (int)sbm->rows);

  int W = right - left;
  int H = top - bottom;

  out->codepoint = codepoint;
  out->width = W;
  out->height = H;
  out->bearingX = left; // union bearing (x from origin to left edge)
  out->bearingY = top;  // union top
  out->advance = face->glyph->advance.x; // 26.6 fixed
  out->pixels = NULL;

  if (W <= 0 || H <= 0) {
    // Space or empty glyph
    FT_Done_Glyph(stroke);
    FT_Done_Glyph(base);
    return 1;
  }

  // Allocate RG buffer (tight, top-to-bottom)
  size_t pitchRG = (size_t)W * 2;
  size_t bytes = (size_t)H * pitchRG;
  uint8_t *dst = (uint8_t *)xmalloc(bytes);
  if (!dst) {
    FT_Done_Glyph(stroke);
    FT_Done_Glyph(base);
    return 0;
  }
  memset(dst, 0, bytes);

  int wrote_fill =
      blit_channel(dst, pitchRG, left, top, fbg, fbm, 0, codepoint);
  int wrote_outline =
      blit_channel(dst, pitchRG, left, top, sbg, sbm, 1, codepoint);

  out->pixels = dst;

  FT_Done_Glyph(stroke);
  FT_Done_Glyph(base);
  return 1;
}

void ftl_free_glyph(FTL_Glyph *g) {
  if (!g)
    return;
  free(g->pixels);
  g->pixels = NULL;
}
