{-# LANGUAGE QuasiQuotes #-}

-- | The Shader module provides low level means to describe a GLSL program. The
-- goal is to have composable shadereffects which can be safely constructed
-- during runtime, compiled and loaded with the desired composition.
module Horture.Shader.Shader
  ( passthroughVertexShader,
    mvpVertexShader,
    aiIsWonderful,
    displayShader,
    imageFragmentShader,
    fontFragmentShader,
    barrelShader,
    stitchShader,
    blurVShader,
    blurHShader,
    cycleColoursShader,
    blinkShader,
    flashbangShader,
    gifFragmentShader,
    gifVertexShader,
    mirrorShader,
    invertShader,
    toonShader,
    backgroundShader,
    audioShader,
    bassRealityWarp,
    kaleidoscopeShader,
  )
where

import Data.ByteString.Char8 (ByteString)
import Text.RawString.QQ

backgroundShader :: ByteString
backgroundShader =
  [r|
#version 410 core

in vec2 texCoord;
uniform float time;                 // set from CPU each frame (seconds)

layout(location = 0) out vec4 frag_colour;

vec4 drawBackground(vec2 uv, float t) {
  // base colour
  vec3 base = vec3(0.2, 0.4, 0.7);

  // distance from centre
  float r = distance(uv, vec2(0.5,0.5));

  // radial pulse oscillates with time
  float pulse = 0.5 + 0.5 * sin(t + r*0.25);

  // vary hue slightly
  vec3 colour = mix(base, vec3(1.0,0.2,0.3), 0.3*pulse);

  return vec4(colour * (1.0 - r) * (0.5 + 0.5*pulse), 1.0);
}

void main()
{
  vec2 uv = vec2(texCoord.x, 1.0 - texCoord.y);
  frag_colour = drawBackground(uv, time);
}
  |]

passthroughVertexShader :: ByteString
passthroughVertexShader =
  [r|
#version 410 core

layout (location = 0) in vec3 aPos;
layout (location = 1) in vec2 aTexCoord;

out vec2 texCoord;

void main() {
  gl_Position = vec4(aPos.x, aPos.y, aPos.z, 1.0);
  texCoord = vec2(aTexCoord.x, aTexCoord.y);
}
  |]

imageFragmentShader :: ByteString
imageFragmentShader =
  [r|
#version 410 core

in vec2 texCoord;

uniform sampler2D imgTexture;

out vec4 frag_colour;

void main() {
  frag_colour = texture(imgTexture, vec2(texCoord.x, texCoord.y));
}
  |]

mvpVertexShader :: ByteString
mvpVertexShader =
  [r|
#version 410 core

layout (location = 0) in vec3 aPos;
layout (location = 1) in vec2 aTexCoord;
uniform mat4 model;
uniform mat4 view;
uniform mat4 proj;
uniform float dt;

out vec2 texCoord;

void main() {
  gl_Position = proj * view * model * vec4(aPos.x, aPos.y, aPos.z, 1.0);
  texCoord = aTexCoord;
}
   |]

displayShader :: ByteString
displayShader =
  [r|
#version 410 core

in vec2 texCoord;

uniform float dt;
uniform sampler2D imgTexture;
uniform vec2 uvInset;

out vec4 frag_colour;

// NOTE: DisplayShader expects RGB colors in RGBA format, thus invalidating any
// input alpha values and replacing them with 1.0. Keeps compatibility high
// between multiple display applications.
void main() {
  vec4 col = texture(imgTexture, texCoord);
  frag_colour = vec4(col.x, col.y, col.z, 1.0);
  // vec2 lo = uvInset;
  // vec2 hi = vec2(1.0) - uvInset;
  // vec2 uv = lo + texCoord * (hi - lo);
  // frag_colour = texture(imgTexture, uv);
}
  |]

gifVertexShader :: ByteString
gifVertexShader =
  [r|
#version 410

layout (location = 0) in vec3 aPos;
layout (location = 1) in vec2 aTexCoord;
uniform float dt;
uniform mat4 model;

out vec2 texCoord;

void main() {
  gl_Position = model * vec4(aPos.x, aPos.y, aPos.z, 1.0);
  texCoord = aTexCoord;
}
   |]

gifFragmentShader :: ByteString
gifFragmentShader =
  [r|
#version 410

in vec2 texCoord;

uniform int index;
uniform sampler2DArray gifTexture;

out vec4 frag_colour;

void main() {
  frag_colour = texture(gifTexture, vec3(texCoord.x, texCoord.y, index));
}
    |]

fontFragmentShader :: ByteString
fontFragmentShader =
  [r|
#version 410

in vec2 texCoord;

uniform sampler2D fontTexture;
uniform float opacity = 1.0;

out vec4 frag_colour;

void main() {
  float color = texture(fontTexture, texCoord).r;
  float alpha = texture(fontTexture, texCoord).g;
  frag_colour = vec4(color, color, color, alpha*opacity);
}
  |]

barrelShader :: ByteString
barrelShader =
  [r|
#version 410

in vec2 texCoord;

uniform sampler2D imgTexture;
uniform float barrelPower = 1.5;
uniform float lifetime = 0;
uniform float dt = 0;
uniform vec3 frequencies = vec3(0, 0, 0);

layout(location = 0) out vec4 frag_colour;

vec4 distort(sampler2D tex, vec2 uv, float lifetime, float dt) {
  vec2 xy = 2.0 * uv - 1.0;
  if (length(xy)>=1.0) return texture(tex, uv);

  float theta = atan(xy.y, xy.x);
  float radius = length(xy);
  radius = pow(radius, barrelPower);
  xy.x = radius * cos(theta);
  xy.y = radius * sin(theta);
  xy = 0.5 * (xy + 1.0);
  vec4 colour = texture(tex, xy);
  return vec4(colour.x, colour.y, colour.z, 1);
}

void main() {
  vec2 uv = vec2(texCoord.x, 1-texCoord.y);
  frag_colour = distort(imgTexture, uv, lifetime, dt);
}
    |]

stitchShader :: ByteString
stitchShader =
  [r|
#version 410

in vec2 texCoord;
uniform sampler2D imgTexture;
uniform float stitchSize = 6.0;
uniform float lifetime = 0;
uniform float dt = 0;
uniform vec3 frequencies = vec3(0, 0, 0);
layout(location = 0) out vec4 frag_colour;

vec4 stitchIt(sampler2D tex, vec2 uv, float lifetime, float dt) {
  // Texture metrics
  vec2 texSize = vec2(textureSize(tex, 0));
  vec2 invTex  = 1.0 / texSize;

  // Guard + subtle temporal wobble to avoid static alias
  float size = max(1.0, stitchSize);
  float j = 0.07 * sin(dt*3.1 + uv.y*50.0) * (0.25 + 0.75*clamp(frequencies.x,0.0,1.0));
  size = max(1.0, size + j);

  // Pixel space
  vec2 px = uv * texSize;

  // Cell origin (top-left) in pixel space
  vec2 cellTL = floor(px / size) * size;

  // Local coord inside the cell [0,size)
  vec2 local = px - cellTL;

  // Sample base colour once per cell (top-left texel of cell)
  vec2 baseUV = (cellTL + 0.5) * invTex;
  vec4 baseCol = texture(tex, baseUV);

  // Thread pattern: two diagonals per cell to mimic cross-stitch
  // Distance to the two diagonals within the cell
  float d1 = abs(local.y - local.x);
  float d2 = abs((size - local.y) - local.x);

  // Thickness scales with size; 1.0 px at size=6
  float thick = clamp(size * (1.0/6.0), 0.75, 1.5);

  // Anti-aliased lines using fwidth
  float aa = fwidth(d1) + 0.0001;
  float l1 = 1.0 - smoothstep(thick - aa, thick + aa, d1);
  aa = fwidth(d2) + 0.0001;
  float l2 = 1.0 - smoothstep(thick - aa, thick + aa, d2);

  // Combine threads; slight priority to the current dominant diagonal
  float bias = step(0.5, fract(dot(frequencies, vec3(0.31,0.47,0.22))*10.0 + floor(cellTL.x+cellTL.y)));
  float thread = mix(l1, l2, bias);

  // Make stitches fade towards cell edges (rounded ends)
  vec2 edge = abs(local - size*0.5) / (size*0.5);
  float cap = smoothstep(1.2, 0.6, max(edge.x, edge.y)); // 0..1 cap mask
  thread *= cap;

  // Thread color slightly tinted from base, with “cotton” highlight
  vec3 threadTint = mix(baseCol.rgb, vec3(0.85, 0.78, 0.65), 0.35);
  // Directional sheen (depends on diagonal chosen)
  vec2 dir = normalize(vec2(1.0, bias>0.5 ? -1.0 : 1.0));
  float sheen = 0.25 + 0.75 * pow(abs(dot(normalize(local - size*0.5), dir)), 8.0);
  vec3 threadCol = clamp(threadTint * (0.85 + 0.15*sheen), 0.0, 1.0);

  // Background dark fabric look from base sample
  vec3 fabric = baseCol.rgb * 0.85;

  // Mix: thread alpha from thread coverage
  vec3 c = mix(fabric, threadCol, clamp(thread, 0.0, 1.0));
  return vec4(c, 1.0);
}

void main() {
  vec2 uv = vec2(texCoord.x, 1.0 - texCoord.y);
  frag_colour = stitchIt(imgTexture, uv, lifetime, 1.0);
}
    |]

aiIsWonderful :: ByteString
aiIsWonderful =
  [r|
#version 410 core

in vec2 texCoord;

uniform sampler2D imgTexture;
uniform float dt;   // seconds
uniform float rng;     // 0..1 per scene or per event

layout(location = 0) out vec4 frag_colour;

// hash without branches
float hash21(vec2 p) {
  p = fract(p*vec2(123.34, 345.45));
  p += dot(p, p+34.345);
  return fract(p.x*p.y);
}

void main() {
  vec2 uv = vec2(texCoord.x, 1-texCoord.y);

  // time knobs
  float t  = dt;
  float rt = t * (1.3 + 2.0*rng);

  // mild wobble (tube-like)
  uv += 0.003 * vec2(
      sin(uv.y*12.0 + rt*1.7),
      sin(uv.x*10.0 - rt*1.2)
  );

  // horizontal scanline darkening
  float scan = 0.06 * sin(uv.y*800.0 + rt*20.0);
  float vign = smoothstep(0.95, 0.2, length(uv-0.5));

  // rare glitch stripes
  float gGate = step(0.985, hash21(vec2(floor(t*8.0), rng)));
  float gBand = smoothstep(0.0, 1.0, abs(sin(uv.y*40.0 + floor(t*8.0)))) * gGate;
  float gShift = 0.006 * gBand;

  // chromatic aberration + glitch shift
  vec2 off = vec2(0.002 + 0.002*sin(rt*0.7), 0.0) + vec2(gShift, 0.0);
  float r = texture(imgTexture, uv + off).r;
  float g = texture(imgTexture, uv).g;
  float b = texture(imgTexture, uv - off).b;

  vec3 col = vec3(r,g,b);

  // film grain
  float grain = hash21(uv*vec2(1920.0,1080.0) + rt) * 0.03;
  col = col * (1.0 - scan) * (1.0 - 0.25*vign) + grain;

  frag_colour = vec4(col, 1.0);
}
    |]

-- | blurVShader splitting the gaussian blurring in two stages according to the
-- properties of separable convolution kernels. I.e. if a filter kernel is
-- separable, its matrix can be expressed as the outer product of two vectors v
-- & w.
-- This results in less operations. First applying v and then applying w, or
-- the other way around since convolution is associative, is more efficient
-- than applying the result of (v * w).
blurVShader :: ByteString
blurVShader =
  [r|
#version 410

in vec2 texCoord;
uniform sampler2D imgTexture;
uniform float lifetime = 0;
uniform float dt = 0;
uniform vec3 frequencies = vec3(0, 0, 0);
layout(location = 0) out vec4 frag_colour;

float offset[3] = float[](0.0, 2.3846153846, 6.2307602308);
float weight[3] = float[](0.2270270270, 0.3162162162, 0.0702702703);

vec4 blurVertical(sampler2D tex, vec2 uv, float lifetime, float dt) {
  vec2 texSize = textureSize(tex, 0);
  float rtH = float(texSize.y);

  vec3 c = texture(tex, uv).rgb * weight[0];
  for (int i=1; i<3; i++) {
    c += texture(tex, uv + vec2(0.0, offset[i])/rtH).rgb * weight[i];
    c += texture(tex, uv - vec2(0.0, offset[i])/rtH).rgb * weight[i];
  }

  return vec4(c, 1.0);
}

void main() {
  vec2 uv = vec2(texCoord.x, 1-texCoord.y);
  frag_colour = blurVertical(imgTexture, uv, lifetime, dt);
}
    |]

-- | blurHShader for horizontally blurring an image. See blurVShader for more
-- info.
blurHShader :: ByteString
blurHShader =
  [r|
#version 410

in vec2 texCoord;
uniform sampler2D imgTexture;
uniform float lifetime = 0;
uniform float dt = 0;
uniform vec3 frequencies = vec3(0, 0, 0);
layout(location = 0) out vec4 frag_colour;

float offset[3] = float[](0.0, 2.3846153846, 6.2307602308);
float weight[3] = float[](0.2270270270, 0.3162162162, 0.0702702703);

vec4 blurHorizontal(sampler2D tex, vec2 uv, float lifetime, float dt) {
  vec2 texSize = textureSize(tex, 0);
  float rtW = float(texSize.x);

  vec3 c = texture(tex, uv).rgb * weight[0];
  for (int i=1; i<3; i++) {
    c += texture(tex, uv + vec2(offset[i], 0.0)/rtW).rgb * weight[i];
    c += texture(tex, uv - vec2(offset[i], 0.0)/rtW).rgb * weight[i];
  }

  return vec4(c, 1.0);
}

void main() {
  vec2 uv = vec2(texCoord.x, 1-texCoord.y);
  frag_colour = blurHorizontal(imgTexture, uv, lifetime, dt);
}
    |]

kaleidoscopeShader :: ByteString
kaleidoscopeShader =
  [r|
    #version 410 core

in vec2 texCoord;

uniform sampler2D imgTexture;
uniform float dt;
uniform float segments = 7.2;    // e.g. 6.0 .. 12.0
uniform float swirl = 0.6;
uniform float aberration = 0.002;

layout(location = 0) out vec4 frag_colour;

float PI = 3.14159265359;

vec2 kaleido(vec2 uv, float seg, float t, float k) {
  // center and go polar
  vec2 p = uv - 0.5;
  float r = length(p);
  float a = atan(p.y, p.x);

  // fold angle into 2*pi/seg wedge and mirror it
  float wedge = 2.0 * PI / seg;
  a = mod(a, wedge);
  a = abs(a - wedge * 0.5);

  // dt-based swirl (stronger near center)
  a += k * (1.0 - smoothstep(0.0, 0.7, r)) * sin(2.0*t + r*14.0);

  // back to cartesian, recentre
  vec2 q = vec2(cos(a), sin(a)) * r + 0.5;
  return q;
}

void main() {
  // base kaleidoscoped coords
  vec2 uv = kaleido(texCoord, max(3.0, segments), dt, swirl);

  // mild breathing zoom to excite motion
  float zoom = 0.02 * sin(dt*0.8);
  uv = (uv - 0.5) * (1.0 - zoom) + 0.5;

  // chromatic aberration: sample channels at tiny radial offsets
  vec2 dir = normalize(uv - 0.5);
  vec3 col;
  col.r = texture(imgTexture, uv + dir * aberration).r;
  col.g = texture(imgTexture, uv).g;
  col.b = texture(imgTexture, uv - dir * aberration).b;

  // soft vignette
  float d = distance(uv, vec2(0.5));
  float vig = smoothstep(0.9, 0.35, d); // falls off toward edges

  // slight contrast pop
  col = mix(col, col*1.15, 0.25);

  frag_colour = vec4(col * vig, 1.0);
}
    |]

-- | flashbangShader simply makes the screen bright and white.
flashbangShader :: ByteString
flashbangShader =
  [r|
#version 410

in vec2 texCoord;
uniform sampler2D imgTexture;
uniform float lifetime = 0;
uniform float dt = 0;
uniform vec3 frequencies = vec3(0, 0, 0);
layout(location = 0) out vec4 frag_colour;

vec4 flashbang(sampler2D tex, vec2 uv, float lifetime, float dt) {
  vec4 c = texture(tex, uv);
  float pp = clamp(1 - (dt / lifetime), 0.0, 1.0);
  return vec4(pp * (1 - c.x) + c.x, pp * (1 - c.y) + c.y, pp * (1 - c.z) + c.z, 1.0);
}

void main() {
  vec2 uv = vec2(texCoord.x, 1-texCoord.y);
  frag_colour = flashbang(imgTexture, uv, lifetime, dt);
}
    |]

-- | cycleColourShader simply cycles through colors using the base texture.
cycleColoursShader :: ByteString
cycleColoursShader =
  [r|
#version 410

in vec2 texCoord;
uniform sampler2D imgTexture;
uniform float lifetime = 0;
uniform float dt = 0;
uniform vec3 frequencies = vec3(0, 0, 0);
layout(location = 0) out vec4 frag_colour;

vec4 cycleColours(sampler2D tex, vec2 uv, float lifetime, float dt) {
  vec4 c = texture(tex, uv);
  float pp = float(dt);
  vec4 update = vec4(abs(cos(pp * c.x)), abs(sin(pp*c.y)), abs(sin(pp*c.z)*cos(pp*c.z)), 1);
  return mix(c, update, clamp(float(dt/(lifetime*0.3)), 0.0, 1.0));
}

void main() {
  vec2 uv = vec2(texCoord.x, 1-texCoord.y);
  frag_colour = cycleColours(imgTexture, uv, lifetime, dt);
}
    |]

-- | blinkShader goes into black and white.
blinkShader :: ByteString
blinkShader =
  [r|
#version 410

in vec2 texCoord;
uniform sampler2D imgTexture;
uniform float lifetime = 0;
uniform float dt = 0;
uniform vec3 frequencies = vec3(0, 0, 0);
layout(location = 0) out vec4 frag_colour;

vec4 blink(sampler2D tex, vec2 uv, float lifetime, float dt) {
  vec4 c = texture(tex, uv);
  float pp = float(1 - (dt / lifetime));
  return vec4(c.x - (pp * c.x), c.y - (pp * c.y), c.z - (pp * c.z), 1);
}

void main() {
  vec2 uv = vec2(texCoord.x, 1-texCoord.y);
  frag_colour = blink(imgTexture, uv, lifetime, dt);
}
    |]

-- | mirrorShader goes into black and white.
mirrorShader :: ByteString
mirrorShader =
  [r|
#version 410

in vec2 texCoord;
uniform sampler2D imgTexture;
uniform float lifetime = 0;
uniform float dt = 0;
uniform vec3 frequencies = vec3(0, 0, 0);
layout(location = 0) out vec4 frag_colour;

vec4 mirror(sampler2D tex, vec2 uv, float lifetime, float dt) {
  return texture(tex, vec2(1.0 - uv.x, uv.y));
}

void main() {
  vec2 uv = vec2(texCoord.x, 1-texCoord.y);
  frag_colour = mirror(imgTexture, uv, lifetime, dt);
}
    |]

invertShader :: ByteString
invertShader =
  [r|
#version 410

in vec2 texCoord;
uniform sampler2D imgTexture;
uniform float lifetime = 0;
uniform float dt = 0;
uniform vec3 frequencies = vec3(0, 0, 0);
layout(location = 0) out vec4 frag_colour;

vec4 invert(sampler2D tex, vec2 uv, float lifetime, float dt) {
  vec4 c = texture(tex, uv);
  return vec4(1 - c.x, 1 - c.y, 1 - c.z, 1);
}

void main() {
  vec2 uv = vec2(texCoord.x, 1-texCoord.y);
  frag_colour = invert(imgTexture, uv, lifetime, dt);
}
    |]

toonShader :: ByteString
toonShader =
  [r|
#version 410

in vec2 texCoord;
uniform sampler2D imgTexture;
uniform float lifetime = 0;
uniform float dt = 0;
uniform vec3 frequencies = vec3(0, 0, 0);
layout(location = 0) out vec4 frag_colour;
float levels = 8.0 - 1.0;
float contrast = 1.2;
float brightness = 1.1;

vec3 hsl2rgb(vec3 hsl) {
    float t = hsl.y * ((hsl.z < 0.5) ? hsl.z : (1.0 - hsl.z));
    vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
    vec3 p = abs(fract(hsl.xxx + K.xyz) * 6.0 - K.www);
    return (hsl.z + t) * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), 2.0*t / hsl.z);
}

vec4 toonify(sampler2D tex, vec2 uv, float lifetime, float dt) {
  float colorFactor = 1.0;
  vec4 color = texture(tex, uv);
  // BT.709 coefficients related to human perception.
  float grey = 0.21 * color.r + 0.71 * color.g + 0.07 * color.b;
  grey = clamp(grey * brightness, 0.0, 1.0);
  float posterized = round(grey * levels) / levels;
  float contrasted = clamp(contrast * (posterized - 0.5) + 0.5, 0.0, 1.0);
  vec3 rgb = hsl2rgb(vec3(0.153, 1.0, contrasted));
  return vec4(rgb, 1.0);
}

void main() {
  vec2 uv = vec2(texCoord.x, 1-texCoord.y);
  frag_colour = toonify(imgTexture, uv, lifetime, dt);
}
    |]

audioShader :: ByteString
audioShader =
  [r|
#version 410
in vec2 texCoord;
uniform sampler2D imgTexture;
uniform float lifetime = 0.0;
uniform float dt = 0.0;
uniform float frequencies[3] = float[3](0.0, 0.0, 0.0); // bass, mid, high in 0..1
layout(location = 0) out vec4 frag_colour;

void main() {
  vec2 uv = vec2(texCoord.x, 1.0 - texCoord.y);
  vec2 C  = vec2(0.5);
  vec2 v  = uv - C;
  float rN = length(v) / 0.70710678;                 // 0..1 radius
  vec2  dir = (rN > 0.0) ? normalize(v) : vec2(1.0,0.0);

  // bands already normalized 0..1
  float fb = clamp(frequencies[0], 0.0, 1.0);
  float fm = clamp(frequencies[1], 0.0, 1.0);
  float fh = clamp(frequencies[2], 0.0, 1.0);

  // Edge-driven vignette; stronger bass pulls inward
  float reach    = mix(0.92, 0.55, fb);
  float vignette = smoothstep(reach, 1.0, rN);

  // Subtle mids distortion near edges
  float waves   = 28.0;
  float distAmp = 0.0006; // gentle
  vec2 uvD = uv + vignette * fm * distAmp *
             vec2(sin((uv.y + dt * 1.4) * waves),
                  cos((uv.x + dt * 1.2) * waves));

  vec4 base = texture(imgTexture, uvD);

  // Bass: warm orange overlay from edges inward
  vec3 orange  = vec3(1.0, 0.58, 0.20);
  float bassAmt = vignette * fb;
  vec3 bassMix  = mix(base.rgb, mix(base.rgb, orange, 0.6), bassAmt);

  // Highs: slight CA + lift (edge-weighted)
  float ca = 0.0015 * vignette * fh;
  float rS = texture(imgTexture, uvD + dir * ca).r;
  float gS = texture(imgTexture, uvD).g;
  float bS = texture(imgTexture, uvD - dir * ca).b;
  vec3  caRGB = vec3(rS, gS, bS);

  vec3 highsLift = vec3(0.10, 0.09, 0.12) * (vignette * fh);
  vec3 highsMix  = mix(bassMix, caRGB, 0.25 * vignette * fh) + highsLift;

  frag_colour = vec4(highsMix, 1.0);
}
  |]

bassRealityWarp :: ByteString
bassRealityWarp =
  [r|
#version 410
in vec2 texCoord;
uniform sampler2D imgTexture;
uniform float lifetime = 0;
uniform float dt = 0;
uniform float frequencies[3] = float[3](0, 0, 0); // bass, mid, high in 0..1
uniform float rng;
layout(location = 0) out vec4 frag_colour;

vec4 applyVisualization(sampler2D tex, vec2 uv, float lifetime, float dt, float freq[3]) {
  float fb = clamp(freq[0], 0.0, 1.0);     // normalized bass

  float factor = 0.0015 * fb;              // displacement scale
  float speed  = mix(0.6, 2.2, fb);        // mild speed-up with bass

  vec2 tuv1 = uv + vec2( factor * sin(speed * dt * rng),
                         factor * cos(speed * dt * rng * 2.0));
  vec2 tuv2 = uv + vec2( factor * cos(speed * dt * rng),
                         factor * sin(speed * dt * rng * 2.0));

  vec4 orgba = texture(tex, uv);
  vec4 rgba1 = texture(tex, tuv1);
  vec4 rgba2 = texture(tex, tuv2);

  return mix(mix(orgba, rgba1, fb * 0.9), rgba2, fb * 0.3);
}

void main() {
  vec2 uv = vec2(texCoord.x, 1.0 - texCoord.y);
  frag_colour = applyVisualization(imgTexture, uv, lifetime, dt, frequencies);
}
  |]
