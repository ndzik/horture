{-# LANGUAGE QuasiQuotes #-}

-- | The Shader module provides low level means to describe a GLSL program. The
-- goal is to have composable shadereffects which can be safely constructed
-- during runtime, compiled and loaded with the desired composition.
module Horture.Shader.Shader
  ( passthroughVertexShader,
    mvpVertexShader,
    displayShader,
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
    juliaFractalShader,
    audioShader,
  )
where

import Data.ByteString.Char8 (ByteString)
import Text.RawString.QQ

passthroughVertexShader :: ByteString
passthroughVertexShader =
  [r|
#version 410

layout (location = 0) in vec3 aPos;
layout (location = 1) in vec2 aTexCoord;

out vec2 texCoord;

void main() {
  gl_Position = vec4(aPos.x, aPos.y, aPos.z, 1.0);
  texCoord = vec2(aTexCoord.x, aTexCoord.y);
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

mvpVertexShader :: ByteString
mvpVertexShader =
  [r|
#version 410

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
#version 410

in vec2 texCoord;

uniform float dt;
uniform sampler2D texture1;

out vec4 frag_colour;

// NOTE: DisplayShader expects RGB colors in RGBA format, thus invalidating any
// input alpha values and replacing them with 1.0. Keeps compatibility high
// between multiple display applications.
void main() {
  vec4 col = texture2D(texture1, texCoord);
  frag_colour = vec4(col.x, col.y, col.z, 1.0);
}
  |]

barrelShader :: ByteString
barrelShader =
  [r|
#version 410

in vec2 texCoord;

uniform sampler2D texture1;
uniform float barrelPower = 1.5;
uniform double lifetime = 0;
uniform double dt = 0;
uniform vec3 frequencies = {0, 0, 0};

layout(location = 0) out vec4 frag_colour;

vec4 distort(sampler2D tex, vec2 uv, double lifetime, double dt) {
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
  frag_colour = distort(texture1, uv, lifetime, dt);
}
    |]

stitchShader :: ByteString
stitchShader =
  [r|
#version 410

in vec2 texCoord;
uniform sampler2D texture1;
uniform float stitchSize = 6.0;
uniform double lifetime = 0;
uniform double dt = 0;
uniform vec3 frequencies = {0, 0, 0};
layout(location = 0) out vec4 frag_colour;

vec4 stitchIt(sampler2D tex, vec2 uv, double lifetime, double dt) {
  vec2 texSize = textureSize(tex, 0);
  float rtW = float(texSize.x);
  float rtH = float(texSize.y);
  vec4 c = vec4(0, 0, 0, 1);
  float size = stitchSize;
  vec2 cpos = uv * vec2(rtW, rtH);
  vec2 tlpos = floor(cpos / vec2(size, size));
  tlpos *= size;
  int remx = int(mod(cpos.x, size));
  int remy = int(mod(cpos.y, size));
  if (remx == 0 && remy == 0) tlpos = cpos;
  vec2 blpos = tlpos;
  blpos.y += (size - 1.0);
  if ((remx == remy) || (((int(cpos.x) - int(blpos.x)) == (int(blpos.y) - int(cpos.y))))) {
    c = texture2D(tex, tlpos * vec2(1.0/rtW, 1.0/rtH)) * 1.4;
  } else {
    c = vec4(0.2, 0.15, 0.05, 1.0);
  }
  return c;
}

void main() {
  vec2 uv = vec2(texCoord.x, 1-texCoord.y);
  frag_colour = stitchIt(texture1, uv, lifetime, dt);
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
uniform sampler2D texture1;
uniform double lifetime = 0;
uniform double dt = 0;
uniform vec3 frequencies = {0, 0, 0};
layout(location = 0) out vec4 frag_colour;

float offset[3] = float[](0.0, 2.3846153846, 6.2307602308);
float weight[3] = float[](0.2270270270, 0.3162162162, 0.0702702703);

vec4 blurVertical(sampler2D tex, vec2 uv, double lifetime, double dt) {
  vec2 texSize = textureSize(tex, 0);
  float rtH = float(texSize.y);

  vec3 c = texture2D(tex, uv).rgb * weight[0];
  for (int i=1; i<3; i++) {
    c += texture2D(tex, uv + vec2(0.0, offset[i])/rtH).rgb * weight[i];
    c += texture2D(tex, uv - vec2(0.0, offset[i])/rtH).rgb * weight[i];
  }

  return vec4(c, 1.0);
}

void main() {
  vec2 uv = vec2(texCoord.x, 1-texCoord.y);
  frag_colour = blurVertical(texture1, uv, lifetime, dt);
}
    |]

-- | blurHShader for horizontally blurring an image. See blurVShader for more
-- info.
blurHShader :: ByteString
blurHShader =
  [r|
#version 410

in vec2 texCoord;
uniform sampler2D texture1;
uniform double lifetime = 0;
uniform double dt = 0;
uniform vec3 frequencies = {0, 0, 0};
layout(location = 0) out vec4 frag_colour;

float offset[3] = float[](0.0, 2.3846153846, 6.2307602308);
float weight[3] = float[](0.2270270270, 0.3162162162, 0.0702702703);

vec4 blurHorizontal(sampler2D tex, vec2 uv, double lifetime, double dt) {
  vec2 texSize = textureSize(tex, 0);
  float rtW = float(texSize.x);

  vec3 c = texture2D(tex, uv).rgb * weight[0];
  for (int i=1; i<3; i++) {
    c += texture2D(tex, uv + vec2(0.0, offset[i])/rtW).rgb * weight[i];
    c += texture2D(tex, uv - vec2(0.0, offset[i])/rtW).rgb * weight[i];
  }

  return vec4(c, 1.0);
}

void main() {
  vec2 uv = vec2(texCoord.x, 1-texCoord.y);
  frag_colour = blurHorizontal(texture1, uv, lifetime, dt);
}
    |]

-- | flashbangShader simply makes the screen bright and white.
flashbangShader :: ByteString
flashbangShader =
  [r|
#version 410

in vec2 texCoord;
uniform sampler2D texture1;
uniform double lifetime = 0;
uniform double dt = 0;
uniform vec3 frequencies = {0, 0, 0};
layout(location = 0) out vec4 frag_colour;

vec4 flashbang(sampler2D tex, vec2 uv, double lifetime, double dt) {
  vec4 c = texture2D(tex, uv);
  double pp = 1 - (dt / lifetime);
  return vec4(pp * (1 - c.x) + c.x, pp * (1 - c.y) + c.y, pp * (1 - c.z) + c.z, pp);
}

void main() {
  vec2 uv = vec2(texCoord.x, 1-texCoord.y);
  frag_colour = flashbang(texture1, uv, lifetime, dt);
}
    |]

-- | cycleColourShader simply cycles through colors using the base texture.
cycleColoursShader :: ByteString
cycleColoursShader =
  [r|
#version 410

in vec2 texCoord;
uniform sampler2D texture1;
uniform double lifetime = 0;
uniform double dt = 0;
uniform vec3 frequencies = {0, 0, 0};
layout(location = 0) out vec4 frag_colour;

vec4 cycleColours(sampler2D tex, vec2 uv, double lifetime, double dt) {
  vec4 c = texture2D(tex, uv);
  float pp = float(dt);
  return vec4(abs(cos(pp * c.x)), abs(sin(pp*c.y)), abs(sin(pp*c.z)*cos(pp*c.z)), 1);
}

void main() {
  vec2 uv = vec2(texCoord.x, 1-texCoord.y);
  frag_colour = cycleColours(texture1, uv, lifetime, dt);
}
    |]

-- | blinkShader goes into black and white.
blinkShader :: ByteString
blinkShader =
  [r|
#version 410

in vec2 texCoord;
uniform sampler2D texture1;
uniform double lifetime = 0;
uniform double dt = 0;
uniform vec3 frequencies = {0, 0, 0};
layout(location = 0) out vec4 frag_colour;

vec4 blink(sampler2D tex, vec2 uv, double lifetime, double dt) {
  vec4 c = texture2D(tex, uv);
  float pp = float(1 - (dt / lifetime));
  return vec4(c.x - (pp * c.x), c.y - (pp * c.y), c.z - (pp * c.z), 1);
}

void main() {
  vec2 uv = vec2(texCoord.x, 1-texCoord.y);
  frag_colour = blink(texture1, uv, lifetime, dt);
}
    |]

-- | mirrorShader goes into black and white.
mirrorShader :: ByteString
mirrorShader =
  [r|
#version 410

in vec2 texCoord;
uniform sampler2D texture1;
uniform double lifetime = 0;
uniform double dt = 0;
uniform vec3 frequencies = {0, 0, 0};
layout(location = 0) out vec4 frag_colour;

vec4 mirror(sampler2D tex, vec2 uv, double lifetime, double dt) {
  vec2 texSize = textureSize(tex, 0);
  return texture2D(tex, vec2(texSize.x - uv.x, uv.y));
}

void main() {
  vec2 uv = vec2(texCoord.x, 1-texCoord.y);
  frag_colour = mirror(texture1, uv, lifetime, dt);
}
    |]

invertShader :: ByteString
invertShader =
  [r|
#version 410

in vec2 texCoord;
uniform sampler2D texture1;
uniform double lifetime = 0;
uniform double dt = 0;
uniform vec3 frequencies = {0, 0, 0};
layout(location = 0) out vec4 frag_colour;

vec4 invert(sampler2D tex, vec2 uv, double lifetime, double dt) {
  vec4 c = texture2D(tex, uv);
  return vec4(1 - c.x, 1 - c.y, 1 - c.z, 1);
}

void main() {
  vec2 uv = vec2(texCoord.x, 1-texCoord.y);
  frag_colour = invert(texture1, uv, lifetime, dt);
}
    |]

toonShader :: ByteString
toonShader =
  [r|
#version 410

in vec2 texCoord;
uniform sampler2D texture1;
uniform double lifetime = 0;
uniform double dt = 0;
uniform vec3 frequencies = {0, 0, 0};
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

vec4 toonify(sampler2D tex, vec2 uv, double lifetime, double dt) {
  float colorFactor = 1.0;
  vec4 color = texture2D(tex, uv);
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
  frag_colour = toonify(texture1, uv, lifetime, dt);
}
    |]

audioShader :: ByteString
audioShader =
  [r|
#version 410

in vec2 texCoord;
uniform sampler2D texture1;
uniform double lifetime = 0;
uniform double dt = 0;
uniform double frequencies[3] = {0, 0, 0};

layout(location = 0) out vec4 frag_colour;

vec3 hsl2rgb(vec3 hsl) {
    float t = hsl.y * ((hsl.z < 0.5) ? hsl.z : (1.0 - hsl.z));
    vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
    vec3 p = abs(fract(hsl.xxx + K.xyz) * 6.0 - K.www);
    return (hsl.z + t) * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), 2.0*t / hsl.z);
}

vec4 applyVisualization(sampler2D tex, vec2 uv, double lifetime, double dt, double freq[3]) {
  vec4 rgba = texture2D(tex, uv);
  const double maxFreq = 16000;
  double fb = clamp(freq[0]*2-1, 0, 100)/100;
  double fm = clamp(freq[1]*2-1, 0, 100)/100;
  double fh = clamp(freq[2]*2-1, 0, 100)/100;
  double r = 1 * fb;
  double g = 1 * fm;
  double b = 1 * fh;

  float dtoc = distance(uv, vec2(0.5, 0.5));
  vec4 tint = vec4(mix(rgba.x, r, 0.1), mix(rgba.y, g, 0.2), mix(rgba.z, b, 0.3), rgba.w);
  return mix(tint, rgba, 1-abs(dtoc));
}

void main() {
  vec2 uv = vec2(texCoord.x, 1-texCoord.y);
  frag_colour = applyVisualization(texture1, uv, lifetime, dt, frequencies);
}
  |]

-- Shader shamelessly copied from here:
-- https://github.com/jklintan/shaders
juliaFractalShader :: ByteString
juliaFractalShader =
  [r|
# version 410 core
// Global variables
uniform float time;

// 2D coordinates
in vec2 texCoord;
// Output fragment color
out vec4 FragColor;

// Add two complex number c = x + yi together
vec2 complex_add(vec2 lhs, vec2 rhs)
{
	return vec2(lhs.x + rhs.x, lhs.y + rhs.y);
}

// Multiply two complex number c = x + yi together
vec2 complex_mul(vec2 lhs, vec2 rhs)
{
	return vec2(lhs.x * rhs.x - lhs.y * rhs.y, lhs.x * rhs.y + lhs.y * rhs.x);
}

// Determine which points on the screen that belongs to
// the Julia set and which does not.
float julia_iteration(float max_iters, vec2 z, vec2 c) {
	for(int i = 0 ; i < max_iters ; i++) {
		if(length(z) > 2.0) {
			return i/max_iters;
		}

		// Find new z which we calculate from
		// f(z) = z^2 + c
		vec2 z_squared = complex_mul(z, z);
		z = complex_add(z_squared, c);
	}
	// This will determine the background color
	// e.g. this is a number not part of the Julia set.
	return 0.0;
}

void main()
{
	// Animation parameters, use a Hermite interpolation
	const float timespan = 20.0;  // Reset animation after 20s
	float w = smoothstep(0.0, 40.0, fract(time/timespan));
	float scale_anim_param = 1.0 + smoothstep(2.0, 0.1, fract(time/timespan));
	float anim_param = 0.64 + 80.0 * w;

	// Get the complex number to base the Julia set on, using
	// different complex numbers gives different looks of the
	// Julia set. Some interesting numbers are:
	// c = -0.70176 - 0.3842i
	// c = 0.285 + 0.01i
	// c = -0.835 - 0.2321i
	// c = -0.4 + 0.6i

	// c = i, a dendrite fractal (on the boundary of the Mandelbrot set)
	// c = -0.123 + 0.745i, Douady's rabbit fractal
	// c = -0.75, the San Marco fractal
	// c = -0.391 - 0.587i, the Siegel disk fractal

	vec2 complex_number = vec2(0.285, 0.01);

	// By multiplying the complex number with the anim_param
	// we can get interesting animations of the set.
	// complex_number = complex_mul(complex_number, vec2(anim_param, anim_param));

	float color = julia_iteration(1000, texCoord / 2, complex_number);
	float multiplier = 2.0; // Used to enhance the colors & contrast.
	FragColor = vec4(vec3(color) * multiplier, 1.0);
}
    |]
