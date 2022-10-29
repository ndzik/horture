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
    flashbangShader,
    gifFragmentShader,
    gifVertexShader,
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

void main() {
  frag_colour = texture2D(texture1, texCoord);
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
    c = vec4(0.2, 0.15, 0.05, 1.0);
  } else {
    c = texture2D(tex, tlpos * vec2(1.0/rtW, 1.0/rtH)) * 1.4;
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
layout(location = 0) out vec4 frag_colour;

float offset[3] = float[](0.0, 1.3846153846, 3.2307602308);
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
layout(location = 0) out vec4 frag_colour;

float offset[3] = float[](0.0, 1.3846153846, 3.2307602308);
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
layout(location = 0) out vec4 frag_colour;

vec4 flashbang(sampler2D tex, vec2 uv, double lifetime, double dt) {
  vec4 c = texture2D(tex, uv);
  double pp = 1 - (dt / lifetime);
  return vec4(pp * (1 - c.x) + c.x, pp * (1 - c.y) + c.y, pp * (1 - c.z) + c.z, 1);
}

void main() {
  vec2 uv = vec2(texCoord.x, 1-texCoord.y);
  frag_colour = flashbang(texture1, uv, lifetime, dt);
}
    |]

-- | flashbangShader simply makes the screen bright and white.
cycleColoursShader :: ByteString
cycleColoursShader =
  [r|
#version 410

in vec2 texCoord;
uniform sampler2D texture1;
uniform double lifetime = 0;
uniform double dt = 0;
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
