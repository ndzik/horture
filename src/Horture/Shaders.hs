{-# LANGUAGE QuasiQuotes #-}

module Horture.Shaders
  ( hortureVertexGIF,
    hortureFragmentGIF,
    hortureVertexShader,
    hortureFragmentShader,
  )
where

import Data.ByteString.Char8 (ByteString)
import Text.RawString.QQ

hortureVertexGIF :: ByteString
hortureVertexGIF =
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

hortureFragmentGIF :: ByteString
hortureFragmentGIF =
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

hortureVertexShader :: ByteString
hortureVertexShader =
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

hortureFragmentShader :: ByteString
hortureFragmentShader =
  [r|
#version 410

in vec2 texCoord;

uniform float dt;
uniform sampler2D texture1;

out vec4 frag_colour;

void main() {
  vec4 colour = texture(texture1, texCoord);
  // frag_colour = vec4(colour.x+sin(4*texCoord.x+dt), colour.y+cos(12*texCoord.y+dt), colour.z-sin(3*dt), colour.w);
  frag_colour = vec4(colour.x, colour.y, colour.z, 1);
}
    |]
