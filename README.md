# Horture

Horture is an application for streamers who want to elevate community
interaction to a new level. In essence, **Horture** allows to remove the
boundary of a streamoverlay and a broadcasters desktop/captured application
window.

## Current state of streaming

Most streamers try to come up with interesting ways to allow their community to
not only be part of a chat, but also get acknowledged for their continued
support. E.g. using alerts which are shown in their streamoverlay to notify
about followings/subscriptions and more. This is a suitable way for handling
such events, but what if you want to go a step further?

## Horture's goal

The simplest usecase for horture is to allow the integration of displaying
events directly on your desktop/captured application window. When broadcasting
you would see what your viewers see, without any delay and without having to
rely on a second monitor having an OBS preview of your stream running.

But the above is not really what horture is intended for. It is made to test
your resilience and allow your community to directly interact with what you
see. This means triggering various effects which alter your vision. Think about
having some kind of _challenge_ day in which you try to speedrun your favourite
game, try to play your best in some PvP matches but with your community trying
to backstab you again and again and again?

## Supported effects

* Display predefined GIFs
* Composable translation/scaling/rotation effects for each displayed element:
  * Move to some position
  * Rotate
  * Orbit around a point
  * Scale
  * Stretch
  * Jitter
  * ... and more, and more to come...
* Composable shaders:
  * Flashbang -> Simulate the effect of a flashbang
  * StitchIt -> Make the broadcasters screen look like a woven picture
  * ThiccIt -> Apply a convex/concave effect to the screen
  * WhereAreMyGlasses -> Apply a blur filter
  * TakeTheWhitePill -> Lots of colours
  * ... any Shader is possible, so this list is extensible with ease

## What is to come?

If my tests work out and I can see that horture is as fun in practice as I
imagine it to be, then I will continously work on the points mentioned in the
list below:

[ ] - Windows support
[ ] - Dynamic shader effect loading: Instead of hardcode shaders into the
      application itself, make them readable from a config file/directory.
[ ] - Shader DSL: Allowing Viewers to describe GLSL shaders in simple terms
[ ] - Support basic stream events:
  [ ] - Allow displaying events for follows, subs and more
