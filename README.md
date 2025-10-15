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

## Video with a debug build

https://github.com/user-attachments/assets/d85e1490-f7be-4ef7-87d8-3c375a46356a

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

## Screenshots

Example screenshots. Since horture is heavily relying on movement and the
described effects, screenshots do it no real justice, but just imagine
everything moving (;.

![Screenshot_2022-10-25-10_1600x900](https://user-images.githubusercontent.com/33512740/197868429-43db91b2-5efb-4c72-bbd6-32072e5a9b14.png)
![Screenshot_2022-10-25-45_1600x900](https://user-images.githubusercontent.com/33512740/197868472-7f7064e2-c2db-4673-a61d-a1822ccba369.png)

As can be seen, horture is currently applied to my browser window. The browser
itself is moved around in 3d-space and can still be used "as usual", or as good
as you are still capable of. Further GIFs are scattered and moving all around.
All effects are, as mentioned, composable and triggerable by your viewers via
some redemption mechanism. The second screenshot shows your screen transformed
and having `WhereAreMyGlasses`, `ThiccIt` and `StitchIt` effects applied.

## What is to come?

If my tests work out and I can see that horture is as fun in practice as I
imagine it to be, then I will continously work on the points mentioned in the
list below:

- [ ] Windows support
- [ ] Dynamic shader effect loading: Instead of hardcode shaders into the
      application itself, make them readable from a config file/directory.
- [ ] Shader DSL: Allowing Viewers to describe GLSL shaders in simple terms
- [ ] Support basic stream events:
   - [ ] Allow displaying events for follows, subs and more
- [ ] Youtube stream backend, currently only Twitch is supported
