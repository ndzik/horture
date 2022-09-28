# Horture architecture

Split into multiple modules, each responsible for a simple task:

* CommandModule:
  - This module provides external inputs to horture. For example a module
    listening to channel point redemptions on Twitch for specific channel.
* Command&Control:
  - Horture user direct interface.
  - Allows setting runtime parameters, which application to horture and direct
    control flow between other modules.
* ExecutionModule:
  - The interactive module which visualizes commands issued to horture.
  - Uses OpenGL.
