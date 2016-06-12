{- |
The simplest implementation of a Tidal MIDI synth mapping.

Use this as a start for your own mappings.
-}
module Sound.Tidal.MIDI.Synth where

import Sound.Tidal.Params
import Sound.Tidal.MIDI.Control

{- | In addition to the standard params

this defines the following general parameters:

* 'modwheel'
* 'pan'
* 'expression'
* 'sustainpedal'
-}
synthController :: ControllerShape
synthController = ControllerShape {
  controls = [ 
    mCC modwheel_p 1,
    mCC pan_p 10,
    mCC expression_p 11,
    mCC sustainpedal_p 64
     ],
  latency = 0.04
  }

synth = toShape synthController
