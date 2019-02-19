module Msg exposing (Msg(..))

import Http
import Knob exposing (KnobMsg)
import Patch as P
import PinTable exposing (PinMsg)
import SynthiSchema as SS


type Msg
    = KnobEvent KnobMsg
    | PinEvent PinMsg
    | GotSchema (Result Http.Error SS.SynthiSchema)
    | GotPatches (Result Http.Error (List P.Patch))
    | AudioPinClick ( Int, Int )
    | AudioPinHover ( Int, Int )
