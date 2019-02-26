module Msg exposing (Msg(..))

import AudioPlayer exposing (AudioMsg)
import Browser exposing (UrlRequest)
import Http
import Knob exposing (KnobMsg)
import Patch as P
import PinTable exposing (PinMsg)
import SynthiSchema as SS
import Url exposing (Url)


type Msg
    = UpdateUrl Url
    | RequestedUrl UrlRequest
    | KnobEvent KnobMsg
    | PinEvent PinMsg
    | GotSchema (Result Http.Error SS.SynthiSchema)
    | GotPatches (Result Http.Error (List P.Patch))
    | AudioPinClick ( Int, Int )
    | AudioPinHover ( Int, Int )
    | AudioPlayer AudioMsg
    | Play P.Patch
    | Ended P.Patch
    | TimeUpdate P.Patch Float
