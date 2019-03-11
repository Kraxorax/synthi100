module Msg exposing (Msg(..))

import AudioMsg exposing (AudioMsg)
import Browser exposing (UrlRequest)
import Http
import Knob exposing (KnobMsg)
import Mouse exposing (MouseData)
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
    | Play P.Patch
    | Pause P.Patch
    | Ended P.Patch
    | TimeUpdate P.Patch Float
    | Seek P.Patch MouseData
