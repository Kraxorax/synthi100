module Msg exposing (Msg(..))

import AudioMsg exposing (AudioMsg)
import Browser exposing (UrlRequest)
import Http
import Knob exposing (KnobMsg)
import Model exposing (SortDirection)
import Mouse exposing (MouseData)
import Patch as P
import PinTable exposing (Panel, PinMsg)
import SynthiSchema as SS
import Url exposing (Url)


type Msg
    = UpdateUrl Url
    | RequestedUrl UrlRequest
    | InputKnobEvent KnobMsg
    | OutputKnobEvent KnobMsg
    | PinEvent PinMsg
    | GotSchema (Result Http.Error SS.SynthiSchema)
    | GotPatches (Result Http.Error (List P.Patch))
    | PinClick Panel ( Int, Int )
    | PinHover Panel ( Int, Int )
    | PinOut
    | Play P.Patch
    | Pause P.Patch
    | Ended P.Patch
    | TimeUpdate P.Patch Float
    | Seek P.Patch MouseData
    | VolumeChange Float
    | Filter String String
    | SortBy String
    | Sort SortDirection
    | MovePatch P.Patch Int
