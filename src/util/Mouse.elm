module Mouse exposing (MouseData, mouseDecoder)

import Json.Decode exposing (Decoder, at, float, int, map4)


type alias MouseData =
    { offsetX : Int
    , offsetY : Int
    , offsetHeight : Float
    , offsetWidth : Float
    }


mouseDecoder : Decoder MouseData
mouseDecoder =
    map4 MouseData
        (at [ "offsetX" ] int)
        (at [ "offsetY" ] int)
        (at [ "target", "offsetHeight" ] float)
        (at [ "target", "offsetWidth" ] float)
