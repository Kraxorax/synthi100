module Events exposing (onDurationChange, onEnded, onTimeUpdate)

import Html
import Html.Events as HE
import Json.Decode as JD
import Msg exposing (Msg(..))
import Patch exposing (Patch)


onEnded : msg -> Html.Attribute msg
onEnded msg =
    HE.on "ended" (JD.succeed msg)


onDurationChange : (Float -> msg) -> Html.Attribute msg
onDurationChange msg =
    HE.on "durationchange" (JD.map msg targetDuration)


onTimeUpdate : (Float -> msg) -> Html.Attribute msg
onTimeUpdate msg =
    HE.on "timeupdate" (JD.map msg targetCurrentTime)


targetDuration : JD.Decoder Float
targetDuration =
    JD.at [ "target", "duration" ] JD.float


targetCurrentTime : JD.Decoder Float
targetCurrentTime =
    JD.at [ "target", "currentTime" ] JD.float
