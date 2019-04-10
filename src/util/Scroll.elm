module Scroll exposing (..)

import Html.Styled as H
import Html.Styled.Events as HE
import Json.Decode as JD

type alias ScrollEvent =
  { scrollHeight : Int
  , scrollPos : Int
  , visibleHeight : Int
  }

onScroll : (ScrollEvent -> msg) -> H.Attribute msg
onScroll tagger =
  HE.on "scroll" (JD.map tagger onScrollJsonParser)

onScrollJsonParser : JD.Decoder ScrollEvent
onScrollJsonParser =
  JD.map3 ScrollEvent
    (JD.at ["target", "scrollHeight"] JD.int)
    (JD.at ["target", "scrollTop"] JD.int)
    (JD.at ["target", "clientHeight"] JD.int)
