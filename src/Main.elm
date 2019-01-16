import Browser
import Html
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)



init: () -> (Model, Cmd msg)
init flags = ({ circleFill = "#0000ff"}, Cmd.none)

subs: Model -> Sub msg
subs model = Sub.none

main =
  Browser.element { init = init, subscriptions = subs, update = update, view = view }

type Msg = OverCircle | OutCircle

type alias Model = {
    circleFill: String
  }

type alias Knob10 = {
    value: Float
  }

update: Msg -> Model -> (Model, Cmd msg)
update msg model = 
  case msg of
    OverCircle -> ({ model | circleFill = "#00ff00"}, Cmd.none)
    OutCircle -> ({ model | circleFill = "#0000ff"}, Cmd.none)


knobValueToAngle: Float -> Float
knobValueToAngle x =
  30 + x * 30

knob10Svg: Bool -> Float -> Svg msg
knob10Svg active val =
  let 
    rotation = "rotate(" ++ String.fromFloat (knobValueToAngle val) ++ " 20 20)"
  in
  g 
  [ transform rotation
  ]
  [ circle
    [ cx "20"
    , cy "20"
    , r "20"
    , fill "#333333"
    ]
    []
  , rect
    [ x "18"
    , y "20"
    , rx "2"
    , ry "2"
    , width "4"
    , height "20"
    , fill "#999999"
    ]
    []
  ]


view: Model -> Html.Html (Msg)
view model =
  Html.div []
    [ text "njaaaaghh"
    , svg
      [ width "1400"
      , height "2400"
      , viewBox "0 0 1400 2400"
      ]
      [ rect
          [ x "10"
          , y "10"
          , width "100"
          , height "100"
          , rx "15"
          , ry "15"
          , fill "#ff0000"
          ]
          []
      , circle
          [ cx "50"
          , cy "50"
          , r "50"
          , fill model.circleFill
          , onMouseOver OverCircle
          , onMouseOut OutCircle
          ]
          []
      , knob10Svg True 5.0
      ]
    ]