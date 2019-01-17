import Browser
import Html
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Array
import Matrix exposing (Matrix, generate, toArray)


init: () -> (Model, Cmd msg)
init flags = ({ circleFill = "#0000ff"}, Cmd.none)

table: Matrix (Svg msg)
table = generate 8 7 (\x y -> knob10Svg (x * 50, y * 50) True (toFloat (x + y)))

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

knob10Svg: (Int, Int) -> Bool -> Float -> Svg msg
knob10Svg (kx, ky) active val =
  let 
    rotation = "rotate(" ++ String.fromFloat (knobValueToAngle val) ++ " 20 20)"
  in
  svg
  [ x (String.fromInt kx)
  , y (String.fromInt ky)
  ]
  [
    svg
    [ width "40"
    , height "40"
    , viewBox "0 0 40 40"
    , transform rotation
    ]
      [
        circle
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
  ]



view: Model -> Html.Html (Msg)
view model =
  let
    knobs = table |> toArray |> Array.toList
  in

  Html.div []
    [ svg
      [ width "1400"
      , height "2400"
      , viewBox "0 0 1400 2400"
      ]
      knobs
    ]