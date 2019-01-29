module Knob exposing (KnobMsg, knob10Svg, knobSvg)

import Html
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)


type KnobMsg
    = KnobIn ( Int, Int )
    | KnobOut


hoverColor =
    "#FF8800"


knobValueToAngle : Float -> Float
knobValueToAngle x =
    30 + x * 30


simpleKnobSvg : Float -> Html.Html KnobMsg
simpleKnobSvg val =
    svg
        [ width "40"
        , height "40"
        , viewBox "0 0 40 40"
        ]
        [ circle
            [ cx "20"
            , cy "20"
            , r "20"
            , fill "#333333"
            ]
            []
        , text_
            [ dx "20"
            , dy "25"
            , textAnchor "middle"
            , fill "#FFFFFF"
            ]
            [ text (String.fromFloat val) ]
        ]


knob10Svg : ( Int, Int ) -> Bool -> Float -> Html.Html KnobMsg
knob10Svg ( kx, ky ) active val =
    let
        xPos =
            kx * 50

        yPos =
            ky * 80

        rotation =
            "rotate(" ++ String.fromFloat (knobValueToAngle val) ++ " 20 20)"

        textColor =
            if active then
                hoverColor

            else
                "#333333"
    in
    svg
        [ x (String.fromInt xPos)
        , y (String.fromInt yPos)
        , width "40"
        , height "70"
        , onMouseOver (KnobIn ( kx, ky ))
        , onMouseOut KnobOut
        ]
        [ text_
            [ dx "20"
            , dy "20"
            , textAnchor "middle"
            , color textColor
            ]
            [ text (String.fromFloat val)
            ]
        , svg [ y "30" ]
            [ simpleKnobSvg val
            ]
        ]


knobSvg : Bool -> String -> Svg msg
knobSvg active rotation =
    svg
        [ width "40"
        , height "40"
        , viewBox "0 0 40 40"
        , transform rotation
        ]
        [ circle
            [ cx "20"
            , cy "20"
            , r "20"
            , fill
                (if active then
                    hoverColor

                 else
                    "#333333"
                )
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
