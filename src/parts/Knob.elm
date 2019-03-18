module Knob exposing (KnobMsg, controlsToKnobSvg, knob10Svg, knobSvg, simpleKnobSvg, simpleSwitchSvg)

import Html.Styled as Html exposing (..)
import Maybe.Extra exposing (isJust)
import Model exposing (Control(..))
import Svg.Styled as Svg exposing (..)
import Svg.Styled.Attributes exposing (..)
import Svg.Styled.Events exposing (..)


type KnobMsg
    = KnobIn ( Int, Int )
    | KnobOut


colors =
    { enabled = "#333333"
    , disabled = "#cccccc"
    }


noValue =
    "-"


hoverColor =
    "#FF8800"


knobValueToAngle : Float -> Float
knobValueToAngle x =
    30 + x * 30


simpleKnobSvg : Maybe Float -> Html.Html KnobMsg
simpleKnobSvg val =
    let
        value =
            val
                |> Maybe.map
                    (\v ->
                        String.fromFloat v
                    )
                |> Maybe.withDefault noValue

        knobColor =
            if val |> isJust then
                colors.enabled

            else
                colors.disabled
    in
    svg
        [ width "40"
        , height "40"
        , viewBox "0 0 40 40"
        ]
        [ circle
            [ cx "20"
            , cy "20"
            , r "20"
            , fill knobColor
            ]
            []
        , text_
            [ dx "20"
            , dy "25"
            , textAnchor "middle"
            , fill "#FFFFFF"
            ]
            [ Svg.text value ]
        ]


simpleSwitchSvg : Maybe String -> Html.Html KnobMsg
simpleSwitchSvg val =
    let
        value =
            case val of
                Just v ->
                    v

                Nothing ->
                    noValue

        knobColor =
            if val |> isJust then
                colors.enabled

            else
                colors.disabled
    in
    svg
        [ width "40"
        , height "40"
        , viewBox "0 0 40 40"
        ]
        [ rect
            [ width "40"
            , height "40"
            , fill knobColor
            ]
            []
        , text_
            [ dx "20"
            , dy "25"
            , textAnchor "middle"
            , fill "#FFFFFF"
            ]
            [ Svg.text value ]
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
            [ Svg.text (String.fromFloat val)
            ]
        , svg [ y "30" ]
            [ simpleKnobSvg (Just val)
            ]
        ]


controlsToKnobSvg : List Control -> Html.Html KnobMsg
controlsToKnobSvg cs =
    Html.div []
        (cs
            |> List.map
                (\ctrl ->
                    case ctrl of
                        KnobCtrl { name, value } ->
                            simpleKnobSvg value

                        SwitchCtrl { name, case_ } ->
                            simpleSwitchSvg case_
                )
        )


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
