module Knob exposing (KnobMsg(..), knobSvg, simpleKnobSvg, simpleSwitchSvg)

import Html.Styled as Html exposing (..)
import Maybe.Extra exposing (isJust)
import Svg.Styled as Svg exposing (..)
import Svg.Styled.Attributes exposing (..)
import Svg.Styled.Events exposing (..)
import ViewModel exposing (Control(..), Knob, Switch)


type KnobMsg
    = KnobIn ( Int, Int )
    | KnobOut
    | KnobHover String


colors =
    { enabled = "#ccc"
    , disabled = "#999"
    }


noValue =
    "–"


hoverColor =
    "#FF8800"


knobValueToAngle : Float -> Float
knobValueToAngle x =
    30 + x * 30


simpleKnobSvg : Knob -> Html.Html KnobMsg
simpleKnobSvg knob =
    let
        val =
            knob.value

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
        [ width "43"
        , height "43"
        , viewBox "-1 -1 42 42"
        , onMouseOver (KnobHover knob.name)
        , onMouseOut KnobOut
        ]
        [ circle
            [ cx "20"
            , cy "20"
            , r "20"
            , fill knobColor
            , stroke colors.enabled
            ]
            []
        , text_
            [ dx "20"
            , dy "25"
            , textAnchor "middle"
            , fontWeight "normal"
            , fill "#000"
            ]
            [ Svg.text value ]
        ]


simpleSwitchSvg : Switch -> Html.Html KnobMsg
simpleSwitchSvg switch =
    let
        val =
            switch.case_

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
        , height "41"
        , viewBox "0 0 40 40"
        , onMouseOver (KnobHover switch.name)
        , onMouseOut KnobOut
        ]
        [ rect
            [ width "40"
            , height "40"
            , fill knobColor
            , stroke colors.enabled
            ]
            []
        , text_
            [ dx "20"
            , dy "25"
            , textAnchor "middle"
            , fill "#000"
            ]
            [ Svg.text value ]
        ]


controlsToKnobSvg : List Control -> Html.Html KnobMsg
controlsToKnobSvg cs =
    Html.div []
        (cs
            |> List.map
                (\ctrl ->
                    case ctrl of
                        KnobCtrl knob ->
                            simpleKnobSvg knob

                        SwitchCtrl sw ->
                            simpleSwitchSvg sw
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
