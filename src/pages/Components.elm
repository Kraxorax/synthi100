module Components exposing (audioNode, cssHaxor, outputPanel, playButton, volumeIcon, volumeInput, volumeSlider, waveformSeeker)

import AudioModel exposing (..)
import Css as Css exposing (..)
import Events exposing (..)
import Html.Attributes.Extra as HAE
import Html.Styled as HS exposing (..)
import Html.Styled.Attributes as HSA exposing (..)
import Html.Styled.Events as HS exposing (..)
import Json.Decode as JD exposing (..)
import Json.Encode as JE
import Knob exposing (KnobMsg, simpleKnobSvg)
import Maybe.Extra exposing (isJust)
import Model exposing (..)
import Mouse exposing (..)
import Msg exposing (..)
import Patch exposing (..)
import Styles exposing (theBlue)
import Svg.Styled as Svg exposing (..)
import Svg.Styled.Attributes as Svg exposing (..)
import Svg.Styled.Events as Svg exposing (..)
import ViewModel exposing (Knob)


volumeThumbCss : Style
volumeThumbCss =
    batch
        [ Css.property "-webkit-appearance" "none"
        , Css.width (px 9)
        , Css.height (px 23)
        , backgroundColor theBlue
        , borderColor (hex "000000")
        , borderWidth (px 1)
        , borderStyle solid
        , borderRadius (px 0)
        ]


volumeSliderCss : Style
volumeSliderCss =
    batch
        [ Css.property "-webkit-appearance" "none"
        , Css.property "-moz-appearance" "none"
        , Css.width (px 102)
        , Css.height (px 2)
        , minWidth (px 88)
        , marginRight (px 80)
        , pseudoElement "-moz-range-thumb" [ volumeThumbCss ]
        , pseudoElement "-webkit-slider-thumb" [ volumeThumbCss ]
        ]


muteButtonCss : Style
muteButtonCss =
    batch
        [ Css.property "-webkit-appearance" "none"
        , Css.property "-moz-appearance" "none"
        , backgroundRepeat noRepeat
        , Css.height (px 28)
        , minWidth (px 33)
        , margin (px 2)
        , Css.checked [ backgroundImage (url "/unmute.svg") ]
        , backgroundImage (url "/mute.svg")
        ]


volumeInputCss : Style
volumeInputCss =
    batch
        [ boxSizing borderBox
        , padding4 (px 0) (pct 8) (px 0) (px 0)
        , displayFlex
        , justifyContent flexEnd
        , alignItems center
        , Css.fontWeight bold
        , Css.fontSize (px 20)
        , borderTop2 (px 1) solid
        , Css.height (px 46)
        , Css.maxWidth (pct 50)
        ]


volumeInput : Bool -> Html Msg
volumeInput muted =
    div [ HSA.css [ volumeInputCss ] ]
        [ span [ HSA.css [ marginRight auto ] ] [ HS.text "volume" ]
        , volumeSlider
        , volumeIcon muted
        ]


volumeSlider : Html Msg
volumeSlider =
    input
        [ HSA.css [ volumeSliderCss ]
        , HSA.type_ "range"
        , onInput (String.toFloat >> Maybe.withDefault 0.5 >> VolumeChange)
        ]
        []


volumeIcon : Bool -> Html Msg
volumeIcon muted =
    input
        [ HSA.css [ muteButtonCss ]
        , HSA.type_ "checkbox"
        , HSA.checked (not muted)
        ]
        []


waveformSeeker : Bool -> Patch -> Html Msg
waveformSeeker isBlack patch =
    let
        bgSource =
            patch.waveformSmall

        seekerColor =
            hex "ffffff"

        seekerPosition =
            patch.audioModel
                |> Maybe.map
                    (\am ->
                        100 / (patch.duration / am.seekerPosition)
                    )

        color =
            if isBlack then
                HSA.style "filter" "brightness(0.01%)"

            else
                HSA.style "filter" "brightness(100%)"

        seeker =
            if seekerPosition |> isJust then
                div
                    [ HSA.css
                        [ Css.height (pct 100)
                        , Css.width (px 1)
                        , backgroundColor seekerColor
                        , Css.position absolute
                        , top (px 0)
                        , left (pct (seekerPosition |> Maybe.withDefault 0))
                        , Css.cursor colResize
                        ]
                    ]
                    []

            else
                div [] []
    in
    div
        [ HS.on "click" (JD.map (Seek patch) mouseDecoder)
        , HSA.css
            [ Css.width (pct 100)
            , Css.height (pct 100)
            , Css.position relative
            , Css.float left
            ]
        ]
        [ img [ src bgSource, color, HSA.css [ Css.width (pct 100), Css.height (pct 100) ] ] []
        , seeker
        ]


audioNode : Model -> Patch -> Html Msg
audioNode model patch =
    let
        playing =
            case patch.audioModel of
                Just am ->
                    am.playing

                Nothing ->
                    False
    in
    if playing then
        audio
            [ HSA.id patch.title
            , src patch.soundUrl
            , HAE.volume model.volume |> HSA.fromUnstyled
            , onEnded (Ended patch) |> HSA.fromUnstyled
            , onTimeUpdate (TimeUpdate patch) |> HSA.fromUnstyled
            ]
            []

    else
        audio
            [ HSA.id patch.title
            , src patch.soundUrl
            , HSA.property "preload" (JE.string "none")
            ]
            []


playButton : Patch -> Html Msg
playButton patch =
    let
        audioModel =
            patch.audioModel |> Maybe.withDefault noPlayingAudioModel

        ( hndlClck, bttnUrl ) =
            if audioModel.playing then
                ( Pause patch, "/pause.svg" )

            else
                ( Play patch, "/play.svg" )
    in
    img [ src bttnUrl, HS.onClick hndlClck ] []


cssHaxor : Html msg
cssHaxor =
    div [ HSA.css [ Css.property "clear" "both" ] ] []


chanSwitch : Bool -> Html msg
chanSwitch on =
    let
        ( h, txt ) =
            if on then
                ( "8", "on" )

            else
                ( "23", "off" )
    in
    svg [ x "11" ]
        [ rect [ x "1", y "1", Svg.width "16", Svg.height "30", Svg.stroke Styles.theDarkGrayString, Svg.strokeWidth "2px", Svg.fill "none" ] []
        , line [ x1 "0", y1 "16", x2 "17", y2 "16", Svg.stroke Styles.theDarkGrayString, Svg.strokeWidth "2px" ] []
        , Svg.circle [ cx "9", cy h, r "4", Svg.fill Styles.theDarkGrayString ] []

        -- , rect [ x "0", y h, Svg.width "17", Svg.height "16", Svg.fill "black" ] []
        , Svg.text_ [ dx "8", dy "47" ] [ Svg.text txt ]
        ]


chanHandle : Html msg
chanHandle =
    rect [ Svg.width "38", Svg.height "9", Svg.fill "black", Svg.strokeWidth "2px", Svg.stroke "#9b9b9b" ] []


sfi =
    String.fromInt


outputPanel : List ( Knob, Knob, Knob ) -> Html KnobMsg
outputPanel knobs =
    let
        vhStart =
            310

        rsNumStart =
            314

        vhOffset =
            23

        vhEnd =
            713

        vvBegin =
            33

        vvStart =
            52

        vvOffset =
            92

        vvEnd =
            564

        knobOffset =
            60

        switches =
            g [ Svg.color "black" ]
                (knobs
                    |> List.indexedMap
                        (\i ( level, filter, pan ) ->
                            let
                                swch =
                                    chanSwitch (level.value |> isJust)

                                xp =
                                    vvBegin + (i * vvOffset) |> String.fromInt
                            in
                            svg [ x xp, y "245" ]
                                [ swch
                                ]
                        )
                )

        handles =
            g [ Svg.color "black" ]
                (knobs
                    |> List.indexedMap
                        (\i ( level, filter, pan ) ->
                            let
                                val =
                                    level.value |> Maybe.withDefault 0

                                xp =
                                    vvBegin + (i * vvOffset) |> String.fromInt

                                hy =
                                    44 + ((10 - val) * 22.7) |> String.fromFloat

                                handle =
                                    svg [ x "0", y hy ] [ chanHandle ]
                            in
                            svg [ x xp, y "261" ]
                                [ handle
                                ]
                        )
                )

        filterKnobs =
            g []
                (knobs
                    |> List.indexedMap
                        (\i ( level, filter, pan ) ->
                            let
                                xp =
                                    vvBegin + (i * vvOffset) |> String.fromInt
                            in
                            svg [ x xp, y "90" ]
                                [ simpleKnobSvg filter
                                ]
                        )
                )

        panKnobs =
            g []
                (knobs
                    |> List.indexedMap
                        (\i ( level, filter, pan ) ->
                            let
                                xp =
                                    vvBegin + (i * vvOffset) |> String.fromInt
                            in
                            svg [ x xp, y "180" ]
                                [ simpleKnobSvg pan
                                ]
                        )
                )
    in
    svg [ Svg.height "600px", Svg.width "740px", Svg.fontFamily "Metropolis", textAnchor "middle", Svg.fontSize "14px" ]
        [ line [ x1 (sfi vvBegin), y1 "1px", x2 (sfi vhEnd), y2 "1px", Svg.stroke "black" ] []

        -- chennel numbers
        , g
            [ Svg.fontWeight "bold" ]
            [ Svg.text_ [ dx "52px", dy "28px" ] [ Svg.text "1" ]
            , Svg.text_ [ dx "144px", dy "28px" ] [ Svg.text "2" ]
            , Svg.text_ [ dx "236px", dy "28px" ] [ Svg.text "3" ]
            , Svg.text_ [ dx "696px", dy "28px" ] [ Svg.text "8" ]
            , Svg.text_ [ dx "328px", dy "28px" ] [ Svg.text "4" ]
            , Svg.text_ [ dx "420px", dy "28px" ] [ Svg.text "5" ]
            , Svg.text_ [ dx "512px", dy "28px" ] [ Svg.text "6" ]
            , Svg.text_ [ dx "604px", dy "28px" ] [ Svg.text "7" ]
            ]
        , line [ x1 (sfi vvBegin), y1 "45px", x2 (sfi vhEnd), y2 "45px", Svg.stroke "black", Svg.strokeWidth "1px" ] []

        -- filter & pan
        , g [ textAnchor "start", Svg.fill "white" ]
            [ Svg.text_ [ dx (sfi vvBegin), dy "70px" ] [ Svg.text "filter" ]
            , line [ x1 (sfi vvBegin), y1 "85px", x2 (sfi vhEnd), y2 "85px", Svg.stroke "white", Svg.strokeWidth "1px" ] []
            , filterKnobs
            , line [ x1 (sfi vvBegin), y1 "135px", x2 (sfi vhEnd), y2 "135px", Svg.stroke "white", Svg.strokeWidth "1px" ] []
            , Svg.text_ [ dx (sfi vvBegin), dy "160px" ] [ Svg.text "pan" ]
            , line [ x1 (sfi vvBegin), y1 "175px", x2 (sfi vhEnd), y2 "175px", Svg.stroke "white", Svg.strokeWidth "1px" ] []
            , panKnobs
            , line [ x1 (sfi vvBegin), y1 "225px", x2 (sfi vhEnd), y2 "225px", Svg.stroke "white", Svg.strokeWidth "1px" ] []
            ]

        -- switches
        , switches

        -- volume - horizontal
        , line [ x1 (sfi vvBegin), y1 (sfi (vhStart + vhOffset * 0)), x2 (sfi vhEnd), y2 (sfi (vhStart + vhOffset * 0)), Svg.stroke "white", Svg.strokeWidth "1px" ] []
        , line [ x1 (sfi vvBegin), y1 (sfi (vhStart + vhOffset * 1)), x2 (sfi vhEnd), y2 (sfi (vhStart + vhOffset * 1)), Svg.stroke "white", Svg.strokeWidth "1px" ] []
        , line [ x1 (sfi vvBegin), y1 (sfi (vhStart + vhOffset * 2)), x2 (sfi vhEnd), y2 (sfi (vhStart + vhOffset * 2)), Svg.stroke "white", Svg.strokeWidth "1px" ] []
        , line [ x1 (sfi vvBegin), y1 (sfi (vhStart + vhOffset * 3)), x2 (sfi vhEnd), y2 (sfi (vhStart + vhOffset * 3)), Svg.stroke "white", Svg.strokeWidth "1px" ] []
        , line [ x1 (sfi vvBegin), y1 (sfi (vhStart + vhOffset * 4)), x2 (sfi vhEnd), y2 (sfi (vhStart + vhOffset * 4)), Svg.stroke "white", Svg.strokeWidth "1px" ] []
        , line [ x1 (sfi vvBegin), y1 (sfi (vhStart + vhOffset * 5)), x2 (sfi vhEnd), y2 (sfi (vhStart + vhOffset * 5)), Svg.stroke "white", Svg.strokeWidth "1px" ] []
        , line [ x1 (sfi vvBegin), y1 (sfi (vhStart + vhOffset * 6)), x2 (sfi vhEnd), y2 (sfi (vhStart + vhOffset * 6)), Svg.stroke "white", Svg.strokeWidth "1px" ] []
        , line [ x1 (sfi vvBegin), y1 (sfi (vhStart + vhOffset * 7)), x2 (sfi vhEnd), y2 (sfi (vhStart + vhOffset * 7)), Svg.stroke "white", Svg.strokeWidth "1px" ] []
        , line [ x1 (sfi vvBegin), y1 (sfi (vhStart + vhOffset * 8)), x2 (sfi vhEnd), y2 (sfi (vhStart + vhOffset * 8)), Svg.stroke "white", Svg.strokeWidth "1px" ] []
        , line [ x1 (sfi vvBegin), y1 (sfi (vhStart + vhOffset * 9)), x2 (sfi vhEnd), y2 (sfi (vhStart + vhOffset * 9)), Svg.stroke "white", Svg.strokeWidth "1px" ] []
        , line [ x1 (sfi vvBegin), y1 (sfi (vhStart + vhOffset * 10)), x2 (sfi vhEnd), y2 (sfi (vhStart + vhOffset * 10)), Svg.stroke "white", Svg.strokeWidth "1px" ] []

        -- volume - vertical
        , line [ x1 (sfi (vvStart + vvOffset * 0)), y1 (sfi vhStart), x2 (sfi (vvStart + vvOffset * 0)), y2 (sfi vvEnd), Svg.stroke "white", Svg.strokeWidth "4px" ] []
        , line [ x1 (sfi (vvStart + vvOffset * 1)), y1 (sfi vhStart), x2 (sfi (vvStart + vvOffset * 1)), y2 (sfi vvEnd), Svg.stroke "white", Svg.strokeWidth "4px" ] []
        , line [ x1 (sfi (vvStart + vvOffset * 2)), y1 (sfi vhStart), x2 (sfi (vvStart + vvOffset * 2)), y2 (sfi vvEnd), Svg.stroke "white", Svg.strokeWidth "4px" ] []
        , line [ x1 (sfi (vvStart + vvOffset * 3)), y1 (sfi vhStart), x2 (sfi (vvStart + vvOffset * 3)), y2 (sfi vvEnd), Svg.stroke "white", Svg.strokeWidth "4px" ] []
        , line [ x1 (sfi (vvStart + vvOffset * 4)), y1 (sfi vhStart), x2 (sfi (vvStart + vvOffset * 4)), y2 (sfi vvEnd), Svg.stroke "white", Svg.strokeWidth "4px" ] []
        , line [ x1 (sfi (vvStart + vvOffset * 5)), y1 (sfi vhStart), x2 (sfi (vvStart + vvOffset * 5)), y2 (sfi vvEnd), Svg.stroke "white", Svg.strokeWidth "4px" ] []
        , line [ x1 (sfi (vvStart + vvOffset * 6)), y1 (sfi vhStart), x2 (sfi (vvStart + vvOffset * 6)), y2 (sfi vvEnd), Svg.stroke "white", Svg.strokeWidth "4px" ] []
        , line [ x1 (sfi (vvStart + vvOffset * 7)), y1 (sfi vhStart), x2 (sfi (vvStart + vvOffset * 7)), y2 (sfi vvEnd), Svg.stroke "white", Svg.strokeWidth "4px" ] []

        -- handles
        , handles

        -- black - end - line
        , line [ x1 (sfi vvBegin), y1 (sfi vvEnd), x2 (sfi vhEnd), y2 (sfi vvEnd), Svg.stroke "black", Svg.strokeWidth "1px" ] []

        -- left side numbers
        , g
            [ Svg.fontWeight "500", Svg.fill "#4a4a4a" ]
            [ Svg.text_ [ dx "16px", dy (sfi (rsNumStart + vhOffset * 0)) ] [ Svg.text "10" ]
            , Svg.text_ [ dx "16px", dy (sfi (rsNumStart + vhOffset * 1)) ] [ Svg.text "9" ]
            , Svg.text_ [ dx "16px", dy (sfi (rsNumStart + vhOffset * 2)) ] [ Svg.text "8" ]
            , Svg.text_ [ dx "16px", dy (sfi (rsNumStart + vhOffset * 3)) ] [ Svg.text "7" ]
            , Svg.text_ [ dx "16px", dy (sfi (rsNumStart + vhOffset * 4)) ] [ Svg.text "6" ]
            , Svg.text_ [ dx "16px", dy (sfi (rsNumStart + vhOffset * 5)) ] [ Svg.text "5" ]
            , Svg.text_ [ dx "16px", dy (sfi (rsNumStart + vhOffset * 6)) ] [ Svg.text "4" ]
            , Svg.text_ [ dx "16px", dy (sfi (rsNumStart + vhOffset * 7)) ] [ Svg.text "3" ]
            , Svg.text_ [ dx "16px", dy (sfi (rsNumStart + vhOffset * 8)) ] [ Svg.text "2" ]
            , Svg.text_ [ dx "16px", dy (sfi (rsNumStart + vhOffset * 9)) ] [ Svg.text "1" ]
            , Svg.text_ [ dx "16px", dy (sfi (rsNumStart + vhOffset * 10)) ] [ Svg.text "0" ]
            ]
        ]
