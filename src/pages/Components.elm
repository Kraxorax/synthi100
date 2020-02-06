module Components exposing (OutputChanValues, audioNode, cssHaxor, outputPanel, playButton, volumeIcon, volumeSlider, waveformSeeker)

import AudioModel exposing (..)
import Css as Css exposing (..)
import Events exposing (..)
import Html
import Html.Attributes.Extra as HAE
import Html.Styled as HS exposing (..)
import Html.Styled.Attributes as HSA exposing (..)
import Html.Styled.Events as HS exposing (..)
import Html.Styled.Lazy exposing (..)
import Json.Decode as JD exposing (..)
import Json.Encode as JE
import Knob exposing (KnobMsg, simpleKnobSvg)
import List
import Maybe.Extra exposing (isJust)
import Model exposing (..)
import Mouse exposing (..)
import Msg exposing (..)
import Patch exposing (..)
import Styles exposing (theBlue, theLightGrayString)
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
        , Css.maxWidth (px 70)
        , Css.height (px 2)
        , pseudoElement "-moz-range-thumb" [ volumeThumbCss ]
        , pseudoElement "-webkit-slider-thumb" [ volumeThumbCss ]
        ]


muteButtonCss : AssetFlags -> Style
muteButtonCss assets =
    batch
        [ Css.property "-webkit-appearance" "none"
        , Css.property "-moz-appearance" "none"
        , backgroundRepeat noRepeat
        , Css.height (px 22)
        , Css.width (px 25)
        , backgroundSize (px 25)
        , margin (px 0)
        , Css.checked [ backgroundImage (url assets.svg.unmute) ]
        , backgroundImage (url assets.svg.mute)
        ]


volumeInputCss : Style
volumeInputCss =
    batch
        [ padding4 (px 0) (pct 8) (px 0) (px 0)
        , displayFlex
        , justifyContent flexEnd
        , alignItems center
        , Css.fontWeight bold
        , Css.fontSize (px 20)
        , borderTop2 (px 1) solid
        , Css.height (px 46)
        , Css.maxWidth (pct 50)
        ]



volumeSlider : Html Msg
volumeSlider =
    input
        [ HSA.id "volumeSlider"
        , HSA.css [ volumeSliderCss ]
        , HSA.type_ "range"
        , onInput (String.toFloat >> Maybe.withDefault 0.5 >> VolumeChange)
        ]
        []


volumeIcon : Model -> Html Msg
volumeIcon model =
    input
        [ HSA.css [ muteButtonCss model.assets ]
        , HSA.type_ "checkbox"
        , HSA.checked (not (model.volume == 0))
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
            , HSA.loop model.loop
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


playButton : Patch -> Int -> AssetFlags -> Html Msg
playButton patch size assets =
    let
        audioModel =
            patch.audioModel |> Maybe.withDefault noPlayingAudioModel

        ( hndlClck, bttnUrl ) =
            if audioModel.playing then
                ( Pause patch, assets.svg.pause )

            else
                ( Play patch, assets.svg.pause )
    in
    img [ src bttnUrl, HS.onClick hndlClck, HSA.width size, HSA.height size ] []


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
    svg [ x "11", Svg.css [Css.overflow auto]] -- unhide partially hidden text
        [ rect [ x "1", y "1", Svg.width "16", Svg.height "30", Svg.stroke Styles.theDarkGrayString, Svg.strokeWidth "2px", Svg.fill "none" ] []
        , line [ x1 "0", y1 "16", x2 "17", y2 "16", Svg.stroke Styles.theDarkGrayString, Svg.strokeWidth "2px" ] []
        , Svg.circle [ cx "9", cy h, r "4", Svg.fill Styles.theDarkGrayString ] []
        , Svg.text_ [ dx "9", dy "47" ] [ Svg.text txt ]
        ]


chanHandle : Html msg
chanHandle =
    rect [ Svg.width "38", Svg.height "9", Svg.fill "black", Svg.strokeWidth "2px", Svg.stroke "#9b9b9b" ] []


sfi =
    String.fromInt


type alias OutputChanValues =
    { level : Knob
    , filter : Knob
    , pan : Knob
    , on : Bool
    }


outputPanel : List OutputChanValues -> Html KnobMsg
outputPanel chanVals =
    let
        vhStart =
            327

        rsNumStart =
            332

        vhOffset =
            24

        vhEnd =
            750

        vvBegin =
            33

        vvStart =
            52

        vvOffset =
            96

        vvEnd =
            593

        knobOffset =
            60

        switches =
            g [ Svg.color "black" ]
                (chanVals
                    |> List.indexedMap
                        (\i cv ->
                            let
                                swch =
                                    chanSwitch cv.on

                                xp =
                                    vvBegin + (i * vvOffset) |> String.fromInt
                            in
                            svg [ x xp, y "267" ]
                                [ swch
                                ]
                        )
                )

        handles =
            g [ Svg.color "black" ]
                (chanVals
                    |> List.indexedMap
                        (\i cv ->
                            let
                                val =
                                    cv.level.value |> Maybe.withDefault 0

                                xp =
                                    vvBegin + (i * vvOffset) |> String.fromInt

                                hy =
                                    63 + ((10 - val) * 24) |> String.fromFloat

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
                (chanVals
                    |> List.indexedMap
                        (\i cv ->
                            let
                                xp =
                                    vvBegin + (i * vvOffset) |> String.fromInt
                            in
                            svg [ x xp, y "95" ]
                                [ simpleKnobSvg cv.filter
                                ]
                        )
                )

        panKnobs =
            g []
                (chanVals
                    |> List.indexedMap
                        (\i cv ->
                            let
                                xp =
                                    vvBegin + (i * vvOffset) |> String.fromInt
                            in
                            svg [ x xp, y "194" ]
                                [ simpleKnobSvg cv.pan
                                ]
                        )
                )
    in
    svg [ Svg.height "615px", Svg.width "750px", Svg.fontFamily "Metropolis", textAnchor "middle", Svg.fontSize "14px" ]
        [ line [ x1 (sfi vvBegin), y1 "1px", x2 (sfi vhEnd), y2 "1px", Svg.stroke "black" ] []

        -- chennel numbers
        , g
            [ Svg.fontWeight "bold" ]
            (List.map (\i -> Svg.text_ [ dx (sfi (52 + vvOffset * i)), dy "28px" ] [ Svg.text (sfi (i+1)) ]) (List.range 0 7))

        , line [ x1 (sfi vvBegin), y1 "46px", x2 (sfi vhEnd), y2 "46px", Svg.stroke "black", Svg.strokeWidth "1px" ] []

        -- filter & pan
        , g [ textAnchor "start", Svg.fill "white" ]
            [ Svg.text_ [ dx (sfi vvBegin), dy "70px", Svg.fontWeight "600", Svg.fill theLightGrayString ] [ Svg.text "filter" ]
            , line [ x1 (sfi vvBegin), y1 "84px", x2 (sfi vhEnd), y2 "84px", Svg.stroke "white", Svg.strokeWidth "1px" ] []
            , filterKnobs
            , line [ x1 (sfi vvBegin), y1 "147px", x2 (sfi vhEnd), y2 "147px", Svg.stroke "white", Svg.strokeWidth "1px" ] []
            , Svg.text_ [ dx (sfi vvBegin), dy "168px", Svg.fill theLightGrayString, Svg.fontWeight "600" ] [ Svg.text "pan" ]
            , line [ x1 (sfi vvBegin), y1 "184px", x2 (sfi vhEnd), y2 "184px", Svg.stroke "white", Svg.strokeWidth "1px" ] []
            , panKnobs
            , line [ x1 (sfi vvBegin), y1 "247px", x2 (sfi vhEnd), y2 "247px", Svg.stroke "white", Svg.strokeWidth "1px" ] []
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
            (List.map (\i -> Svg.text_ [ dx "16px", dy (sfi (rsNumStart + vhOffset * i)) ] [ Svg.text (sfi (10 - i)) ]) (List.range 0 10))
        ]
