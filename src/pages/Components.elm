module Components exposing (audioNode, cssHaxor, outputPanel, playButton, volumeInput, waveformSeeker)

import AudioModel exposing (..)
import Css as Css exposing (..)
import Events exposing (..)
import Html.Attributes.Extra as HAE
import Html.Styled as HS exposing (..)
import Html.Styled.Attributes as HSA exposing (..)
import Html.Styled.Events as HS exposing (..)
import Json.Decode as JD exposing (..)
import Json.Encode as JE
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
        , borderStyle (solid)
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
        , Css.checked [ backgroundImage (url "/mute.svg") ]
        , backgroundImage (url "/unmute.svg")
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
        , Css.height (px 46), Css.maxWidth (pct 50)
        ]


volumeInput : Bool -> Html Msg
volumeInput muted =
    div [ HSA.css [ volumeInputCss ] ]
        [ span [HSA.css [ marginRight auto ]] [ HS.text "volume" ]
        , input
            [ HSA.css [ volumeSliderCss ]
            , HSA.type_ "range"
            , onInput (String.toFloat >> Maybe.withDefault 0.5 >> VolumeChange)
            ]
            [ ]
        , input
            [ HSA.css [ muteButtonCss ]
            , HSA.type_ "checkbox"
            , HSA.checked (not muted)
            , HS.onClick (Mute (not muted))
            ]
            [ ]
        ]




waveformSeeker : Patch -> Html Msg
waveformSeeker patch =
    let
        seekerPosition =
            patch.audioModel
                |> Maybe.map
                    (\am ->
                        100 / (patch.duration / am.seekerPosition)
                    )

        seeker =
            if seekerPosition |> isJust then
                div
                    [ HSA.css
                        [ Css.height (pct 100)
                        , Css.width (px 1)
                        , backgroundColor (hex "ffffff")
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
        [ img [ src patch.waveformSmall, HSA.css [ Css.width (pct 100), Css.height (pct 100) ] ] []
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

        volume =
            if model.muted then
                0

            else
                model.volume
    in
    if playing then
        audio
            [ HSA.id patch.title
            , src patch.soundUrl
            , HAE.volume volume |> HSA.fromUnstyled
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
        h =
            if on then
                "1"

            else
                "15"
    in
    svg [ x "11" ]
        [ rect [ x "1", y "1", Svg.width "16", Svg.height "30", Svg.stroke "black", Svg.strokeWidth "2px", Svg.fill "none" ] []
        , rect [ x "0", y h, Svg.width "17", Svg.height "16", Svg.fill "black" ] []
        ]


chanHandle : Html msg
chanHandle =
    rect [ Svg.width "38", Svg.height "9", Svg.fill "black", Svg.strokeWidth "2px", Svg.stroke "#9b9b9b" ] []


outputPanel : List Knob -> Html msg
outputPanel knobs =
    let
        chans =
            g [ Svg.color "black" ]
                (knobs
                    |> List.indexedMap
                        (\i k ->
                            let
                                swch =
                                    chanSwitch (k.value |> isJust)

                                val =
                                    k.value |> Maybe.withDefault 0

                                xp =
                                    33 + (i * 92) |> String.fromInt

                                hy =
                                    44 + ((10 - val) * 22.7) |> String.fromFloat

                                handle =
                                    svg [ x "0", y hy ] [ chanHandle ]
                            in
                            svg [ x xp, y "61" ]
                                [ swch
                                , handle
                                ]
                        )
                )
    in
    svg [ Svg.height "380px", Svg.width "740px", Svg.fontFamily "Metropolis", textAnchor "middle", Svg.fontSize "14px" ]
        [ line [ x1 "33px", y1 "1px", x2 "713px", y2 "1px", Svg.stroke "black" ] []
        , line [ x1 "33px", y1 "45px", x2 "713px", y2 "45px", Svg.stroke "black", Svg.strokeWidth "1px" ] []
        , line [ x1 "33px", y1 "110px", x2 "713px", y2 "110px", Svg.stroke "white", Svg.strokeWidth "1px" ] []
        , line [ x1 "33px", y1 "133px", x2 "713px", y2 "133px", Svg.stroke "white", Svg.strokeWidth "1px" ] []
        , line [ x1 "33px", y1 "156px", x2 "713px", y2 "156px", Svg.stroke "white", Svg.strokeWidth "1px" ] []
        , line [ x1 "33px", y1 "179px", x2 "713px", y2 "179px", Svg.stroke "white", Svg.strokeWidth "1px" ] []
        , line [ x1 "33px", y1 "202px", x2 "713px", y2 "202px", Svg.stroke "white", Svg.strokeWidth "1px" ] []
        , line [ x1 "33px", y1 "225px", x2 "713px", y2 "225px", Svg.stroke "white", Svg.strokeWidth "1px" ] []
        , line [ x1 "33px", y1 "248px", x2 "713px", y2 "248px", Svg.stroke "white", Svg.strokeWidth "1px" ] []
        , line [ x1 "33px", y1 "271px", x2 "713px", y2 "271px", Svg.stroke "white", Svg.strokeWidth "1px" ] []
        , line [ x1 "33px", y1 "294px", x2 "713px", y2 "294px", Svg.stroke "white", Svg.strokeWidth "1px" ] []
        , line [ x1 "33px", y1 "317px", x2 "713px", y2 "317px", Svg.stroke "white", Svg.strokeWidth "1px" ] []
        , line [ x1 "33px", y1 "336px", x2 "713px", y2 "336px", Svg.stroke "white", Svg.strokeWidth "1px" ] []
        , line [ x1 "33px", y1 "365px", x2 "713px", y2 "365px", Svg.stroke "black", Svg.strokeWidth "1px" ] []
        , line [ x1 "52px", y1 "110px", x2 "52px", y2 "364px", Svg.stroke "white", Svg.strokeWidth "4px" ] []
        , line [ x1 "144px", y1 "110px", x2 "144px", y2 "364px", Svg.stroke "white", Svg.strokeWidth "4px" ] []
        , line [ x1 "236px", y1 "110px", x2 "236px", y2 "364px", Svg.stroke "white", Svg.strokeWidth "4px" ] []
        , line [ x1 "328px", y1 "110px", x2 "328px", y2 "364px", Svg.stroke "white", Svg.strokeWidth "4px" ] []
        , line [ x1 "420px", y1 "110px", x2 "420px", y2 "364px", Svg.stroke "white", Svg.strokeWidth "4px" ] []
        , line [ x1 "512px", y1 "110px", x2 "512px", y2 "364px", Svg.stroke "white", Svg.strokeWidth "4px" ] []
        , line [ x1 "604px", y1 "110px", x2 "604px", y2 "364px", Svg.stroke "white", Svg.strokeWidth "4px" ] []
        , line [ x1 "696px", y1 "110px", x2 "696px", y2 "364px", Svg.stroke "white", Svg.strokeWidth "4px" ] []
        , g
            [ Svg.fontWeight "bold" ]
            [ Svg.text_ [ dx "52px", dy "28px" ] [ Svg.text "1" ]
            , Svg.text_ [ dx "144px", dy "28px" ] [ Svg.text "2" ]
            , Svg.text_ [ dx "236px", dy "28px" ] [ Svg.text "3" ]
            , Svg.text_ [ dx "328px", dy "28px" ] [ Svg.text "4" ]
            , Svg.text_ [ dx "420px", dy "28px" ] [ Svg.text "5" ]
            , Svg.text_ [ dx "512px", dy "28px" ] [ Svg.text "6" ]
            , Svg.text_ [ dx "604px", dy "28px" ] [ Svg.text "7" ]
            , Svg.text_ [ dx "696px", dy "28px" ] [ Svg.text "8" ]
            ]
        , g
            [ Svg.fontWeight "500", Svg.fill "#4a4a4a" ]
            [ Svg.text_ [ dx "16px", dy "114px" ] [ Svg.text "10" ]
            , Svg.text_ [ dx "16px", dy "137px" ] [ Svg.text "9" ]
            , Svg.text_ [ dx "16px", dy "160px" ] [ Svg.text "8" ]
            , Svg.text_ [ dx "16px", dy "183px" ] [ Svg.text "7" ]
            , Svg.text_ [ dx "16px", dy "206px" ] [ Svg.text "6" ]
            , Svg.text_ [ dx "16px", dy "229px" ] [ Svg.text "5" ]
            , Svg.text_ [ dx "16px", dy "252px" ] [ Svg.text "4" ]
            , Svg.text_ [ dx "16px", dy "275px" ] [ Svg.text "3" ]
            , Svg.text_ [ dx "16px", dy "298px" ] [ Svg.text "2" ]
            , Svg.text_ [ dx "16px", dy "321px" ] [ Svg.text "1" ]
            , Svg.text_ [ dx "16px", dy "340px" ] [ Svg.text "0" ]
            , Svg.text_ [ dx "103px", dy "93px" ] [ Svg.text "off" ]
            , Svg.text_ [ dx "287px", dy "93px" ] [ Svg.text "off" ]
            , Svg.text_ [ dx "471px", dy "93px" ] [ Svg.text "off" ]
            , Svg.text_ [ dx "655px", dy "93px" ] [ Svg.text "off" ]
            ]
        , chans
        ]
