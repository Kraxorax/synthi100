module Components exposing (audioNode, cssHaxor, playButton, volumeInput, waveformSeeker)

import AudioModel exposing (..)
import Css as Css exposing (..)
import Events exposing (..)
import Html.Attributes.Extra as HAE
import Html.Styled exposing (..)
import Html.Styled.Attributes as HSA exposing (..)
import Html.Styled.Events exposing (..)
import Json.Decode as JD exposing (..)
import Json.Encode as JE
import Maybe.Extra exposing (isJust)
import Model exposing (..)
import Mouse exposing (..)
import Msg exposing (..)
import Patch exposing (..)
import Styles exposing (theBlue)


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
        , fontWeight bold
        , fontSize (px 20)
        , borderTop2 (px 1) solid
        , Css.height (px 46), Css.maxWidth (pct 50)
        ]


volumeInput : Bool -> Html Msg
volumeInput muted =
    div [ css [ volumeInputCss ] ]
        [ span [css [ marginRight auto ]] [ text "volume" ]
        , input
            [ css [ volumeSliderCss ]
            , type_ "range"
            , onInput (String.toFloat >> Maybe.withDefault 0.5 >> VolumeChange)
            ]
            [ ]
        , input
            [ css [ muteButtonCss ]
            , type_ "checkbox"
            , HSA.checked (not muted)
            , onClick (Mute (not muted))
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
                    [ css
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
        [ on "click" (JD.map (Seek patch) mouseDecoder)
        , css
            [ Css.width (pct 100)
            , Css.height (pct 100)
            , Css.position relative
            , Css.float left
            ]
        ]
        [ img [ src patch.waveformSmall, css [ Css.width (pct 100), Css.height (pct 100) ] ] []
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
            [ id patch.title
            , src patch.soundUrl
            , HAE.volume volume |> HSA.fromUnstyled
            , onEnded (Ended patch) |> HSA.fromUnstyled
            , onTimeUpdate (TimeUpdate patch) |> HSA.fromUnstyled
            ]
            []

    else
        audio
            [ id patch.title
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
    img [ src bttnUrl, onClick hndlClck ] []


cssHaxor : Html msg
cssHaxor =
    div [ css [ Css.property "clear" "both" ] ] []
