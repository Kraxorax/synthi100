module HomePage exposing (page)

import AudioModel exposing (AudioModel, noPlayingAudioModel)
import Css as Css exposing (..)
import Css.Global exposing (body, global)
import Events exposing (..)
import Html.Attributes.Extra as HAE
import Html.Styled exposing (..)
import Html.Styled.Attributes as HSA exposing (..)
import Html.Styled.Events exposing (on, onClick, onMouseDown, onMouseUp)
import Json.Decode as JD exposing (map)
import Maybe.Extra exposing (isJust)
import Model exposing (Model)
import Mouse exposing (..)
import Msg exposing (Msg(..))
import Patch as P


page : Model -> Html Msg
page model =
    div []
        [ h1 []
            [ text "Home page"
            ]
        , patchesList (model.patches |> Maybe.withDefault [])
        ]


patchesList : List P.Patch -> Html Msg
patchesList ps =
    let
        patchItems =
            ps
                |> List.map
                    (\p -> patchItem p)
    in
    div [ css [ Css.width (pct 66) ] ] patchItems


patchItem : P.Patch -> Html Msg
patchItem patch =
    div [ css [ display block, Css.height (px 90) ] ]
        [ patchMeta patch
        , patchMedia patch
        ]


patchMedia : P.Patch -> Html Msg
patchMedia patch =
    let
        audioModel =
            case patch.audioModel of
                Just audioM ->
                    audioM

                Nothing ->
                    noPlayingAudioModel

        seekerPosition =
            case patch.audioModel of
                Just am ->
                    Just <| 100 / (patch.duration / audioModel.seekerPosition)

                Nothing ->
                    Nothing

        ( hndlClck, bttnText ) =
            if audioModel.playing then
                ( Pause patch, "pause" )

            else
                ( Play patch, "play" )
    in
    div [ patchMediaCss ]
        [ button
            [ onClick hndlClck
            , css [ Css.width (px 60), Css.height (px 60), float left ]
            ]
            [ text bttnText ]
        , waveformSeeker patch seekerPosition
        , audioNode audioModel patch
        ]


patchMediaCss =
    css [ display block, Css.height (pct 100), float left ]


audioNode : AudioModel -> P.Patch -> Html Msg
audioNode model patch =
    if model.playing then
        audio
            [ id patch.title
            , src patch.soundUrl
            , HAE.volume model.volume |> HSA.fromUnstyled
            , onEnded (Ended patch) |> HSA.fromUnstyled
            , onTimeUpdate (TimeUpdate patch) |> HSA.fromUnstyled
            ]
            []

    else
        audio
            [ id patch.title
            , src patch.soundUrl
            ]
            []


waveformSeeker : P.Patch -> Maybe Float -> Html Msg
waveformSeeker patch seekPos =
    let
        seeker =
            if seekPos |> isJust then
                div
                    [ css
                        [ Css.height (pct 100)
                        , Css.width (px 1)
                        , backgroundColor (hex "ffffff")
                        , Css.position absolute
                        , top (px 0)
                        , left (pct (seekPos |> Maybe.withDefault 0))
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
            [ Css.width (px 320)
            , Css.height (px 60)
            , Css.position relative
            , float left
            ]
        ]
        [ img [ src patch.waveformSmall ] []
        , seeker
        ]


patchMeta : P.Patch -> Html Msg
patchMeta p =
    let
        durationText =
            "duration: " ++ (p.duration |> String.fromFloat) ++ " sec"

        attribs =
            p.attributeValues |> String.join " / "
    in
    div [ css [ Css.width (pct 50), Css.height (pct 100), display block, float left ] ]
        [ div [ css [ Css.width (pct 33), float left ] ]
            [ text p.title ]
        , div [ css [ Css.width (pct 66), float left ] ]
            [ text durationText
            , br [] []
            , text attribs
            ]
        ]
