module HomePage exposing (page)

import AudioPlayer exposing (AudioModel, noPlayingAudioModel)
import Css as Css exposing (..)
import Css.Global exposing (body, global)
import Events exposing (..)
import Html.Attributes.Extra as HAE
import Html.Styled exposing (..)
import Html.Styled.Attributes as HSA exposing (..)
import Html.Styled.Events exposing (onClick)
import Maybe.Extra exposing (isJust)
import Model exposing (Model)
import Msg exposing (Msg(..))
import Patch as P


page : Model -> Html Msg
page model =
    div []
        [ h1 []
            [ text "Home page"
            ]
        , patchesList model.nowPlaying (model.patches |> Maybe.withDefault [])
        ]


patchesList : Maybe ( P.Patch, AudioModel ) -> List P.Patch -> Html Msg
patchesList nowPlaying ps =
    let
        patchItems =
            ps
                |> List.map
                    (\p ->
                        case nowPlaying of
                            Just ( ap, am ) ->
                                let
                                    isPlaying =
                                        p.title == ap.title

                                    audioModel =
                                        if isPlaying then
                                            Just am

                                        else
                                            Nothing
                                in
                                patchItem audioModel p

                            Nothing ->
                                patchItem Nothing p
                    )
    in
    div [ css [ Css.width (pct 66) ] ] patchItems


patchItem : Maybe AudioModel -> P.Patch -> Html Msg
patchItem am p =
    div []
        [ patchMeta p
        , patchMedia am p
        ]


patchMedia : Maybe AudioModel -> P.Patch -> Html Msg
patchMedia am p =
    let
        audioModel =
            case am of
                Just audioM ->
                    audioM

                Nothing ->
                    noPlayingAudioModel

        playing =
            (am |> isJust) && audioModel.playing
    in
    if playing then
        div []
            [ button [ onClick (Pause p) ] [ text "pause" ]
            , span [] [ text (audioModel.seekerPosition |> String.fromFloat) ]
            , img [ src p.waveformSmall ] []
            , audioNode audioModel p
            ]

    else
        div []
            [ button [ onClick (Play p) ] [ text "play" ]
            , span [] [ text (audioModel.seekerPosition |> String.fromFloat) ]
            , img [ src p.waveformSmall ] []
            , audioNode audioModel p
            ]


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


patchMeta : P.Patch -> Html Msg
patchMeta p =
    let
        durationText =
            "duration: " ++ (p.duration |> String.fromFloat) ++ " sec"

        attribs =
            p.attributeValues |> String.join " / "
    in
    div [ css [ Css.width (pct 50) ] ]
        [ div [ css [ Css.width (pct 33), float left ] ]
            [ text p.title ]
        , div [ css [ Css.width (pct 66), float left ] ]
            [ text durationText
            , br [] []
            , text attribs
            ]
        ]
