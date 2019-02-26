module HomePage exposing (page)

import AudioPlayer exposing (AudioModel)
import Css as Css exposing (..)
import Css.Global exposing (body, global)
import Events exposing (..)
import Html.Attributes.Extra as HAE
import Html.Styled exposing (..)
import Html.Styled.Attributes as HSA exposing (..)
import Html.Styled.Events exposing (onClick)
import Model exposing (Model)
import Msg exposing (Msg(..))
import Patch as P


page : Model -> Html Msg
page model =
    div []
        [ h1 []
            [ text "Home page"
            ]
        , patchesList model.audio (model.patches |> Maybe.withDefault [])
        ]


patchesList : AudioModel -> List P.Patch -> Html Msg
patchesList am ps =
    let
        patchItems =
            ps
                |> List.map
                    (\p -> patchItem am p)
    in
    div [ css [ Css.width (pct 66) ] ] patchItems


patchItem : AudioModel -> P.Patch -> Html Msg
patchItem am p =
    div []
        [ patchMeta p
        , patchMedia am p
        ]


patchMedia : AudioModel -> P.Patch -> Html Msg
patchMedia am p =
    div []
        [ button [ onClick (Play p) ] [ text "play" ]
        , img [ src p.waveformSmall ] []
        , audioNode am p
        ]


audioNode : AudioModel -> P.Patch -> Html Msg
audioNode model patch =
    audio
        [ id patch.title
        , src patch.soundUrl
        , HAE.volume model.volume |> HSA.fromUnstyled
        , onEnded (Ended patch) |> HSA.fromUnstyled
        , onTimeUpdate (TimeUpdate patch) |> HSA.fromUnstyled
        ]
        []



-- |> map (\amsg -> Msg.AudioPlayer amsg)


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
