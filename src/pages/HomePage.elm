module HomePage exposing (page)

import Css exposing (..)
import Css.Global exposing (body, global)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, src)
import Model exposing (Model)
import Msg exposing (Msg)
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
    div [ css [ width (pct 66) ] ] patchItems


patchItem : P.Patch -> Html Msg
patchItem p =
    div []
        [ patchMeta p
        , patchMedia p
        ]


patchMedia : P.Patch -> Html Msg
patchMedia p =
    img [ src p.waveformSmall ] []


patchMeta : P.Patch -> Html Msg
patchMeta p =
    let
        durationText =
            "duration: " ++ (p.duration |> String.fromFloat) ++ " sec"

        attribs =
            p.attributeValues |> String.join " / "
    in
    div [ css [ width (pct 50) ] ]
        [ div [ css [ width (pct 33), float left ] ]
            [ text p.title ]
        , div [ css [ width (pct 66), float left ] ]
            [ text durationText
            , br [] []
            , text attribs
            ]
        ]
