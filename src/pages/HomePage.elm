module HomePage exposing (page)

import AudioModel exposing (AudioModel, noPlayingAudioModel)
import Components exposing (..)
import Css as Css exposing (..)
import Css.Global exposing (body, global)
import Events exposing (..)
import Html.Attributes.Extra as HAE
import Html.Styled exposing (..)
import Html.Styled.Attributes as HSA exposing (..)
import Html.Styled.Events exposing (on, onClick, onInput, onMouseDown, onMouseUp)
import Json.Decode as JD exposing (map)
import List.Extra exposing (find, findIndex)
import Maybe.Extra exposing (isJust)
import Model exposing (..)
import Mouse exposing (..)
import Msg exposing (Msg(..))
import Patch as P
import Styles exposing (..)
import SynthiSchema exposing (Attribute)


page : Model -> Html Msg
page model =
    div [ css [ Css.paddingLeft (px 31) ] ]
        [ filterList model
        , patchesList model
        ]


filterHeaderStyle : Style
filterHeaderStyle =
    batch
        [ borderTop2 (px 1) solid
        , padding2 (px 10) (px 0)
        , fontSize (px 20)
        , fontWeight bold
        , letterSpacing (px 0.5)
        ]


filterList : Model -> Html Msg
filterList model =
    let
        attrs =
            case model.synthiSchema of
                Just ss ->
                    ss.attributes

                Nothing ->
                    []
    in
    div [ css [ Css.width (pct 33), float left, maxWidth (px 390) ] ]
        [ div [ css [ filterHeaderStyle ] ]
            [ text "filters" ]
        , div []
            (attrs
                |> List.map
                    (\a ->
                        filterGroup
                            ((model.attributeFilters |> find (\f -> a.name == f.attrName))
                                |> Maybe.withDefault (AttrFilter "" [])
                            )
                            a
                    )
            )
        ]


filterSubheaderStyle : Style
filterSubheaderStyle =
    batch
        [ borderTop3 (px 1) solid theBlue
        , borderBottom3 (px 1) solid theBlue
        , padding2 (px 10) (px 20)
        , color (hex "fff")
        , fontSize (px 20)
        , fontWeight (int 500)
        ]


filterGroup : AttrFilter -> Attribute -> Html Msg
filterGroup filter attr =
    div []
        [ div [ css [ filterSubheaderStyle ] ]
            [ text attr.name ]
        , ul [ css [ Css.listStyle none, paddingLeft (px 10) ] ]
            [ div []
                (attr.values
                    |> List.map
                        (\v ->
                            li [ css [ Css.margin (px 10) ] ]
                                [ radio v attr.name (filter.selected |> findIndex (\s -> s == v) |> isJust) ]
                        )
                )
            ]
        ]


radio : String -> String -> Bool -> Html Msg
radio val group isChecked =
    label []
        [ xBox isChecked
        , input [ type_ "checkbox", id val, onInput (Filter group), HSA.checked isChecked, value val, css [ Css.display none ] ] []
        , span [ css [ color (hex "fff"), fontSize (px 14), paddingLeft (px 12) ] ] [ text val ]
        ]


patchesList : Model -> Html Msg
patchesList model =
    let
        ps =
            model.patches |> Maybe.withDefault []

        fltrs =
            model.attributeFilters |> List.concatMap (\f -> f.selected)

        patchItems =
            ps
                |> List.filter
                    (\p ->
                        p.attributeValues
                            |> List.all
                                (\av ->
                                    fltrs
                                        |> findIndex (\f -> f == av)
                                        |> isJust
                                )
                    )
                |> List.map
                    (\p -> patchItem model p)
    in
    div [ css [ Css.width (pct 66), float left ] ]
        (sortInput
            :: volumeInput
            :: patchItems
        )


sortInput : Html Msg
sortInput =
    div []
        [ span [] [ text "sort by" ]
        , select [ onInput SortBy ]
            [ option [ value "duration" ] [ text "duration" ]
            , option [ value "title" ] [ text "title" ]
            ]
        , button [ onClick (Sort Ascending) ] [ text "asc" ]
        , button [ onClick (Sort Descending) ] [ text "desc" ]
        ]


patchItem : Model -> P.Patch -> Html Msg
patchItem model patch =
    div [ css [ display block, Css.height (px 90) ] ]
        [ patchMeta patch
        , patchMedia model patch
        ]


patchMedia : Model -> P.Patch -> Html Msg
patchMedia model patch =
    div [ patchMediaCss ]
        [ playButton patch
        , div [ css [ Css.width (px 320), Css.height (px 90) ] ]
            [ waveformSeeker patch ]
        , audioNode model patch
        ]


patchMediaCss =
    css [ display block, Css.height (pct 100), float left ]


patchMeta : P.Patch -> Html Msg
patchMeta p =
    let
        durationText =
            "duration: " ++ (p.duration |> String.fromFloat) ++ " sec"

        attribs =
            p.attributeValues |> String.join " / "

        patchUrl =
            "patch/" ++ p.title
    in
    a [ href patchUrl, css [ Css.width (pct 50), Css.height (pct 100), display block, float left ] ]
        [ div [ css [ Css.width (pct 33), float left ] ]
            [ text p.title ]
        , div [ css [ Css.width (pct 66), float left ] ]
            [ text durationText
            , br [] []
            , text attribs
            ]
        ]
