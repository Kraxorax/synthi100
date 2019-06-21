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
    div [ css [ displayFlex ] ]
        [ div [ css [ flex (num 1) ] ] [ filterList model ]
        , div [ css [ flex (num 2) ] ] [ patchesList model ]
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
    div [ css [ paddingLeft (px 31), maxWidth (px 390) ] ]
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
                |> filterPatches model.attributeFilters
                |> List.map
                    (\p -> patchItem model p)
    in
    div
        [ css
            [ marginTop (px 18)
            , letterSpacing (px 0.5)
            , fontSize (px 20)
            ]
        ]
        (sortInput model
            :: volume (model.volume == 0)
            :: patchItems
        )


volume : Bool -> Html Msg
volume muted =
    div [ HSA.css [ volumeInputCss ] ]
        [ div [ HSA.css [ flex (num 1) ] ] [ text "volume" ]
        , div [ HSA.css [ displayFlex, flex (num 1) ] ]
            [ div [ HSA.css [ marginRight (px 16) ] ] [ volumeIcon muted ]
            , div [ HSA.css [ paddingTop (px 4) ] ] [ volumeSlider ]
            ]
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


filterPatches : List AttrFilter -> List P.Patch -> List P.Patch
filterPatches afs ps =
    ps
        |> List.filter
            (\p ->
                let
                    selTypes =
                        afs |> List.Extra.find (\a -> a.attrName == "type") |> Maybe.withDefault { attrName = "", selected = [] }

                    selQuality =
                        afs |> List.Extra.find (\a -> a.attrName == "quality") |> Maybe.withDefault { attrName = "", selected = [] }

                    selRange =
                        afs |> List.Extra.find (\a -> a.attrName == "range") |> Maybe.withDefault { attrName = "", selected = [] }

                    selComplexity =
                        afs |> List.Extra.find (\a -> a.attrName == "complexity") |> Maybe.withDefault { attrName = "", selected = [] }

                    passType =
                        (selTypes.selected |> List.length)
                            == 0
                            || (selTypes.selected |> List.Extra.findIndex (\s -> s == p.attributes.type_) |> Maybe.map (\i -> i >= 0) |> Maybe.withDefault False)

                    passQuality =
                        (selQuality.selected |> List.length)
                            == 0
                            || (selQuality.selected |> List.Extra.findIndex (\s -> s == p.attributes.quality) |> Maybe.map (\i -> i >= 0) |> Maybe.withDefault False)

                    passRange =
                        (selRange.selected |> List.length)
                            == 0
                            || (selRange.selected |> List.Extra.findIndex (\s -> s == p.attributes.range) |> Maybe.map (\i -> i >= 0) |> Maybe.withDefault False)

                    passComplexity =
                        (selComplexity.selected |> List.length)
                            == 0
                            || (selComplexity.selected |> List.Extra.findIndex (\s -> s == p.attributes.complexity) |> Maybe.map (\i -> i >= 0) |> Maybe.withDefault False)
                in
                passType && passQuality && passRange && passComplexity
            )


sortingArrowCss : Style
sortingArrowCss =
    batch
        [ Css.property "-webkit-appearance" "none"
        , Css.property "-moz-appearance" "none"
        , backgroundImage (url "/sort_arrow_down.svg")
        , backgroundColor transparent
        , backgroundRepeat noRepeat
        , Css.width (px 33)
        , Css.margin4 (px 0) (px 6) (px 0) (px 6)
        , Css.minWidth (px 33)
        , Css.height (px 17)
        , outline none
        , Css.checked [ backgroundImage (url "/sort_arrow_down_selected.svg") ]
        ]


sortingSelectCss : Style
sortingSelectCss =
    batch
        [ Css.property "-moz-appearance" "none"
        , Css.property "-webkit-appearance" "none"
        , border3 (px 1) solid (hex "ffffff")
        , backgroundImage (url "/select_arrow_down.svg")
        , backgroundColor (hex "000000")
        , Css.color (hex "#ffffff")
        , backgroundRepeat noRepeat
        , Css.property "background-position" "right 5px center"
        , Css.fontFamily inherit
        , Css.height (px 23)
        , paddingLeft (px 6)
        , margin4 (px 0) (px 12) (px 0) (px 12)
        , Css.letterSpacing (px 0.5)
        , Css.width (px 116)
        , Css.minWidth (px 88)
        , Css.fontSize (px 14)
        ]


sortInput : Model -> Html Msg
sortInput model =
    let
        sortedBy s =
            model.sortBy == s

        isAscending =
            model.sortOrder == Ascending
    in
    div
        [ css
            [ displayFlex
            , alignItems center
            , fontWeight bold
            , borderTop2 (px 1) solid
            , fontSize (px 20)
            , maxWidth (pct 50)
            , Css.height (px 46)
            , justifyContent flexEnd
            , boxSizing borderBox
            , padding4 (px 0) (pct 8) (px 4) (px 0)
            ]
        ]
        [ span [ css [ marginRight auto, whiteSpace noWrap ] ] [ text "sort by" ]
        , select [ onInput SortBy, css [ sortingSelectCss ] ]
            [ option [ value "duration", selected (sortedBy "duration") ] [ text "duration" ]
            , option [ value "title", selected (sortedBy "title") ] [ text "title" ]
            ]
        , input
            [ type_ "radio"
            , name "sort"
            , HSA.checked isAscending
            , onClick (Sort Ascending)
            , css [ sortingArrowCss ]
            ]
            []
        , input
            [ type_ "radio"
            , name "sort"
            , onClick (Sort Descending)
            , HSA.checked (not isAscending)
            , css
                [ sortingArrowCss
                , Css.property "-webkit-transform" "rotate(180deg)"
                ]
            ]
            []
        ]


patchMetaCss =
    batch
        [ displayFlex
        , flex (num 1)
        , paddingLeft (px 14)
        , textDecoration none
        , color inherit
        , alignItems baseline
        , justifyContent spaceBetween
        ]


patchTitleCss =
    batch
        [ flex (num 1)
        , minWidth (px 150)
        , whiteSpace noWrap
        , letterSpacing (px 0)
        , marginRight (px 10)
        , Css.height (pct 100)
        , marginTop (px 32)
        ]


patchMediaCss =
    batch
        [ displayFlex
        , flex (num 1)
        , paddingRight (px 14)
        , alignItems center
        ]


patchInfoCss =
    batch
        [ flex (num 2)
        , fontSize (px 14)
        , displayFlex
        , flexDirection column
        ]


patchItemCss : Style
patchItemCss =
    batch
        [ displayFlex
        , color (hex "ffffff")
        , fontWeight bold
        , borderTop2 (px 1) solid
        , borderColor (hex "ffffff")
        ]


patchItem : Model -> P.Patch -> Html Msg
patchItem model patch =
    div [ css [ patchItemCss ] ]
        [ patchMedia model patch
        , patchMeta patch
        ]


patchMedia : Model -> P.Patch -> Html Msg
patchMedia model patch =
    div [ css [ patchMediaCss ] ]
        [ div [ css [ patchTitleCss ] ]
            [ text patch.title ]
        , div
            [ css [ Css.width (px 64), Css.height (px 64) ] ]
            [ playButton patch ]
        , div
            [ css [ Css.width auto, Css.height (px 90), marginLeft (px 9) ] ]
            [ waveformSeeker False patch ]
        , audioNode model patch
        ]


patchMeta : P.Patch -> Html Msg
patchMeta p =
    let
        durationText =
            P.durationToTime p.duration

        attribs =
            p.attributeValues |> String.join " / "

        patchUrl =
            "patch/" ++ p.title
    in
    div [ css [ patchMetaCss ] ]
        [ div [ css [ patchInfoCss ] ]
            [ div [ css [ displayFlex, marginTop (px 8), borderBottom3 (px 1) solid (hex "333") ] ]
                [ div [ css [ lineHeight (num 1.2), minWidth (px 45), margin4 (px 0) (px 10) (px 7) (px 0), padding4 (px 5) (px 10) (px 0) (px 0), borderRight3 (px 2) solid theBlue ] ]
                    [ text durationText ]
                , div [ css [ lineHeight (num 1.2), marginBottom (px 10), paddingTop (px 5) ] ]
                    [ text attribs ]
                ]
            , div [ css [ displayFlex, marginTop (px 13) ] ]
                [ div [ css [ flex (num 1), displayFlex, alignItems center, cursor pointer ] ]
                    [ downBttn "#9b9b9b", span [ css [ marginLeft (px 10) ] ] [ text "download audio" ] ]
                , a [ href patchUrl, css [ linkUnstyle, flex (num 1), displayFlex, alignItems center ] ]
                    [ rightBttn "#9b9b9b", span [ css [ marginLeft (px 10) ] ] [ text "score" ] ]
                ]
            ]
        ]
