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
    div []
        [ div
            [ css
                [ color (hex "ffffff")
                , fontSize (px 36)
                , paddingBottom (px 14)
                , marginLeft (px 31)
                , borderBottom3 (px 1) solid theBlue
                , fontWeight bold
                ]
            ]
            [ text "DATABASE" ]
        , div [ css [ displayFlex ] ]
            [ div [ css [ flex (num 1) ] ] [ filterList model ]
            , div [ css [ flex (num 2) ] ] [ patchesList model ]
            ]
        ]


filterHeaderStyle : Style
filterHeaderStyle =
    batch
        [ padding2 (px 12.5) (px 0)
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
        , div [ css [ marginBottom (px 20) ] ]
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
    div [ css [ lastChild [ borderBottom3 (px 1) solid theBlue ] ] ]
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
        patchItems =
            model.filteredPatches
                |> List.map
                    (\p -> patchItem model p)
    in
    div
        [ css
            [ letterSpacing (px 0.5)
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
        , Css.maxWidth (pct 48)
        ]


sortingArrowCss : Style
sortingArrowCss =
    batch
        [ Css.property "-webkit-appearance" "none"
        , Css.property "-moz-appearance" "none"
        , backgroundImage (url "/sort_arrow_down.svg")
        , backgroundColor transparent
        , backgroundRepeat noRepeat
        , Css.width (px 33)
        , Css.margin4 (px 0) (px 0) (px 0) (px 0)
        , Css.minWidth (px 33)
        , Css.height (px 17)
        , outline none
        , margin2 (px 0) (px 4)
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
        , margin4 (px 0) (px 60) (px 0) (px 0)
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
        , alignItems left
        , flex (num 1)
        , paddingLeft (px 0)
        , textDecoration none
        , color inherit
        , justifyContent spaceBetween
        ]


patchTitleCss : Color -> Style
patchTitleCss titleColor =
    batch
        [ flex (num 1)
        , minWidth (px 150)
        , whiteSpace noWrap
        , letterSpacing (px 0)
        , marginRight (px 10)
        , color titleColor
        , padding2 (px 8) (px 0)
        ]


patchMediaCss =
    batch
        [ displayFlex
        , flex (num 1)
        , flexDirection column
        , paddingRight (px 14)
        ]


patchInfoCss =
    batch
        [ flex (num 1)
        , fontSize (px 14)
        , displayFlex
        , flexDirection column
        , alignItems center
        ]


patchItemCss : Style
patchItemCss =
    batch
        [ displayFlex
        , color (hex "ffffff")
        , fontWeight bold
        , borderTop2 (px 1) solid
        , borderColor (hex "ffffff")
        , lastChild
            [ borderBottom2 (px 1) solid
            ]
        ]


patchItem : Model -> P.Patch -> Html Msg
patchItem model patch =
    div [ css [ patchItemCss ] ]
        [ patchMedia model patch
        , patchMeta patch
        ]


patchMedia : Model -> P.Patch -> Html Msg
patchMedia model patch =
    let
        am =
            patch.audioModel |> Maybe.withDefault noPlayingAudioModel

        titleColor =
            if am.playing then
                hex "ffffff"

            else
                theBlue

        patchUrl =
            "patch/" ++ patch.title
    in
    div [ css [ patchMediaCss ] ]
        [ div [ css [ Css.height (px 36), displayFlex, patchTitleCss titleColor, alignItems center, borderBottom3 (px 1) solid (hex "333"), flexDirection row ] ]
            [ div [ css [ flex (num 5), color (hex "ffffff") ] ]
                [ a [ href patchUrl, css [ linkUnstyle ] ] [ text patch.title ] ]
            , div [ css [ color (hex "ffffff"), fontSize (px 14) ] ]
                [ text (P.durationToTime patch.duration) ]
            ]
        , div
            [ css [ Css.height (px 72), displayFlex, alignItems center, flexDirection row ] ]
            [ div
                [ css [] ]
                [ playButton patch 48 ]
            , div
                [ css [ flex (num 5), Css.height (pct 100) ] ]
                [ waveformSeeker False patch ]
            , audioNode model patch
            ]
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
    div [ css [ patchMetaCss, patchInfoCss ] ]
        [ div [ css [ Css.height (px 36), displayFlex, alignItems left, Css.width (pct 100), borderBottom3 (px 1) solid (hex "333") ] ]
            [ div [ css [ lineHeight (num 1.2), paddingTop (px 8) ] ]
                [ text attribs ]
            ]
        , div [ css [ displayFlex, alignItems center, flex (num 1), Css.width (pct 100), marginTop (px 0) ] ]
            [ a [ href p.download, css [ linkUnstyle, flex (num 1), displayFlex, Css.width (pct 100), alignItems center, cursor pointer ] ]
                [ downBttn "#9b9b9b"
                , span [ css [ marginLeft (px 4) ] ] [ text "download" ]
                ]
            , a [ href patchUrl, css [ linkUnstyle, flex (num 1), displayFlex ] ]
                [ rightBttn "#9b9b9b"
                , span [ css [ marginLeft (px 4), marginTop (px 2) ] ] [ text "score" ]
                ]
            ]
        ]
