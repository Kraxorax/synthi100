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
        [ div [ css [ flex (num 1)]] [filterList model]
        , div [css  [ flex (num 2)]] [patchesList model]
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
    div [ css
            [ marginTop (px 18)
            , letterSpacing (px 0.5)
            , fontSize (px 20)
            ]
        ]
        (sortInput
            :: volumeInput model.muted
            :: patchItems
        )


sortingArrowCss : Style
sortingArrowCss =
    batch
        [ Css.property "-webkit-appearance" "none"
        , Css.property "-moz-appearance" "none"
        , backgroundImage (url "sort_arrow_down.svg")
        , backgroundColor transparent
        , backgroundRepeat noRepeat
        , Css.width (px 33)
        , Css.margin4 (px 0) (px 6) (px 0) (px 6)
        , Css.minWidth (px 33)
        , Css.height (px 17)
        , outline none
        , Css.checked [backgroundImage (url "sort_arrow_down_selected.svg")]
        ]


sortingSelectCss : Style
sortingSelectCss =
    batch
        [ Css.property "-moz-appearance" "none"
        , Css.property "-webkit-appearance" "none"
        , border3 (px 1) solid (hex "ffffff")
        , backgroundImage (url "select_arrow_down.svg")
        , backgroundColor (hex "000000")
        , Css.color (hex "#ffffff")
        , backgroundRepeat noRepeat
        , Css.property "background-position" "right 5px center"
        , Css.fontFamily inherit
        , Css.height (px 23)
        , paddingLeft (px 6)
        , margin4 (px 0) (px 12) (px 0) (px 12)
        , Css.letterSpacing (px 0.5)
        , Css.width  (px 116)
        , Css.minWidth  (px 88)
        , Css.fontSize (px 14)
        ]


sortInput : Html Msg
sortInput =
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
        , select [ onInput SortBy, css [sortingSelectCss] ]
            [ option [ value "duration" ] [ text "duration" ]
            , option [ value "title" ] [ text "title" ]
            ]
        , input [ type_ "radio"
                , name "sort"
                , onClick (Sort Ascending)
                , css [ sortingArrowCss ]
                ]
                []
        , input [ type_ "radio"
                , name "sort"
                , onClick (Sort Descending)
                , css [ sortingArrowCss
                      , Css.property "-webkit-transform" "rotate(180deg)"
                      ]
                ]
                []
        ]


patchMetaCss =
    batch
        [ displayFlex
        , flex (num 1.06)
        , textDecoration none
        , color inherit
        , alignItems baseline
        , justifyContent spaceBetween
        ]


patchTitleCss =
    batch
        [ flex (num 1)
        , whiteSpace noWrap
        , lineHeight (px 62)
        , letterSpacing (px 0)
        , marginRight (px 10)
        ]


patchMediaCss =
    batch
        [ displayFlex
        , flex (num 1)
        , alignItems center
        ]


patchInfoCss =
    batch
        [ flex (num 2)
        , marginRight (px 19)
        , fontSize (px 14)
        , displayFlex
        , maxWidth (px 275)
        , flexDirection (column)
        ]


patchItemCss : Style
patchItemCss =
    batch
        [ displayFlex
        , color (hex "ffffff")
        , fontWeight bold
        , borderTop2 (px 1) solid
        , marginRight (pct 3.125)
        , borderColor theBlue
        ]


patchItem : Model -> P.Patch -> Html Msg
patchItem model patch =
    div [ css [ patchItemCss ] ]
        [ patchMeta patch
        , patchMedia model patch
        ]


patchMedia : Model -> P.Patch -> Html Msg
patchMedia model patch =
    div [ css [ patchMediaCss ] ]
        [ div
            [ css [ Css.width (px 64), Css.height (px 64) ] ]
            [ playButton patch ]
        , div
            [ css [ Css.width auto, Css.height (px 90), marginLeft (px 9) ] ]
            [ waveformSeeker patch ]
        , audioNode model patch
        ]


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
    a [ href patchUrl, css [ patchMetaCss ] ]
        [ div [ css  [ patchTitleCss ] ]
            [ text p.title ]
        , div [ css [ patchInfoCss ] ]
            [ div [ css [lineHeight (num 1.2), marginBottom (px 7) ] ] [ text durationText ]
            , div [ css [lineHeight (num 1.15), marginBottom (px 10) ] ] [ text attribs ]
            ]
        ]
