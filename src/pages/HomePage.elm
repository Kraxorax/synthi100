module HomePage exposing (page)

import AudioModel exposing (AudioModel, noPlayingAudioModel)
import Components exposing (..)
import Css as Css exposing (..)
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
    div
        [ css [ padding2 (px 0) (px 10) ] ]
        [ div [ ]
            [  div
                [ css
                    [ Css.height (px 46)
                    , marginLeft (px 30)
                    , marginRight (px 10)
                    , borderBottom3 (px 1) solid theBlue
                    , fontSize (px 36)
                    , letterSpacing (px 1)
                    , fontWeight bold
                    ]
                ]
                [ text "DATABASE"  ]
            ]
            , div
                [ css [ displayFlex ] ]
                [ div
                    [ css
                        [ padding2 (px 0) (px 20)
                        , borderStyle solid
                        , borderColor (hex "#000")
                        , boxSizing borderBox
                        , flex (pct (100/3.0))
                        , borderWidth2 (px 0) (px 10)
                        ]
                    ]
                    [ filterList model ]
               , div
                   [ css [ borderStyle solid
                         , borderColor (hex "#000000")
                         , boxSizing borderBox
                         , flex (pct (200/3.0))
                         , borderWidth2 (px 0) (px 10)
                         ]
                   ]
                   (patchesList model)
              ]
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
    section []
        [ h2 [] [ text "filters" ]
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
        , color (hex "fff")
        , fontSize (px 18)
        , Css.height (px 42)
        , alignItems center
        , letterSpacing (px 0.5)
        , displayFlex
        , paddingLeft (px 17)
        ]


filterGroup : AttrFilter -> Attribute -> Html Msg
filterGroup filter attr =
    div [ css [ lastChild [ borderBottom3 (px 1) solid theBlue ] ] ]
        [ div [ css [ filterSubheaderStyle ] ]
            [ text attr.name ]
        , ul [ css [ Css.listStyle none, paddingLeft (px 0) ] ]
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
        , input
            [ type_ "checkbox"
            , id val
            , onInput (Filter group)
            , HSA.checked isChecked
            , value val
            , css [ Css.display none ]
            ]
            []
        , span
            [ css
                [ letterSpacing (px 0.5)
                , color (hex "fff")
                , fontSize (px 14)
                , marginLeft (px 20)
                ]
            ]
            [ text val ]
        ]


patchesList : Model -> List (Html Msg)
patchesList model =
    let
        patchItems =
            model.filteredPatches
                |> List.map
                    (\p -> patchItem model p)
    in sortInput model ::
       volume model ::
       patchItems


volume : Model -> Html Msg
volume model =
    div [ css [ volumeInputCss ] ]
        [ label
            [ for "volumeSlider"
            , css [ labelCss ]
            ]
            [ text "volume" ]
        , div
            [ css [ displayFlex
                  , alignItems center
                  , flex (pct 60)
                  , paddingLeft (px 5)
                  ]
            ]
            [ div
                [ css [ Css.width (px 25)
                      , Css.height (px 22)
                      ]
                ]
                [ volumeIcon model ]
            , div
                [ css [ marginLeft (px 12)
                      , marginBottom (px 10)
                      ]
                ]
                [ volumeSlider ]
            ]
        ]


volumeInputCss : Style
volumeInputCss =
    batch
        [ boxSizing borderBox
        , displayFlex
        , justifyContent flexEnd
        , alignItems center
        , Css.fontWeight bold
        , Css.fontSize (px 20)
        , borderTop2 (px 1) solid
        , Css.height (px 40)
        , Css.width (pct (100*400 / 820))
        ]


sortingArrowCss : AssetFlags -> Style
sortingArrowCss assets =
    batch
        [ Css.property "-webkit-appearance" "none"
        , Css.property "-moz-appearance" "none"
        , backgroundImage (url assets.svg.sortArrowDown)
        , backgroundColor (hex "00000000")
        , backgroundColor transparent
        , backgroundRepeat noRepeat
        , Css.width (px 25)
        , Css.margin4 (px 0) (px 0) (px 0) (px 0)
        , Css.minWidth (px 25)
        , Css.height (px 13)
        , outline none
        , margin2 (px 0) (px 4)
        , Css.checked [ backgroundImage (url assets.svg.sortArrowDownSelected ) ]
        ]


sortingSelectCss : AssetFlags -> Style
sortingSelectCss assets =
    batch
        [ Css.property "-moz-appearance" "none"
        , Css.property "-webkit-appearance" "none"
        , border3 (px 1) solid (hex "ffffff")
        , boxSizing (borderBox)
        , backgroundImage (url assets.svg.selectArrowDown)
        , backgroundColor (hex "000000")
        , Css.color (hex "#ffffff")
        , backgroundRepeat noRepeat
        , Css.property "background-position" "right 5px center"
        , Css.fontFamily inherit
        , Css.height (px 23)
        , paddingLeft (px 10)
        , Css.letterSpacing (px 0.5)
        , Css.minWidth (px 88)
        , Css.fontSize (px 14)
        ]

labelCss : Style
labelCss =
    batch
        [ paddingRight (px 10)
        , boxSizing borderBox
        , whiteSpace noWrap
        , flex (pct 40)
        , letterSpacing (px 0.5)
        , fontWeight bold
        , fontSize (px 18)
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
            , paddingRight (px 10)
            , boxSizing borderBox
            , Css.width (pct 50)
            , Css.height (px 40)
            ]
        ]
        [ label
            [ for "sortSelect"
            , css [ labelCss ]
            ]
            [text "sort by"]
        , select
            [ onInput SortBy
            , id "sortSelect"
            , css
                [ sortingSelectCss model.assets
                , maxWidth (px 116)
                , flex (pct 30)
                ]
            ]
            [ option
                [ value "duration", selected (sortedBy "duration") ]
                [ text "duration" ]
            , option
                [ value "title", selected (sortedBy "title") ]
                [ text "title" ]
            ]
        , div
            [ css
                [ flex (pct 30)
                , textAlign center
                , whiteSpace noWrap
                ]
            ]
            [ input
                [ type_ "radio"
                , name "sort"
                , HSA.checked isAscending
                , onClick (Sort Ascending)
                , css [ sortingArrowCss model.assets ]
                ]
                []
            , input
                [ type_ "radio"
                , name "sort"
                , onClick (Sort Descending)
                , HSA.checked (not isAscending)
                , css [ sortingArrowCss model.assets
                      , Css.property "-webkit-transform" "rotate(180deg)"
                      ]
                ]
                []
            ]
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
        [ displayFlex
        , alignItems baseline
        , Css.width (pct 100)
        , justifyContent spaceBetween
        , whiteSpace noWrap
        , letterSpacing (px 0)
        , color titleColor
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
        , Css.height (px 114)
        , Css.width (pct 100)
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
    div
        [ css [ patchItemCss ] ]
        [ div
            [ css
                [ paddingRight (px 10)
                , flex (pct 50)
                , displayFlex
                , flexDirection column
                ]
            ]
            (patchMedia model patch)
        , div
            [ css
                [color (hex "fff")
                , paddingLeft (px 10)
                , flex (pct 50)
                , displayFlex
                , flexDirection column
                ]
            ]
            ( patchMeta patch )
        ]

patchTopRow : Style
patchTopRow =
    batch
        [ Css.height (px 36)
        , displayFlex
        , alignItems center
        , borderBottom3 (px 1) solid (hex "333")
        ]

patchMedia : Model -> P.Patch -> List (Html Msg)
patchMedia model patch =
    let
        am =
            patch.audioModel |> Maybe.withDefault noPlayingAudioModel

        titleColor =
            if am.playing || am.seekerPosition /= 0.0 then
                hex "ffffff"

            else
                theBlue

        patchUrl =
            "patch/" ++ patch.title
    in
        [ div
            [ css [ patchTopRow ] ]
            [ div
                [css [ patchTitleCss titleColor ] ]
                [ a
                    [ href patchUrl
                    , css
                        [ marginRight (px 10)
                        , fontWeight bold
                        , fontSize (px 20)
                        , linkUnstyle
                        ]
                    ]
                    [ text patch.title ]
                , span
                    [ css
                        [ marginLeft (px 10)
                        , fontSize (px 14)
                        , fontWeight (int 600)
                        ]
                    ]
                    [ text (P.durationToTime patch.duration) ]
                ]
            ]
        , div
            [ css
                [ displayFlex, alignItems center, flexDirection row ] ]
            [ div
                [ css [] ]
                [ playButton patch 48 model.assets]
            , div
                [ css [ flex (num 5), Css.height (px 77) ] ]
                [ waveformSeeker False patch ]
            , audioNode model patch
            ]
        ]


patchAttributesCss : Style
patchAttributesCss =
    batch
        [ fontSize (px 14)
        , letterSpacing (px 0.2)
        , fontWeight (int 600)
        -- an invisible text in font  to align baseline
        -- with the patch title and duration info
        , after [ Css.property "content" "''"
                , fontSize (px 20) ]
        ]


patchLinksBoxCss : Style
patchLinksBoxCss =
    batch
        [ displayFlex
        , alignItems center
        , flex (num 1)
        , Css.width (pct 100)
        , fontSize (px 14)
        , fontWeight bold
        , marginTop (px 0)
        ]

patchLinkCss : Style
patchLinkCss =
    batch
        [ linkUnstyle
        , flex (num 1)
        , displayFlex
        , alignItems center
        ]

patchMeta : P.Patch -> List (Html Msg)
patchMeta p =
    let
        durationText =
            P.durationToTime p.duration

        attribs =
            p.attributeValues |> String.join " / "

        patchUrl =
            "patch/" ++ p.title
    in
        [ div
            [ css [ patchTopRow ] ]
            [ div
                [ css
                    [ displayFlex
                    , alignItems baseline
                    ]
                ]
                [ span
                    [ css [ patchAttributesCss ] ]
                    [ text attribs ]
                ]
            ]
        , div
            [ css
                [ patchLinksBoxCss ] ]
            [ a
                [ href p.download
                , css
                    [ patchLinkCss
                    , flex (num 1)
                    ]
                ]
                [ downBttn theGrayString
                , span
                    [ css [ marginLeft (px 4) ] ]
                    [ text "download" ]
                ]
            , a
                [ href patchUrl
                , css
                    [ patchLinkCss
                    , flex (num 2)
                    ]
                ]
                [ rightBttn theGrayString
                , span
                    [ css [ marginLeft (px 4) ] ]
                    [ text "score" ]
                ]
            ]
        ]
