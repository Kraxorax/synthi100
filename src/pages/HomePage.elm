module HomePage exposing (page)

import AudioModel exposing (AudioModel, noPlayingAudioModel)
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
import SynthiSchema exposing (Attribute)


page : Model -> Html Msg
page model =
    div []
        [ h1 []
            [ text "Home page"
            ]
        , filterList model
        , patchesList model
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
    div [ css [ Css.width (pct 33), float left ] ]
        [ h4 []
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


filterGroup : AttrFilter -> Attribute -> Html Msg
filterGroup filter attr =
    div []
        [ h5 []
            [ text attr.name ]
        , ul [ css [ Css.listStyle none ] ]
            [ div []
                (attr.values
                    |> List.map
                        (\v ->
                            li []
                                [ radio v attr.name (filter.selected |> findIndex (\s -> s == v) |> isJust) ]
                        )
                )
            ]
        ]


radio : String -> String -> Bool -> Html Msg
radio val group isChecked =
    label []
        [ input [ type_ "checkbox", id val, onInput (Filter group), HSA.checked isChecked, value val ] []
        , text val
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


volumeInput : Html Msg
volumeInput =
    div []
        [ span [] [ text "volume" ]
        , input [ type_ "range", onInput (String.toFloat >> Maybe.withDefault 0.5 >> VolumeChange) ] []
        , button [] [ text "mute" ]
        ]


patchItem : Model -> P.Patch -> Html Msg
patchItem model patch =
    div [ css [ display block, Css.height (px 90) ] ]
        [ patchMeta patch
        , patchMedia model patch
        ]


patchMedia : Model -> P.Patch -> Html Msg
patchMedia model patch =
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
        , audioNode model patch
        ]


patchMediaCss =
    css [ display block, Css.height (pct 100), float left ]


audioNode : Model -> P.Patch -> Html Msg
audioNode model patch =
    let
        playing =
            case patch.audioModel of
                Just am ->
                    am.playing

                Nothing ->
                    False
    in
    if playing then
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
