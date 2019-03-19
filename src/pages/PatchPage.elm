module PatchPage exposing (page)

import Components exposing (..)
import Css exposing (..)
import Css.Global exposing (body, global)
import Html.Styled as HS exposing (..)
import Html.Styled.Attributes exposing (css, href, src)
import Html.Styled.Events exposing (..)
import Knob exposing (..)
import List.Extra exposing (find)
import Model exposing (Model, Module)
import Msg exposing (..)
import Patch exposing (..)
import PinTable exposing (..)
import Routing as R
import SynthiSchema as SS
import Url as Url
import Url.Builder as Url exposing (absolute, relative)


page : Bool -> String -> Model -> Html Msg
page showGraphical patchTitle model =
    let
        patch =
            model.patches
                |> Maybe.map (find (\p -> p.title == patchTitle) >> Maybe.withDefault noPatch)
                |> Maybe.withDefault noPatch

        view =
            if showGraphical then
                [ graphical model patch ]

            else
                controls model patch
                    :: [ waveAndText model patch ]
    in
    div [ css [ Css.color (hex "ffffff") ] ]
        view


waveOrGraph : Model -> String -> Html Msg
waveOrGraph model patchTitle =
    let
        waveTextUrl =
            absolute [ "patch", patchTitle ] []

        graphicalUrl =
            absolute [ "patch", patchTitle, "graphical" ] []
    in
    div []
        [ a [ href waveTextUrl ] [ text "wave / textual" ]
        , br [] []
        , a [ href graphicalUrl ] [ text "graphical" ]
        ]


controls : Model -> Patch -> Html Msg
controls model patch =
    div [ css [ Css.width (pct 33), float left ] ]
        [ waveOrGraph model patch.title
        , div []
            [ playButton patch
            , volumeInput
            ]
        , audioNode model patch
        , patchMeta patch
        , button [ onClick (MovePatch patch -1) ] [ text "previous" ]
        , button [ onClick (MovePatch patch 1) ] [ text "next" ]
        ]


patchMeta : Patch -> Html Msg
patchMeta patch =
    div []
        [ h1 [] [ text patch.title ]
        , p [] [ text ("duration: " ++ (patch.duration |> String.fromFloat)) ]
        , p [] [ text (patch.attributeValues |> String.join " / ") ]
        , hr [] []
        ]


waveAndText : Model -> Patch -> Html Msg
waveAndText model patch =
    div [ css [ Css.width (pct 66), float left ] ]
        [ waveformSeeker patch
        , HS.pre [ css [ Css.float left ] ] [ text patch.score ]
        ]


graphical : Model -> Patch -> Html Msg
graphical model patch =
    div [ css [ Css.backgroundColor (hex "9b9b9b"), float left, Css.width (pct 100) ] ]
        [ graphicControls model patch
        , div [ css [ Css.width (pct 66), float left ] ]
            [ pin model patch
            ]
        ]


graphicControls : Model -> Patch -> Html Msg
graphicControls model patch =
    div [ css [ Css.width (pct 33), float left ] ]
        [ waveOrGraph model patch.title
        , patchMeta patch
        , parameters model patch
        , button [ onClick (MovePatch patch -1) ] [ text "previous" ]
        , button [ onClick (MovePatch patch 1) ] [ text "next" ]
        ]


parameters : Model -> Patch -> Html Msg
parameters model patch =
    div []
        [ h1 [] [ text "parameters" ]
        , hr [] []
        , knob model patch
        ]


knob : Model -> Patch -> Html Msg
knob model patch =
    let
        ( im, om ) =
            model.activeModules |> Maybe.withDefault ( Module "" [], Module "" [] )
    in
    div []
        [ HS.map (\kmsg -> KnobEvent kmsg) (controlsToKnobSvg om.controls)
        , HS.map (\kmsg -> KnobEvent kmsg) (controlsToKnobSvg im.controls)
        ]


pin : Model -> Patch -> HS.Html Msg
pin model patch =
    let
        ( audioOutModuleText, audioInModuleText ) =
            getModulesText model.synthiSchema model.audioPinModel

        ( controlOutModuleText, controlInModuleText ) =
            getModulesText model.synthiSchema model.controlPinModel
    in
    div []
        [ div [ css [ Css.height (px 100) ] ]
            [ div [] [ text audioOutModuleText ]
            , div [] [ text audioInModuleText ]
            ]
        , HS.map (reactToAudioPinEvent Audio) (audioPanel patch.audioPins model.audioPinModel)
        , div [ css [ Css.height (px 100) ] ]
            [ div [] [ text controlOutModuleText ]
            , div [] [ text controlInModuleText ]
            ]
        , HS.map (reactToAudioPinEvent Control) (audioPanel patch.controlPins model.controlPinModel)
        ]


reactToAudioPinEvent : PinTable.Pin -> PinTable.PinMsg -> Msg
reactToAudioPinEvent p pinMsg =
    case pinMsg of
        PinTable.PinClick ( x, y ) ->
            Msg.PinClick p ( x, y )

        PinTable.PinIn ( x, y ) ->
            Msg.PinHover p ( x, y )

        PinTable.PinOut ->
            Msg.PinOut


getModulesText : Maybe SS.SynthiSchema -> PinModel -> ( String, String )
getModulesText mss pm =
    let
        ( inModuleIndex, outModuleIndex ) =
            pm.hoverPin

        ( inModulePosition, outModulePosition ) =
            coordsToPinPos pm.hoverPin

        outModule =
            mss
                |> Maybe.map
                    (\ss ->
                        ss.audioPanel |> List.Extra.getAt outModuleIndex |> Maybe.withDefault { name = "--", module_ = "--" }
                    )

        outModuleText =
            if outModuleIndex >= 0 then
                outModule
                    |> Maybe.map
                        (\om -> (outModulePosition |> String.fromInt) ++ om.name ++ " " ++ om.module_)
                    |> Maybe.withDefault "-"

            else
                ""

        inModule =
            mss
                |> Maybe.map
                    (\ss ->
                        ss.audioPanel |> List.Extra.getAt (inModuleIndex + 60) |> Maybe.withDefault { name = "--", module_ = "--" }
                    )

        inModuleText =
            if inModuleIndex >= 0 then
                inModule
                    |> Maybe.map
                        (\aom -> (inModulePosition |> String.fromInt) ++ aom.name ++ " " ++ aom.module_)
                    |> Maybe.withDefault "-"

            else
                ""
    in
    ( inModuleText, outModuleText )
