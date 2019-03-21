module PatchPage exposing (page)

import Components exposing (..)
import Css exposing (..)
import Css.Global exposing (body, global)
import Html.Styled as HS exposing (..)
import Html.Styled.Attributes exposing (css, href, src)
import Html.Styled.Events exposing (..)
import Knob exposing (..)
import List.Extra exposing (find)
import Model exposing (Model)
import Msg exposing (..)
import Patch exposing (..)
import PinTable exposing (..)
import Routing as R
import SynthiSchema as SS
import Url as Url
import Url.Builder as Url exposing (absolute, relative)
import ViewModel exposing (Module)


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
        , div [ css [ Css.width (pct 66), float right ] ]
            [ pin model patch
            ]
        ]


graphicControls : Model -> Patch -> Html Msg
graphicControls model patch =
    div [ css [ Css.width (pct 33), float left, position Css.fixed, top (px 108) ] ]
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

        ( iac, oac ) =
            model.activeControl
    in
    div []
        [ p [] [ text im.name ]
        , p [] [ text (iac |> Maybe.withDefault ".") ]
        , HS.map (\kmsg -> InputKnobEvent kmsg) (controlsToKnobSvg im.controls)
        , p [] [ text om.name ]
        , p [] [ text (oac |> Maybe.withDefault ".") ]
        , HS.map (\kmsg -> OutputKnobEvent kmsg) (controlsToKnobSvg om.controls)
        ]


pin : Model -> Patch -> HS.Html Msg
pin model patch =
    let
        ( audioInModuleText, audioOutModuleText ) =
            model.synthiSchema
                |> Maybe.map
                    (getModulesText Audio model.audioPinModel)
                |> Maybe.withDefault ( "", "" )

        ( controlInModuleText, controlOutModuleText ) =
            model.synthiSchema
                |> Maybe.map
                    (getModulesText Control model.controlPinModel)
                |> Maybe.withDefault ( "", "" )
    in
    div []
        [ div [ css [ Css.height (px 100) ] ]
            [ div [] [ text audioInModuleText ]
            , div [] [ text audioOutModuleText ]
            ]
        , HS.map (reactToAudioPinEvent Audio) (audioPanel patch.audioPins model.audioPinModel)
        , div [ css [ Css.height (px 100) ] ]
            [ div [] [ text controlInModuleText ]
            , div [] [ text controlOutModuleText ]
            ]
        , HS.map (reactToAudioPinEvent Control) (audioPanel patch.controlPins model.controlPinModel)
        ]


reactToAudioPinEvent : PinTable.Panel -> PinTable.PinMsg -> Msg
reactToAudioPinEvent p pinMsg =
    case pinMsg of
        PinTable.PinClick ( x, y ) ->
            Msg.PinClick p ( x, y )

        PinTable.PinIn ( x, y ) ->
            Msg.PinHover p ( x, y )

        PinTable.PinOut ->
            Msg.PinOut


getModulesText : Panel -> PinModel -> SS.SynthiSchema -> ( String, String )
getModulesText panel pm ss =
    let
        ( inModuleIndex, outModuleIndex ) =
            pm.hoverPin

        ( inModulePosition, outModulePosition ) =
            coordsToPinPos pm.hoverPin

        p =
            case panel of
                Audio ->
                    ss.audioPanel

                control ->
                    ss.controlPanel

        inModule =
            p
                |> List.Extra.getAt inModuleIndex

        outModule =
            p
                |> List.Extra.getAt (outModuleIndex + 60)

        ( inModuleText, outModuleText ) =
            Maybe.map2
                (\im om ->
                    ( moduleAndPosToText inModulePosition im, moduleAndPosToText outModulePosition om )
                )
                inModule
                outModule
                |> Maybe.withDefault ( "", "" )
    in
    ( inModuleText, outModuleText )


moduleAndPosToText : Int -> SS.Connection -> String
moduleAndPosToText pos mod =
    if pos >= 0 then
        (pos |> String.fromInt) ++ " - " ++ mod.name ++ " : " ++ mod.module_

    else
        ""
