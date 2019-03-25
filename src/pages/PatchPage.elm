module PatchPage exposing (page)

import Components exposing (..)
import Css exposing (..)
import Css.Global exposing (body, global)
import Html.Styled as HS exposing (..)
import Html.Styled.Attributes exposing (css, href, src, type_)
import Html.Styled.Events exposing (..)
import Knob exposing (..)
import List.Extra exposing (find)
import Model exposing (Model)
import Msg exposing (..)
import Patch exposing (..)
import PinTable exposing (..)
import Routing as R
import Styles exposing (..)
import SynthiSchema as SS
import Url as Url
import Url.Builder as Url exposing (absolute, relative)
import ViewModel exposing (Knob, Module)


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
    div [ css [ Css.color (hex "ffffff"), Css.paddingLeft (px 31) ] ]
        view


waveOrGraphCss : Style
waveOrGraphCss =
    batch
        [ borderTop2 (px 1) solid
        , borderBottom2 (px 1) solid
        , padding2 (px 12) (px 0)
        , height (px 56)
        ]


waveOrGraph : Model -> String -> Html Msg
waveOrGraph model patchTitle =
    let
        waveTextUrl =
            absolute [ "patch", patchTitle ] []

        graphicalUrl =
            absolute [ "patch", patchTitle, "graphical" ] []
    in
    div [ css [ waveOrGraphCss ] ]
        [ a [ href waveTextUrl, css [ Css.float left, linkUnstyle ] ]
            [ img [ src "/wave-textual.svg", css [ marginBottom (px 10) ] ] []
            , br [] []
            , span [] [ text "wave / textual" ]
            ]
        , a [ href graphicalUrl, css [ Css.float right, linkUnstyle ] ]
            [ span [] [ text "graphical" ]
            , img [ src "/graphical.svg", css [ marginLeft (px 18) ] ] []
            ]
        ]


controlsCss : Style
controlsCss =
    batch
        [ fontSize (px 14)
        , fontWeight bold
        , letterSpacing (px 0.5)
        , color (hex "fff")
        , Css.width (pct 33)
        , float left
        , maxWidth (px 390)
        , marginRight (px 55)
        ]


linkUnstyle : Style
linkUnstyle =
    batch
        [ color (hex "fff")
        , textDecoration none
        , display block
        , textAlign center
        ]


audioControlsCss : Style
audioControlsCss =
    batch
        [ height (px 312)
        , padding2 (px 25) (px 0)
        ]


ppVICss : Style
ppVICss =
    batch
        [ Css.property "-webkit-appearance" "none"
        , width (px 256)
        , height (px 4)
        , margin2 (px 22) (px 0)
        ]


ppVIThumbCss : Style
ppVIThumbCss =
    batch
        [ pseudoElement "-moz-range-thumb"
            [ width (px 10)
            , height (px 40)
            , border3 (px 1) solid (hex "000")
            , borderRadius (px 0)
            , backgroundColor theBlue
            ]
        ]


ppVolumeInput : Model -> Html Msg
ppVolumeInput model =
    let
        isMute =
            model.volume == 0
    in
    div [ css [ Css.maxWidth (px 258), float right ] ]
        [ input
            [ type_ "range"
            , onInput (String.toFloat >> Maybe.withDefault 0.5 >> VolumeChange)
            , css [ ppVICss, ppVIThumbCss ]
            ]
            []
        , span [] [ text "volume" ]
        , muteBttn isMute
        ]


muteBttn : Bool -> Html Msg
muteBttn isMute =
    let
        url =
            if isMute then
                "/unmute.svg"

            else
                "/mute.svg"
    in
    img
        [ src url
        , css [ Css.float right, marginTop (px -10) ]
        , onClick (VolumeChange 0)
        ]
        []


controls : Model -> Patch -> Html Msg
controls model patch =
    div
        [ css [ controlsCss ] ]
        [ waveOrGraph model patch.title
        , div [ css [ audioControlsCss ] ]
            [ playButton patch
            , ppVolumeInput model
            , audioNode model patch
            , patchMeta patch
            ]
        , button [ onClick (MovePatch patch -1) ] [ text "previous" ]
        , button [ onClick (MovePatch patch 1) ] [ text "next" ]
        ]


patchMeta : Patch -> Html Msg
patchMeta patch =
    div []
        [ h1 [ css [ Css.fontSize (px 48) ] ] [ text patch.title ]
        , p [ css [ marginBottom (px 20) ] ] [ text ("duration: " ++ (patch.duration |> String.fromFloat)) ]
        , p [ css [ marginBottom (px 20) ] ] [ text (patch.attributeValues |> String.join " / ") ]
        , hr [] []
        ]


waveAndText : Model -> Patch -> Html Msg
waveAndText model patch =
    div [ css [ Css.width (pct 66), float left ] ]
        [ div [ css [ Css.width (pct 100), Css.height (px 386), marginBottom (px 15) ] ]
            [ waveformSeeker patch ]
        , div [ css [ backgroundColor (hex "c8c8c8"), padding2 (px 35) (px 42) ] ]
            [ HS.pre
                [ css
                    [ Css.fontSize (px 14)
                    , fontWeight (int 500)
                    , color (hex "000")
                    , lineHeight (Css.em 1.29)
                    , letterSpacing (px 0.5)
                    ]
                ]
                [ text patch.score ]
            ]
        ]


graphical : Model -> Patch -> Html Msg
graphical model patch =
    div [ css [ Css.backgroundColor (hex "9b9b9b"), float left, Css.width (pct 100) ] ]
        [ graphicControls model patch
        , div [ css [ Css.width (pct 66), float right ] ]
            [ pin model patch
            , outputChannels model patch
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


outputChannels : Model -> Patch -> HS.Html Msg
outputChannels model patch =
    let
        channels =
            model.synthiSchema
                |> Maybe.map
                    (\schema ->
                        getOutputChannels schema patch.moduleSettings
                    )
                |> Maybe.withDefault []
    in
    div []
        (channels
            |> List.map outputChan
        )


outputChan : Knob -> HS.Html Msg
outputChan chan =
    div []
        [ text (chan.value |> Maybe.withDefault 0 |> String.fromFloat)
        ]


channelTag : String
channelTag =
    "output-ch-"


getOutputChannels : SS.SynthiSchema -> List ModuleSettings -> List Knob
getOutputChannels ss lms =
    let
        outChans =
            ss.modules
                |> List.filterMap
                    (\mod ->
                        if mod.name |> String.startsWith channelTag then
                            Just (Knob mod.name Nothing)

                        else
                            Nothing
                    )

        chansWithValues =
            outChans
                |> List.map
                    (\chan ->
                        let
                            k =
                                lms |> List.Extra.find (\pm -> pm.name == chan.name)

                            level =
                                k
                                    |> Maybe.map
                                        (\kk ->
                                            let
                                                lvl =
                                                    kk.controlValues
                                                        |> List.Extra.find
                                                            (\ctrl ->
                                                                case ctrl of
                                                                    KnobVal kkk ->
                                                                        kkk.name == "level"

                                                                    _ ->
                                                                        False
                                                            )
                                            in
                                            lvl
                                                |> Maybe.map
                                                    (\l ->
                                                        case l of
                                                            KnobVal lk ->
                                                                lk.position

                                                            _ ->
                                                                0
                                                    )
                                                |> Maybe.withDefault 0
                                        )
                        in
                        Knob chan.name level
                    )
    in
    chansWithValues |> List.sortBy .name


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
