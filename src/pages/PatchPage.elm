module PatchPage exposing (page)

import Components exposing (..)
import Css exposing (..)
import Css.Global exposing (body, global)
import Html.Styled as HS exposing (..)
import Html.Styled.Attributes exposing (css, download, href, src, type_)
import Html.Styled.Events exposing (..)
import Knob exposing (..)
import List.Extra exposing (find)
import Maybe.FlatMap exposing (flatMap)
import Model exposing (Model)
import Msg exposing (..)
import Patch exposing (..)
import PinTable exposing (..)
import Routing as R
import Styles exposing (..)
import Svg.Styled as Svg
import Svg.Styled.Attributes as Svg
import SynthiSchema as SS
import Url as Url
import Url.Builder as Url exposing (absolute, relative)
import ViewModel exposing (Control(..), Knob, Module)


page : Bool -> String -> Model -> Html Msg
page showGraphical patchTitle model =
    let
        patch =
            model.patches
                |> Maybe.map (find (\p -> p.title == patchTitle) >> Maybe.withDefault noPatch)
                |> Maybe.withDefault noPatch

        ( view, bgColor ) =
            ( [ graphical model patch ], "9b9b9b" )
    in
    div
        [ css
            [ Css.color (hex "ffffff")
            , Css.paddingLeft (px 31)
            , Css.paddingTop (px 18)
            , backgroundColor (hex bgColor)
            , backgroundClip paddingBox
            , displayFlex
            , flex (int 1)
            ]
        ]
        view


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


ppVICss : Style
ppVICss =
    batch
        [ Css.property "-webkit-appearance" "none"
        , width (px 80)
        , height (px 3)
        , margin2 (px 46) (px 0)
        , transform (rotate (deg -90))
        ]


ppVIThumbCss : Style
ppVIThumbCss =
    batch
        [ pseudoElement "-moz-range-thumb"
            [ width (px 4)
            , height (px 20)
            , border3 (px 2) solid theGray
            , borderRadius (px 0)
            , backgroundColor theDarkGray
            ]
        ]


ppVolumeInput : Model -> Html Msg
ppVolumeInput model =
    let
        isMute =
            model.volume == 0
    in
    div [ css [ displayFlex, flexDirection column, alignItems center, height (pct 100) ] ]
        [ div [ css [ flex (int 4) ] ]
            [ input
                [ type_ "range"
                , onInput (String.toFloat >> Maybe.withDefault 0.5 >> VolumeChange)
                , css [ ppVICss, ppVIThumbCss ]
                ]
                []
            ]
        , div [ css [ flex (int 1) ] ]
            [ muteBttn (model.volume == 0) ]
        ]


muteBttn : Bool -> Html Msg
muteBttn isMute =
    let
        url =
            if isMute then
                "/mute.svg"

            else
                "/unmute.svg"
    in
    img
        [ src url
        , css [ height (px 20) ]
        , Html.Styled.Attributes.style "filter" "brightness(1000%)"
        ]
        []


headerCss : Style
headerCss =
    batch
        [ fontSize (px 24)
        , fontWeight bold
        , letterSpacing (px 0.5)
        ]


audioControlsCss : Style
audioControlsCss =
    batch [ displayFlex, alignItems center, height (px 120) ]


soundControls : Model -> Patch -> Html Msg
soundControls model patch =
    div [ css [ audioControlsCss ] ]
        [ div [ css [ flex (int 1) ] ]
            [ playButton patch ]
        , div [ css [ flex (int 3), height (pct 100) ] ]
            [ waveformSeeker True patch ]
        , div [ css [ flex (int 1) ] ]
            [ text (durationToTime patch.duration) ]
        , div [ css [ flex (int 1), height (pct 100) ] ]
            [ ppVolumeInput model ]
        , div [] [ audioNode model patch ]
        ]


controls : Model -> Patch -> Html Msg
controls model patch =
    div
        [ css [ controlsCss ] ]
        [ soundControls model patch
        , div
            [ css
                [ padding2 (px 18) (px 0)
                , margin (px 0)
                , borderTop2 (px 1) solid
                , borderBottom2 (px 1) solid
                , displayFlex
                , alignItems center
                , marginTop (px 30)
                ]
            ]
            [ downBttn "#ffffff"
            , a
                [ href patch.download
                , download ""
                , css
                    [ linkUnstyle
                    , paddingLeft (px 15)
                    ]
                ]
                [ text "download audio + text" ]
            ]
        , div
            [ css
                [ Css.height (px 60)
                , borderTop2 (px 1) solid
                , borderBottom2 (px 1) solid
                , maxWidth (px 390)
                , Css.width (pct 33)
                , position fixed
                , bottom (px 30)
                ]
            ]
            [ div [ css [ width (pct 50), float left ] ] [ h1 [] [ text "Patch" ] ]
            , div [ css [ float right, display inlineBlock, marginTop (px 5) ] ]
                [ prevBttn (MovePatch patch -1)
                , nextBttn (MovePatch patch 1)
                ]
            ]
        ]


pnBttnDivCss : Style
pnBttnDivCss =
    batch
        [ display inlineBlock
        , margin2 (px 5) (px 10)
        , cursor pointer
        ]


pnBttnSpanCss : Style
pnBttnSpanCss =
    batch
        [ display inlineBlock
        , marginTop (px 6)
        ]


prevBttn : msg -> Html msg
prevBttn msg =
    div [ onClick msg, css [ pnBttnDivCss ] ]
        [ prev
        , span [ css [ pnBttnSpanCss ] ] [ text "previous" ]
        ]


nextBttn : msg -> Html msg
nextBttn msg =
    div [ onClick msg, css [ pnBttnDivCss ] ]
        [ next
        , span [ css [ pnBttnSpanCss ] ] [ text "next" ]
        ]


patchMeta : Patch -> Html Msg
patchMeta patch =
    div []
        [ h1 [ css [ Css.fontSize (px 36) ] ] [ text patch.title ]
        , p [ css [ marginBottom (px 20) ] ] [ text (patch.attributeValues |> String.join " / ") ]
        ]


score : Patch -> Html Msg
score patch =
    div [ css [ backgroundColor (hex "c8c8c8"), padding2 (px 35) (px 42) ] ]
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


sectionCss : Style
sectionCss =
    batch
        [ flex (int 1)
        , displayFlex
        , borderBottom3 (px 2) solid (hex "000000")
        , borderTop3 (px 2) solid (hex "000000")
        , paddingTop (px 6)
        , marginBottom (px 6)
        ]


graphical : Model -> Patch -> Html Msg
graphical model patch =
    div [ css [ displayFlex, flexDirection column, Css.width (pct 100), flex (int 1) ] ]
        [ div [ css [ sectionCss ] ]
            [ graphicControls "Audio signals" True model patch
            , pinPanel Audio model patch
            ]
        , div [ css [ sectionCss ] ]
            [ graphicControls "Control voltages" True model patch
            , pinPanel Control model patch
            ]
        , div [ css [ sectionCss ] ]
            [ graphicControls "Output channels" False model patch
            , outputChannels model patch
            ]
        , div
            [ css
                [ flex (int 1)
                , displayFlex
                , borderBottom2 (px 5) double
                ]
            ]
            [ graphicControls "Textual score" False model patch
            , score patch
            ]
        ]


controlsGraphicalCss : Style
controlsGraphicalCss =
    batch
        [ fontSize (px 14)
        , fontWeight bold
        , letterSpacing (px 0.5)
        , color (hex "fff")
        , flex (int 1)
        , alignSelf flexStart
        , maxWidth (px 390)
        , marginRight (px 55)
        , paddingTop (px 6)
        ]


graphicControls : String -> Bool -> Model -> Patch -> Html Msg
graphicControls header showParameters model patch =
    div [ css [ controlsGraphicalCss ] ]
        [ div
            [ css
                [ Css.height (px 60)
                , color (hex "000")
                , borderBottom3 (px 2) solid (hex "000")
                ]
            ]
            [ span [ css [ headerCss, display inlineBlock, marginTop (px 6) ] ]
                [ text header ]
            , img [ src "/graphical.svg", css [ marginRight (px 18), float right ] ] []
            ]
        , soundControls model patch
        , patchMeta patch
        , downloadStrip patch
        , patchNav patch
        , if showParameters then
            parameters model patch

          else
            div [] []
        ]


patchNav : Patch -> Html Msg
patchNav patch =
    div
        [ css
            [ Css.height (px 60)
            , borderTop3 (px 1) solid (hex "000000")
            , borderBottom3 (px 1) solid (hex "000000")
            , width (pct 100)
            , maxWidth (px 390)
            , displayFlex
            ]
        ]
        [ div [ css [ flex (int 5) ] ] [ h2 [] [ text "Patch" ] ]
        , div [ css [ flex (int 3), marginTop (px 5) ] ]
            [ prevBttn (MovePatch patch -1)
            , nextBttn (MovePatch patch 1)
            ]
        ]


downloadStrip : Patch -> Html Msg
downloadStrip patch =
    div
        [ css
            [ padding2 (px 18) (px 0)
            , margin (px 0)
            , borderTop2 (px 1) solid
            , borderBottom2 (px 1) solid
            , displayFlex
            , alignItems center
            , marginTop (px 30)
            , color (hex "000000")
            ]
        ]
        [ downBttn "#ffffff"
        , a
            [ href patch.download
            , download ""
            , css
                [ linkUnstyle
                , paddingLeft (px 15)
                ]
            ]
            [ text "download audio + text" ]
        ]


parameters : Model -> Patch -> Html Msg
parameters model patch =
    div []
        [ div
            [ css
                [ Css.height (px 60)
                , color (hex "000")
                , borderBottom3 (px 2) solid (hex "000")
                , borderTop3 (px 2) solid (hex "000")
                ]
            ]
            [ span [ css [ headerCss, display inlineBlock, marginTop (px 18) ] ] [ text "Parameters" ] ]
        , knob model patch
        ]


knobInfoStyle : Style
knobInfoStyle =
    batch
        [ borderBottom3 (px 1) solid (hex "d8d8d8")
        , paddingBottom (px 10)
        , height (px 15)
        ]


knob : Model -> Patch -> Html Msg
knob model patch =
    let
        ( im, om ) =
            model.activeModules |> Maybe.withDefault ( Module " " [], Module " " [] )

        ( iac, oac ) =
            model.activeControl
    in
    div
        [ css
            [ color (hex "d8d8d8")
            , fontWeight bold
            , letterSpacing (px 0.5)
            , textTransform uppercase
            ]
        ]
        [ div
            [ css
                [ Css.width (pct 100)
                , margin2 (px 15) (px 0)
                ]
            ]
            [ div [ css [ knobInfoStyle ] ]
                [ span [] [ text im.name ] ]
            , div [ css [ knobInfoStyle, paddingTop (px 10) ] ]
                [ span [] [ text (iac |> Maybe.withDefault " ") ] ]
            ]
        , div [ css [ marginBottom (px 22), height (px 43) ] ]
            [ HS.map (\kmsg -> InputKnobEvent kmsg) (controlsToKnobSvg False im.controls) ]
        , div
            [ css
                [ Css.width (pct 100)
                , margin2 (px 15) (px 0)
                ]
            ]
            [ div [ css [ knobInfoStyle, borderTop3 (px 1) solid (hex "d8d8d8"), paddingTop (px 10) ] ]
                [ span [] [ text om.name ] ]
            , div [ css [ knobInfoStyle, paddingTop (px 10) ] ]
                [ span [] [ text (oac |> Maybe.withDefault "") ] ]
            ]
        , div [ css [ height (px 43) ] ]
            [ HS.map (\kmsg -> OutputKnobEvent kmsg) (controlsToKnobSvg False om.controls) ]
        ]


controlsToKnobSvg : Bool -> List Control -> HS.Html KnobMsg
controlsToKnobSvg isVert cs =
    div [ css [ fontWeight (int 500) ] ]
        (cs
            |> List.map
                (\ctrl ->
                    case ctrl of
                        KnobCtrl knb ->
                            ctrlHldr isVert (simpleKnobSvg knb)

                        SwitchCtrl sw ->
                            ctrlHldr isVert (simpleSwitchSvg sw)
                )
        )


ctrlHldr : Bool -> Html KnobMsg -> Html KnobMsg
ctrlHldr isVert ctrl =
    let
        dspl =
            if isVert then
                display block

            else
                display inlineBlock
    in
    div [ css [ dspl, marginRight (px 6), cursor default ] ] [ ctrl ]


pinPanel : Panel -> Model -> Patch -> HS.Html Msg
pinPanel panel model patch =
    let
        ( inModuleText, outModuleText ) =
            case panel of
                Audio ->
                    model.synthiSchema
                        |> Maybe.map
                            (getModulesText model.audioPinModel)
                        |> Maybe.withDefault ( "", "" )

                Control ->
                    model.synthiSchema
                        |> Maybe.map
                            (getModulesText model.controlPinModel)
                        |> Maybe.withDefault ( "", "" )

        ( pinModel, pins, msgMap ) =
            case panel of
                Audio ->
                    ( model.audioPinModel, patch.audioPins, reactToAudioPinEvent panel )

                Control ->
                    ( model.controlPinModel, patch.controlPins, reactToAudioPinEvent panel )
    in
    div [ css [ Css.width (px 760) ] ]
        [ div [ css [ Css.height (px 58), paddingLeft (px 10), backgroundColor (hex "9b9b9b") ] ]
            [ moduleStrip outModuleText (px 40) (px 0)
            , moduleStrip inModuleText (px 0) (px -14)
            ]
        , pinTable msgMap pins pinModel
        ]


pinTable : (PinMsg -> Msg) -> List Pin -> PinModel -> Html Msg
pinTable msgCast pins model =
    div [ css [ marginBottom (px 40) ] ]
        [ HS.map msgCast (audioPanel pins model) ]


moduleStrip : String -> Px -> Px -> Html Msg
moduleStrip txt rm tm =
    div
        [ css
            [ Css.height (px 50)
            , Css.width (px 327)
            , borderBottom3 (px 2) solid (hex "000")
            , float left
            , margin4 (px 0) rm (px 0) (px 20)
            , textTransform uppercase
            , fontSize (px 10)
            , fontWeight bold
            , letterSpacing (px 1.4)
            ]
        ]
        [ span [ css [ display inlineBlock, margin2 (px 23) (px 0), float left ] ] [ text txt ]
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
    div [ css [ Css.color (hex "000"), fontSize (px 14), fontWeight bold, letterSpacing (px 1.4), marginLeft (px 26) ] ]
        (div [ css [ Css.height (px 50), Css.width (px 327), marginLeft (px 33) ] ]
            [ span [ css [ display inlineBlock ] ] [ text "OUTPUT CHANNELS" ]
            ]
            :: [ outputPanel channels ]
        )
        |> HS.map (\kmsg -> OutputKnobEvent kmsg)


outputChan : Knob -> HS.Html Msg
outputChan chan =
    div []
        [ text (chan.value |> Maybe.withDefault 0 |> String.fromFloat)
        ]


channelTag : String
channelTag =
    "output-ch-"


getOutputChannels : SS.SynthiSchema -> List ModuleSettings -> List ( Knob, Knob, Knob )
getOutputChannels ss lms =
    let
        outChans =
            ss.modules
                |> List.filterMap
                    (\mod ->
                        if mod.name |> String.startsWith channelTag then
                            Just mod

                        else
                            Nothing
                    )

        chansWithValues =
            outChans
                |> List.map
                    (\chan ->
                        let
                            moduleM =
                                lms |> List.Extra.find (\pm -> pm.name == chan.name)

                            ( level, filter, pan ) =
                                moduleM
                                    |> Maybe.map
                                        (\mdl ->
                                            let
                                                enabled =
                                                    mdl.controlValues
                                                        |> List.Extra.find
                                                            (\ctrl ->
                                                                case ctrl of
                                                                    SwitchVal kkk ->
                                                                        kkk.name == "enabled"

                                                                    _ ->
                                                                        False
                                                            )

                                                lvl =
                                                    mdl.controlValues
                                                        |> List.Extra.find
                                                            (\ctrl ->
                                                                case ctrl of
                                                                    KnobVal kkk ->
                                                                        kkk.name == "level"

                                                                    _ ->
                                                                        False
                                                            )
                                                        |> flatMap
                                                            (\p ->
                                                                case p of
                                                                    KnobVal k ->
                                                                        Just k.position

                                                                    _ ->
                                                                        Nothing
                                                            )

                                                fltr =
                                                    mdl.controlValues
                                                        |> List.Extra.find
                                                            (\ctrl ->
                                                                case ctrl of
                                                                    KnobVal kkk ->
                                                                        kkk.name == "filter"

                                                                    _ ->
                                                                        False
                                                            )
                                                        |> flatMap
                                                            (\p ->
                                                                case p of
                                                                    KnobVal k ->
                                                                        Just k.position

                                                                    _ ->
                                                                        Nothing
                                                            )

                                                pn =
                                                    mdl.controlValues
                                                        |> List.Extra.find
                                                            (\ctrl ->
                                                                case ctrl of
                                                                    KnobVal kkk ->
                                                                        kkk.name == "pan"

                                                                    _ ->
                                                                        False
                                                            )
                                                        |> flatMap
                                                            (\p ->
                                                                case p of
                                                                    KnobVal k ->
                                                                        Just k.position

                                                                    _ ->
                                                                        Nothing
                                                            )
                                            in
                                            ( lvl, fltr, pn )
                                        )
                                    |> Maybe.withDefault ( Nothing, Nothing, Nothing )
                        in
                        ( Knob chan.name level, Knob chan.name filter, Knob chan.name pan )
                    )
    in
    chansWithValues |> List.sortBy (\( l, f, p ) -> l.name)


reactToAudioPinEvent : PinTable.Panel -> PinTable.PinMsg -> Msg
reactToAudioPinEvent p pinMsg =
    case pinMsg of
        PinTable.PinClick ( x, y ) ->
            Msg.PinClick p ( x, y )

        PinTable.PinIn ( x, y ) ->
            Msg.PinHover p ( x, y )

        PinTable.PinOut ->
            Msg.PinOut p


getModulesText : PinModel -> SS.SynthiSchema -> ( String, String )
getModulesText pm ss =
    let
        ( inModuleIndex, outModuleIndex ) =
            pm.hoverPin

        ( inModulePosition, outModulePosition ) =
            coordsToPinPos pm.hoverPin

        p =
            case pm.panel of
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
        (pos |> String.fromInt) ++ " " ++ mod.module_ ++ " " ++ mod.name

    else
        ""
