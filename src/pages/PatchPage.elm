module PatchPage exposing (page)

import Components exposing (..)
import Css exposing (..)
import Css.Global exposing (body, global)
import Html.Styled as HS exposing (..)
import Html.Styled.Attributes as HSA exposing (css, download, href, src, type_)
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
            model.filteredPatches
                |> find (\p -> p.title == patchTitle)
                |> Maybe.withDefault noPatch

        ( view, bgColor ) =
            ( graphical model patch, "9b9b9b" )
    in
    div
        [ css
            [ backgroundColor (hex bgColor)
            , Css.color (hex "ffffff")
            , backgroundClip paddingBox
            , maxWidth (px 1280)
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
        , width (px 126)
        , height (px 3)
        , margin2 (px 6) (px 0)
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
    div [ css [ displayFlex, justifyContent (spaceBetween), alignItems center, borderBottom3 (px 1) solid (hex "000000"), padding2 (px 12) (px 0) ] ]
        [ div [ css [ displayFlex, alignItems center ] ]
            [ muteBttn (model.volume == 0)
            , input
                [ type_ "range"
                , onInput (String.toFloat >> Maybe.withDefault 0.5 >> VolumeChange)
                , css [ marginLeft (px 30), ppVICss, ppVIThumbCss, cursor pointer ]
                ]
                []
            ]
        , label [ css [ displayFlex, alignItems center, cursor pointer ] ]
            [ xBox model.loop
            , input [ type_ "checkbox", HSA.checked model.loop, onInput (\_ -> Loop), css [ display none ] ] []
            , span [ css [ margin2 (px 0) (px 6) ] ] [ text "loop" ]
            ]
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
        , css [ height (px 20), width (px 25), marginRight (px 8) ]
        , HSA.style "filter" "brightness(1000%)"
        ]
        []


headerCss : Style
headerCss =
    batch
        [ fontSize (px 18)
        , fontWeight bold
        , letterSpacing (px 0.5)
        ]


audioControlsCss : Style
audioControlsCss =
    batch [ displayFlex, position Css.relative, alignItems center,  height (px 85), borderBottom3 (px 1) solid (hex "000000") ]


soundControls : Model -> Patch -> Html Msg
soundControls model patch =
    div [ css [ audioControlsCss ] ]
        [ div
            [ css
                [ position Css.absolute, right (px 5), top (px 0)]
            ]
            [ text (durationToTime patch.duration) ]
        , div [ ]
            [ playButton patch 48 ]
        , div [ css [ height (pct 100) ] ]
            [ waveformSeeker True patch ]
        , div [] [ audioNode model patch ]
        ]


prevNext : Patch -> Html Msg
prevNext patch =
    div [ css [ displayFlex, flexDirection row, alignItems center ] ]
        [ div [ css [ marginRight (px 20) ] ]
            [ span [ css [ fontSize (px 18), fontWeight bold ] ] [ text "Patch" ] ]
        , prevBttn (MovePatch patch -1)
        , nextBttn (MovePatch patch 1)
        ]


prevNextBttnCss : Style
prevNextBttnCss =
    batch
        [ margin2 (px 0) (px 6)
        , cursor pointer
        ]


prevBttn : msg -> Html msg
prevBttn msg =
    div [ onClick msg, css [ prevNextBttnCss ] ]
        [ prev
        ]


nextBttn : msg -> Html msg
nextBttn msg =
    div [ onClick msg, css [ prevNextBttnCss ] ]
        [ next
        ]


patchMeta : Patch -> Model -> Html Msg
patchMeta patch model =
    div [ css [ displayFlex, flexDirection row ] ]
        [ div [ css [ flex (int 6) ] ]
            [ h1 [ css [ Css.fontSize (px 36), width (pct 50) ] ] [ text patch.title ]
            , p [ css [ marginBottom (px 20) ] ] [ text (patch.attributeValues |> String.join " / ") ]
            ]
        , div [ css [ flex (int 1), height (pct 100) ] ]
            [ ppVolumeInput model ]
        ]


score : Patch -> Html Msg
score patch =
    HS.pre
        [ css
            [ backgroundColor (hex "c8c8c8")
            , minHeight (px 400)
            , maxWidth (px 657)
            , fontFamilies ["Metropolis"]
            , minWidth (px 660)
            , padding (px 40)
            , Css.fontSize (px 14)
            , fontWeight normal
            , overflow auto
            , color (hex "000")
            , lineHeight (px 18)
            , letterSpacing (px 0.5)
            , marginBottom (px 40)
            ]
        ]
        [ text patch.score ]


toTop : Html Msg
toTop =
    div
        [ css [ position Css.absolute, right (px -60), bottom (px 40), displayFlex, alignItems center, cursor pointer ]
        , onClick ToTop
        ]
        [ upBttn "#ffffff"
        , span [ css [ paddingLeft (px 6) ] ] [ text "top" ]
        ]



patchSectionCss : Style
patchSectionCss =
    batch
        [ displayFlex
        , justifyContent spaceBetween
        , marginLeft (px 40)
        , maxWidth (px 1165)
        , boxSizing borderBox
        ]

doubleSpacer : Html Msg
doubleSpacer =
    div
        [ css
            [ borderBottom3 (px 1) solid (hex "000")
            , borderTop3 (px 1) solid (hex "000")
            , minWidth (px 1010)
            , maxWidth (px 1165)
            , backgroundColor theGray
            , height (px 5)
            , marginLeft (px 40)]
        ]
        []

leftColumnCss : Style
leftColumnCss =
    batch
        [ paddingRight (px 10)
        , boxSizing borderBox
        , minWidth (px 260)
        , width (px 390)
        , maxWidth (px 390)
        ]

graphical : Model -> Patch -> List (Html Msg)
graphical model patch =
    [ div
         [ css [ patchSectionCss, borderBottom3 (px 1) solid (hex "000"),  height (px 60), alignItems center, backgroundColor theGray, minWidth (px 1010) ] ]
         [ div [ css [ leftColumnCss, displayFlex, justifyContent spaceBetween, alignItems baseline ] ]
              [ div [ css [ fontSize (px 36), fontWeight bold, letterSpacing (px 0.5)  ] ] [ text "SCORE" ]
              , prevNext patch
              ]
        , div [ css [  alignItems baseline, backgroundColor theGray, displayFlex, boxSizing borderBox, width (px 745), justifyContent spaceBetween, minWidth (px 745)  ] ]
            [ div [ css [ fontSize (px 36), paddingLeft (px 30), letterSpacing (px 0.5), fontWeight bold ] ] [ text patch.title ]
            , div [ css [fontSize (px 14), letterSpacing (px 0.2), fontWeight bold ] ]
                    [ text (patch.attributeValues |> String.join " / ") ]
            ]
        ]
    , div [ css [ patchSectionCss ] ]
        [ div [ css [ leftColumnCss ] ]
            [ graphicControls model.audioPinModel.activeModules "Audio signals" "/icon-audio.png" True model patch ]
        , div [ css [  backgroundColor theGray  ] ]
            [ pinPanel Audio model patch ]
        ]
    , doubleSpacer
    , div [ css [ patchSectionCss ] ]
        [ div [ css [ leftColumnCss ] ]
            [ graphicControls model.controlPinModel.activeModules "Control voltages" "/icon-control.png" True model patch ]
        , div [ css [  backgroundColor theGray  ] ]
            [ pinPanel Control model patch ]
        ]
    , doubleSpacer
    , div [ css [ patchSectionCss ] ]
        [ div [ css [ leftColumnCss ] ]
              [ graphicControls Nothing "Output channels" "/icon-channels.png" False model patch ]
        , div [ css [  backgroundColor theGray  ] ]
              [ outputChannels model patch ]
        ]
    , doubleSpacer
    , div [ css [ patchSectionCss ] ]
        [ div [ css [ leftColumnCss ] ]
            [ graphicControls Nothing "Textual score" "/icon-score.png" False model patch ]
        , div [ css [ position Css.relative, backgroundColor theGray, paddingTop (px 10), paddingLeft (px 10)  ] ]
            [ score patch, toTop ]
        ]
    ]


controlsGraphicalCss : Style
controlsGraphicalCss =
    batch
        [ fontSize (px 14)
        , fontWeight bold
        , paddingTop (px 7)
        , letterSpacing (px 0.5)
        , color (hex "fff")
        ]


graphicControls : Maybe ( Module, Module ) -> String -> String -> Bool -> Model -> Patch -> Html Msg
graphicControls mModules header iconUrl showParameters model patch =
    let
        ctrls =
            case header of
                "Audio signals" ->
                    [ soundControls model patch
                    , ppVolumeInput model
                    , downloadStrip patch
                    , pageMap header
                    , parameters mModules model patch
                    ]

                "Control voltages" ->
                    [ pageMap header
                    , parameters mModules model patch
                    ]

                "Output channels" ->
                    [ pageMap header ]

                "Textual score" ->
                    [ pageMap header ]

                _ ->
                    []
    in
    div [ css [ controlsGraphicalCss ] ]
        (a [ HSA.name (header |> String.replace " " "_") ] []
            :: ctrls
        )


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
            [ padding2 (px 10) (px 0)
            , borderBottom2 (px 3) solid
            , displayFlex
            , alignItems center
            , color (hex "000000")
            ]
        ]
        [ downBttn "#ffffff"
        , a
            [ href patch.download
            , css
                [ linkUnstyle
                , color (hex "fff")
                , fontWeight (int 600)
                , letterSpacing (px 0.5)
                , marginLeft (px 15)
                ]
            ]
            [ text "download" ]
        ]


pageMap : String -> Html msg
pageMap header =
    let
        sections =
            [ "Audio signals", "Control voltages", "Output channels", "Textual score" ]
    in
    div [ css [ color theLightGray, fontSize (px 18), padding4 (px 7) (px 0) (px 2) (px 0), letterSpacing (px 0.5), borderBottom3 (px 3) solid (hex "000000") ] ]
        (sections
            |> List.map
                (\s ->
                    let
                        color =
                            if s == header then
                                hex "000000"

                            else
                                theLightGray
                    in
                    div [ css [ padding2 (px 3) (px 0) ] ]
                        [ a [ href ("#" ++ s |> String.replace " " "_"), css [ linkUnstyle, Css.color color ] ] [ text s ] ]
                )
        )


parameters : Maybe ( Module, Module ) -> Model -> Patch -> Html Msg
parameters mModules model patch =
    div []
        [ div
            [ css
                [ Css.height (px 48)
                , displayFlex
                , alignItems center
                , color (hex "000")
                , borderBottom3 (px 1) solid (hex "000")
                ]
            ]
            [ span [ css [ headerCss ] ] [ text "Parameters" ] ]
        , knob mModules model patch
        ]


knobInfoStyle : Style
knobInfoStyle =
    batch
        [ borderBottom3 (px 1) solid (hex "d8d8d8")
        , height (px 33)
        , displayFlex
        , alignItems center
        ]


knob : Maybe ( Module, Module ) -> Model -> Patch -> Html Msg
knob mModules model patch =
    let
        ( im, om ) =
            mModules |> Maybe.withDefault ( Module " " [], Module " " [] )

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
                ]
            ]
            [ div [ css [ knobInfoStyle ] ]
                [ span [] [ text im.name ] ]
            , div [ css [ knobInfoStyle ] ]
                [ span [] [ text (iac |> Maybe.withDefault " ") ] ]
            ]
        , div [ css [ padding2 (px 8) (px 0), displayFlex, alignItems center, boxSizing borderBox, minHeight (px 64) ] ]
            [ HS.map (\kmsg -> InputKnobEvent kmsg) (controlsToKnobSvg False im.controls) ]
        , div
            [ css
                [ Css.width (pct 100)
                ]
            ]
            [ div [ css [ knobInfoStyle, borderTop3 (px 1) solid (hex "d8d8d8") ] ]
                [ span [] [ text om.name ] ]
            , div [ css [ knobInfoStyle ] ]
                [ span [] [ text (oac |> Maybe.withDefault "") ] ]
            ]
        , div [ css [  padding2 (px 8) (px 0), borderBottom3 (px 1) solid (hex "d8d8d8"), displayFlex, alignItems center, boxSizing borderBox, minHeight (px 64) ] ]
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
    div [ css [ dspl, marginRight (px 4), cursor default ] ] [ ctrl ]


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
    div [ css [ Css.width (px 750)  ] ]
        [ div
            [ css
                [ displayFlex
                , paddingLeft (px 35)
                , width (pct 100)
                , boxSizing borderBox
                , justifyContent spaceBetween
                ]
            ]
            [ span [ css [moduleStripCss, marginRight (px 20)]]
                [text  outModuleText]
            , span [ css [moduleStripCss, marginLeft (px 20)]]
                [text  inModuleText]
            ]
        , pinTable msgMap pins pinModel
        ]


pinTable : (PinMsg -> Msg) -> List Pin -> PinModel -> Html Msg
pinTable msgCast pins model =
    div [ css [ marginBottom (px 35), backgroundColor theGray ] ]
        [ HS.map msgCast (audioPanel pins model) ]


moduleStripCss : Style
moduleStripCss =
    batch
        [ borderBottom3 (px 2) solid (hex "000")
        , Css.height (px 50)
        , displayFlex
        , flex (num 1)
        , alignItems center
        , textTransform uppercase
        , fontSize (px 10)
        , fontWeight bold
        , letterSpacing (px 1.4)
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
    div [ css [ Css.color (hex "000"), fontSize (px 14), fontWeight bold, letterSpacing (px 1.4) ] ]
        (div [ css [ Css.height (px 50), Css.width (px 327), marginLeft (px 33) ] ]
            [ span [ css [ display inlineBlock, paddingTop (px 20) ] ] [ text "OUTPUT CHANNELS" ]
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


getOutputChannels : SS.SynthiSchema -> List ModuleSettings -> List OutputChanValues
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

                            chanValues =
                                moduleM
                                    |> Maybe.map
                                        (\mdl ->
                                            let
                                                isOn =
                                                    mdl.controlValues
                                                        |> List.Extra.find
                                                            (\ctrl ->
                                                                case ctrl of
                                                                    SwitchVal kkk ->
                                                                        kkk.name == "enabled"

                                                                    _ ->
                                                                        False
                                                            )
                                                        |> Maybe.map
                                                            (\ctrl ->
                                                                case ctrl of
                                                                    SwitchVal enbld ->
                                                                        enbld.case_ == "on"

                                                                    _ ->
                                                                        False
                                                            )
                                                        |> Maybe.withDefault False

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
                                            OutputChanValues (Knob chan.name lvl) (Knob chan.name fltr) (Knob chan.name pn) isOn
                                        )
                                    |> Maybe.withDefault (OutputChanValues (Knob chan.name Nothing) (Knob chan.name Nothing) (Knob chan.name Nothing) False)
                        in
                        chanValues
                    )
    in
    chansWithValues |> List.sortBy (\cv -> cv.level.name)


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
