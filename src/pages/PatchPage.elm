module PatchPage exposing (page)

import Components exposing (..)
import Css exposing (..)
import Css.Global exposing (body, global)
import Html.Styled as HS exposing (..)
import Html.Styled.Attributes exposing (css, href, src, type_, download)
import Html.Styled.Events exposing (..)
import Knob exposing (..)
import List.Extra exposing (find)
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
import Scroll exposing (onScroll)

page : Bool -> String -> Model -> Html Msg
page showGraphical patchTitle model =
    let
        patch =
            model.patches
                |> Maybe.map (find (\p -> p.title == patchTitle) >> Maybe.withDefault noPatch)
                |> Maybe.withDefault noPatch

        ( view, bgColor ) =
            if showGraphical then
                ( [ graphical model patch, cssHaxor ], "9b9b9b" )

            else
                ( controls model patch
                    :: [ waveAndText model patch, cssHaxor ]
                , "000"
                )
    in
    div
        [ css
            [ Css.color (hex "ffffff")
            , Css.paddingLeft (px 31)
            , Css.paddingTop (px 18)
            , backgroundColor (hex bgColor)
            , backgroundClip paddingBox
            , displayFlex
            ]
        ]
        view


waveOrGraphCss : Style
waveOrGraphCss =
    batch
        [ borderTop2 (px 1) solid
        , borderBottom2 (px 1) solid
        , padding2 (px 12) (px 0)
        , height (px 56)
        ]


waveOrGraphBlackCss : Style
waveOrGraphBlackCss =
    batch
        [ borderTop3 (px 2) solid (hex "000")
        , borderBottom3 (px 2) solid (hex "000")
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

        wogCss =
            case model.currentRoute of
                R.PatchGraphical _ ->
                    waveOrGraphBlackCss

                _ ->
                    waveOrGraphCss
    in
    div [ css [ wogCss ] ]
        [ a [ href waveTextUrl, css [ Css.float left, linkUnstyle ] ]
            [ img [ src "/wave-textual.svg", css [ marginBottom (px 10) ] ] []
            , br [] []
            , span [] [ text "audio / text" ]
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
        , display inlineBlock
        ]


audioControlsCss : Style
audioControlsCss =
    batch
        [ height (px 280)
        , padding2 (px 20) (px 0)
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
        , muteBttn model.muted
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
        , onClick (Mute (not isMute))
        ]
        []


headerCss : Style
headerCss =
    batch
        [ fontSize (px 24)
        , fontWeight bold
        , letterSpacing (px 0.5)
        ]


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
        , div
            [ css
                [ padding2 (px 18) (px 0)
                , margin (px 0)
                , borderTop2 (px 1) solid
                , borderBottom2 (px 1) solid
                , displayFlex
                , alignItems center
                ]
            ]
            [ dlBttn
            , a [ href patch.download
                , download ""
                , css [ linkUnstyle
                    , paddingLeft (px 15)
                    ]
                ] 
                [ text "download audio + text"] 
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
    div [ css [ Css.height (px 182) ] ]
        [ h1 [ css [ Css.fontSize (px 48) ] ] [ text patch.title ]
        , p [ css [ marginBottom (px 20) ] ] [ text ("duration: " ++ (patch.duration |> String.fromFloat)) ]
        , p [ css [ marginBottom (px 20) ] ] [ text (patch.attributeValues |> String.join " / ") ]
        ]


waveAndText : Model -> Patch -> Html Msg
waveAndText model patch =
    div [ css [ Css.width (pct 66), float left ] ]
        [ div [ css [ Css.width (pct 100), Css.height (px 386), marginBottom (px 15) ] ]
            [ waveformSeeker True patch ]
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
    div [ css [ displayFlex, Css.width (pct 100) ] ]
        [ graphicControls model patch
        , div [ css [ flex (int 2) ] ]
            [ pin model patch
            , outputChannels model patch
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
        , position sticky
        , top (px 18)
        ]


graphicControls : Model -> Patch -> Html Msg
graphicControls model patch =
    div [ css [ controlsGraphicalCss ] ]
        [ waveOrGraph model patch.title
        , div
            [ css
                [ Css.height (px 60)
                , color (hex "000")
                , borderBottom3 (px 2) solid (hex "000")
                ]
            ]
            [ span [ css [ headerCss, display inlineBlock, marginTop (px 18) ] ] [ text "Audio signals" ] ]
        , patchMeta patch
        , parameters model patch
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


knob : Model -> Patch -> Html Msg
knob model patch =
    let
        ( im, om ) =
            model.activeModules |> Maybe.withDefault ( Module "" [], Module "" [] )

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
            [ div [ css [ Css.width (pct 50), display inlineBlock, textAlign center ] ]
                [ span [] [ text im.name ] ]
            , div [ css [ Css.width (pct 50), display inlineBlock, textAlign center ] ]
                [ span [] [ text (iac |> Maybe.withDefault "") ] ]
            ]
        , div [ css [ marginBottom (px 22) ] ]
            [ HS.map (\kmsg -> InputKnobEvent kmsg) (controlsToKnobSvg False im.controls) ]
        
        , div
            [ css
                [ Css.width (pct 100)
                , margin2 (px 15) (px 0)
                ]
            ]
            [ div [ css [ Css.width (pct 50), display inlineBlock, textAlign center ] ]
                [ span [] [ text om.name ] ]
            , div [ css [ Css.width (pct 50), display inlineBlock, textAlign center ] ]
                [ span [] [ text (oac |> Maybe.withDefault "") ] ]
            ]
        , div [ ]
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


pin : Model -> Patch -> HS.Html Msg
pin model patch =
    let
        -- ( audioInModuleText, audioOutModuleText ) =
        --     model.synthiSchema
        --         |> Maybe.map
        --             (getModulesText Audio model.audioPinModel)
        --         |> Maybe.withDefault ( "", "" )

        -- ( controlInModuleText, controlOutModuleText ) =
        --     model.synthiSchema
        --         |> Maybe.map
        --             (getModulesText Control model.controlPinModel)
        --         |> Maybe.withDefault ( "", "" )

        ( inModuleText, outModuleText ) =
            model.synthiSchema
                |> Maybe.map
                    (getModulesText model.pinModel)
                |> Maybe.withDefault ( "", "" )

        (audioModel, controlModel) = case model.pinModel.panel of
                                        Audio -> ( model.pinModel, PinTable.initModel )
                                        Control -> ( PinTable.initModel, model.pinModel )
    in
    div [ css [ Css.width (px 760) ] ]
        [ div [ css [ Css.height (px 58), position sticky, top (px 0), paddingTop (px 18), paddingLeft (px 10), backgroundColor (hex "9b9b9b") ] ]
            [ moduleStrip outModuleText (px 40) (px 0)
            , moduleStrip inModuleText (px 0) (px -14)
            ]
        , div [ css [ marginBottom (px 40) ] ] 
            [ HS.map (reactToAudioPinEvent Audio) (audioPanel patch.audioPins audioModel) ]
        -- , div [ css [ Css.height (px 58) ] ]
        --     [ moduleStrip controlOutModuleText (px 40) (px 0)
        --     , moduleStrip controlInModuleText (px 0) (px -14)
        --     ]
        , div [ css [ marginBottom (px 40) ] ] 
            [ HS.map (reactToAudioPinEvent Control) (audioPanel patch.controlPins controlModel) ]
        ]


moduleStrip : String -> Px -> Px -> Html Msg
moduleStrip txt rm tm =
    div
        [ css
            [ Css.height (px 50)
            , Css.width (px 327)
            , borderBottom3 (px 1) solid (hex "000")
            , borderTop3 (px 1) solid (hex "000")
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


-- dots4 : Html PinMsg
-- dots4 =
--     Svg.svg [ Svg.width "72", Svg.height "50" ]
--         (List.range 0 3 |> List.map (\x -> pinSvg ( x, 0 ) "" Nothing initModel))


-- dots3 : Html PinMsg
-- dots3 =
--     Svg.svg [ Svg.width "72", Svg.height "70" ]
--         (List.range 0 2 |> List.map (\y -> pinSvg ( 0, y ) "" Nothing initModel))


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
        (div [ css [ Css.height (px 50), Css.width (px 327), borderTop2 (px 1) solid, marginLeft (px 33) ] ]
            [ span [ css [ display inlineBlock, marginTop (px 16) ] ] [ text "OUTPUT CHANNELS" ]
            ]
            :: [ outputPanel channels ]
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
