module Main exposing (init, main, subs, update, view)

import AboutPage
import String
import Array
import AudioModel exposing (emptyAudioModel)
import Browser
import Browser.Navigation exposing (Key, load, pushUrl, replaceUrl)
import Css exposing (..)
import Css.Global exposing (body, global, selector)
import Debug
import Footer exposing (footer)
import Header exposing (header)
import HiddenDownloader exposing (hiddenDownloader)
import HomePage
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Html.Styled exposing (Html, audio, node, text, iframe)
import Html.Styled.Attributes exposing (controls, id, type_, css, src)
import Http
import Json.Decode as JD
import Knob exposing (KnobMsg(..), simpleKnobSvg, simpleSwitchSvg)
import List.Extra exposing (find, findIndex, getAt)
import Matrix exposing (Matrix, generate, toArray)
import Maybe.Extra exposing (isJust)
import Model exposing (..)
import Msg exposing (Msg(..))
import Patch as P
import PatchPage
import PinTable exposing (..)
import Ports exposing (..)
import Routing exposing (Route(..), urlToRoute)
import Scroll exposing (..)
import Styles exposing (theDarkGray)
import SynthiSchema as SS
import Url exposing (Url)
import ViewModel as VM


type alias Flags =
    { synthiSchema : JD.Value
    , patches : JD.Value
    }


init : () -> Url -> Key -> ( Model, Cmd Msg )
init _ url key =
    let
        mdl =
            Model.initModel url key

        cmd =
            Cmd.batch
                [ getSchema
                , getPatches
                -- remove query parameters if redirected back from SSO
                , replaceUrl key url.path
                ]
    in
    ( mdl, cmd )


getSchema : Cmd Msg
getSchema =
    Http.get
        { url = "/api/v1/schema"
        , expect = Http.expectJson GotSchema SS.schemaDecoder
        }


getPatches : Cmd Msg
getPatches =
    Http.get
        { url = "/api/v1/patches"
        , expect = Http.expectJson GotPatches P.patchesDecoder
        }


subs : Model -> Sub Msg
subs model =
    -- currently unused
    -- scrollPort Scroll
    Sub.none


main =
    Browser.application
        { init = init
        , subscriptions = subs
        , update = update
        , view = view
        , onUrlRequest = \urlReq -> RequestedUrl urlReq
        , onUrlChange = \url -> UpdateUrl url
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateUrl url ->
            ( { model | currentRoute = urlToRoute url }, Cmd.none )

        RequestedUrl urlReq ->
            case urlReq of
                Browser.Internal url ->
                    -- hacky way to prevent whole page load if clicking
                    -- on '#' anchors or trying to download a file
                    if url.fragment /= Nothing || String.endsWith ".zip" url.path  then
                        ( model, load (Url.toString url))
                    else
                        ( model, pushUrl model.navKey (Url.toString url) )

                Browser.External href ->
                    ( model, load href )

        Scroll se ->
            ( { model | scroll = se }, Cmd.none )

        MovePatch patch step ->
            let
                currIndex =
                    model.filteredPatches |> List.Extra.findIndex (\p -> p.title == patch.title) |> Maybe.withDefault 0

                nextPos =
                    currIndex + step

                nextIndex =
                    nextPos |> (min (List.length model.filteredPatches) >> max 0)

                currPatch =
                    model.filteredPatches |> List.Extra.getAt nextIndex |> Maybe.withDefault patch

                url =
                    "/patch/" ++ currPatch.title
            in
            ( model, pushUrl model.navKey url )

        InputKnobEvent knobMsg ->
            case knobMsg of
                KnobHover s ->
                    ( { model | activeControl = ( Just s, Nothing ) }, Cmd.none )

                KnobOut ->
                    ( { model | activeControl = ( Nothing, Nothing ) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        OutputKnobEvent knobMsg ->
            case knobMsg of
                KnobHover s ->
                    ( { model | activeControl = ( Nothing, Just s ) }, Cmd.none )

                KnobOut ->
                    ( { model | activeControl = ( Nothing, Nothing ) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        PinEvent pinMsg ->
            -- may be useful :p
            ( model, Cmd.none )

        GotSchema schemaResult ->
            case schemaResult of
                Err err ->
                    ( model, Cmd.none )

                Ok schema ->
                    let
                        filters =
                            schema.attributes
                                |> List.map
                                    (\attr -> AttrFilter attr.name [])
                    in
                    ( { model | synthiSchema = Just schema
                              , attributeFilters = filters
                              , userInfo = schema.userInfo }, Cmd.none )

        GotPatches patches ->
            case patches of
                Err err ->
                    ( model, Cmd.none )

                Ok ptcs ->
                    ( { model | patches = Just ptcs, filteredPatches = filterPatches model.attributeFilters ptcs }, Cmd.none )

        Msg.PinClick panel ( x, y ) ->
            let
                mdl =
                    Maybe.map2
                        (\schema patches ->
                            let
                                patchTitle =
                                    getPatchTitle model.currentRoute

                                patch =
                                    patches
                                        |> List.Extra.find
                                            (\p -> p.title == patchTitle)
                                        |> Maybe.withDefault P.noPatch

                                ( im, om ) =
                                    pinToModules panel schema patch ( x, y )
                            in
                            case panel of
                                Audio ->
                                    { model
                                        | audioPinModel = setActivePin panel ( x, y ) (Just ( im, om )) model.audioPinModel
                                    }

                                Control ->
                                    { model
                                        | controlPinModel = setActivePin panel ( x, y ) (Just ( im, om )) model.controlPinModel
                                    }
                        )
                        model.synthiSchema
                        model.patches
                        |> Maybe.withDefault model
            in
            ( mdl, Cmd.none )

        Msg.PinHover panel ( x, y ) ->
            let
                mdl =
                    model.synthiSchema
                        |> Maybe.map
                            (\schema ->
                                case panel of
                                    Audio ->
                                        { model
                                            | audioPinModel = setHoverPin ( x, y ) model.audioPinModel
                                        }

                                    Control ->
                                        { model
                                            | controlPinModel = setHoverPin ( x, y ) model.controlPinModel
                                        }
                            )
                        |> Maybe.withDefault model
            in
            ( mdl, Cmd.none )

        Msg.PinOut panel ->
            let
                m =
                    case panel of
                        Audio ->
                            { model
                                | audioPinModel = setHoverPin ( -1, -1 ) model.audioPinModel
                            }

                        Control ->
                            { model
                                | controlPinModel = setHoverPin ( -1, -1 ) model.controlPinModel
                            }
            in
            ( m
            , Cmd.none
            )

        Play patch ->
            let
                ps =
                    model.filteredPatches
                        |> List.map
                            (\p ->
                                let
                                    am =
                                        if p.title == patch.title then
                                            case p.audioModel of
                                                Just auMod ->
                                                    Just { auMod | playing = True }

                                                Nothing ->
                                                    Just emptyAudioModel

                                        else
                                            Nothing
                                in
                                { p | audioModel = am }
                            )

                cmds =
                    play patch.title
            in
            ( { model | filteredPatches = ps }, cmds )

        Pause patch ->
            let
                ps =
                    model.filteredPatches
                        |> List.map
                            (\p ->
                                case p.audioModel of
                                    Just am ->
                                        let
                                            audioModel =
                                                Just { am | playing = False }
                                        in
                                        { p | audioModel = audioModel }

                                    Nothing ->
                                        { p | audioModel = Nothing }
                            )
            in
            ( { model | filteredPatches = ps }, pause patch.title )

        Ended patch ->
            let
                ps =
                    model.filteredPatches
                        |> List.map
                            (\p -> { p | audioModel = Nothing })
            in
            ( { model | filteredPatches = ps }, Cmd.none )

        Loop ->
            let
                newLoop =
                    not model.loop
            in
            ( { model | loop = newLoop }, loop newLoop )

        TimeUpdate patch seekerPosition ->
            let
                ps =
                    model.filteredPatches
                        |> List.map
                            (\p ->
                                case p.audioModel of
                                    Just am ->
                                        let
                                            audioModel =
                                                { am | seekerPosition = seekerPosition }
                                        in
                                        { p | audioModel = Just audioModel }

                                    Nothing ->
                                        p
                            )
            in
            ( { model | filteredPatches = ps }, Cmd.none )

        Seek patch mouseData ->
            let
                newTime =
                    patch.duration
                        / (mouseData.offsetWidth
                            / (mouseData.offsetX |> toFloat)
                          )

                audioModel =
                    case patch.audioModel of
                        Just am ->
                            Just { am | seekerPosition = newTime }

                        Nothing ->
                            Nothing

                ps =
                    model.filteredPatches
                        |> List.map
                            (\p ->
                                if p.title == patch.title then
                                    { p | audioModel = audioModel }

                                else
                                    p
                            )
            in
            ( { model | filteredPatches = ps }, setCurrentTime ( patch.title, newTime ) )

        Filter group value ->
            let
                attrFilters =
                    model.attributeFilters
                        |> List.map
                            (\af ->
                                if group == af.attrName then
                                    if af.selected |> findIndex (\s -> s == value) |> isJust then
                                        { af | selected = af.selected |> List.filter (\s -> not (s == value)) }

                                    else
                                        { af | selected = value :: af.selected }

                                else
                                    af
                            )

                fltrPtchs =
                    filterPatches attrFilters (model.patches |> Maybe.withDefault [])
            in
            ( { model | attributeFilters = attrFilters, filteredPatches = fltrPtchs }, Cmd.none )

        VolumeChange vol ->
            ( { model | volume = vol / 100 }, Cmd.none )

        SortBy sortBy ->
            let
                sortedPatches =
                    case model.sortOrder of
                        Ascending ->
                            model.filteredPatches |> sortAscendingBy sortBy

                        Descending ->
                            model.filteredPatches |> sortDescendingBy sortBy
            in
            ( { model | filteredPatches = sortedPatches, sortBy = sortBy }, Cmd.none )

        Sort direction ->
            let
                sortedPatches =
                    case direction of
                        Ascending ->
                            model.filteredPatches |> sortAscendingBy model.sortBy

                        Descending ->
                            model.filteredPatches |> sortDescendingBy model.sortBy
            in
            ( { model | filteredPatches = sortedPatches, sortOrder = direction }, Cmd.none )

        ToTop ->
            ( model, Ports.toTop () )

        Logout ->
            ( model, load "/saml/logout" )

getPatchTitle route =
    case route of
        Patch title ->
            title

        PatchGraphical title ->
            title

        _ ->
            ""

sortDescendingBy : String -> List P.Patch -> List P.Patch
sortDescendingBy prop ps =
    if prop == "title" then
        ps |> List.sortWith (\p1 p2 -> compare p2.title p1.title)
    else
        ps |> List.sortWith (\p1 p2 -> compare p2.duration p1.duration)


sortAscendingBy : String -> List P.Patch -> List P.Patch
sortAscendingBy prop ps =
    if prop == "title" then
        ps |> List.sortBy .title
    else
        ps |> List.sortBy .duration


noModuleFound : Int -> SS.Connection
noModuleFound x =
    { module_ = "no in", name = "no conn at " ++ (x |> String.fromInt) }


pinToModules : PinTable.Panel -> SS.SynthiSchema -> P.Patch -> ( Int, Int ) -> ( VM.Module, VM.Module )
pinToModules pin schema patch ( x, y ) =
    let
        panel =
            case pin of
                Audio ->
                    schema.audioPanel

                Control ->
                    schema.controlPanel

        yp =
            y + 60

        inConnection =
            panel |> getAt x |> Maybe.withDefault (noModuleFound x)

        inModuleName =
            inConnection.module_

        outConnection =
            panel |> getAt yp |> Maybe.withDefault (noModuleFound yp)

        outModuleName =
            outConnection.module_

        inModuleSchema =
            schema.modules |> find (\m -> m.name == inModuleName) |> Maybe.withDefault SS.noModule

        outModuleSchema =
            schema.modules |> find (\m -> m.name == outModuleName) |> Maybe.withDefault SS.noModule

        ims =
            patch.moduleSettings
                |> find (\m -> m.name == inModuleName)

        inModuleSettings =
            case ims of
                Just mdl ->
                    VM.Module inModuleName (patchToControls inModuleSchema.controls mdl.controlValues)

                Nothing ->
                    VM.Module inModuleName (emptyControls inModuleSchema.controls)

        oms =
            patch.moduleSettings
                |> find (\m -> m.name == outModuleName)

        outModuleSettings =
            case oms of
                Just mdl ->
                    VM.Module outModuleName (patchToControls outModuleSchema.controls mdl.controlValues)

                Nothing ->
                    VM.Module outModuleName (emptyControls outModuleSchema.controls)
    in
    ( inModuleSettings, outModuleSettings )


patchToControls : List SS.Control -> List P.Control -> List VM.Control
patchToControls scs pcs =
    scs
        |> List.map
            (\sc ->
                let
                    pctrl =
                        pcs
                            |> find
                                (\pc ->
                                    case pc of
                                        P.KnobVal k ->
                                            k.name == sc.name

                                        P.SwitchVal s ->
                                            s.name == sc.name
                                )

                    ctrl =
                        case pctrl of
                            Just (P.KnobVal { name, position }) ->
                                VM.KnobCtrl (VM.Knob name (Just position))

                            Just (P.SwitchVal { name, case_ }) ->
                                VM.SwitchCtrl (VM.Switch name (Just case_))

                            Nothing ->
                                if sc.type_ == "knob" then
                                    VM.KnobCtrl (VM.Knob sc.name Nothing)

                                else
                                    VM.SwitchCtrl (VM.Switch sc.name Nothing)
                in
                ctrl
            )


filterPatches : List AttrFilter -> List P.Patch -> List P.Patch
filterPatches afs ps =
    ps
        |> List.filter
            (\p ->
                let
                    selTypes =
                        afs |> List.Extra.find (\a -> a.attrName == "type") |> Maybe.withDefault { attrName = "", selected = [] }

                    selQuality =
                        afs |> List.Extra.find (\a -> a.attrName == "quality") |> Maybe.withDefault { attrName = "", selected = [] }

                    selRange =
                        afs |> List.Extra.find (\a -> a.attrName == "range") |> Maybe.withDefault { attrName = "", selected = [] }

                    selComplexity =
                        afs |> List.Extra.find (\a -> a.attrName == "complexity") |> Maybe.withDefault { attrName = "", selected = [] }

                    passType =
                        (selTypes.selected |> List.length)
                            == 0
                            || (selTypes.selected |> List.Extra.findIndex (\s -> s == p.attributes.type_) |> Maybe.map (\i -> i >= 0) |> Maybe.withDefault False)

                    passQuality =
                        (selQuality.selected |> List.length)
                            == 0
                            || (selQuality.selected |> List.Extra.findIndex (\s -> s == p.attributes.quality) |> Maybe.map (\i -> i >= 0) |> Maybe.withDefault False)

                    passRange =
                        (selRange.selected |> List.length)
                            == 0
                            || (selRange.selected |> List.Extra.findIndex (\s -> s == p.attributes.range) |> Maybe.map (\i -> i >= 0) |> Maybe.withDefault False)

                    passComplexity =
                        (selComplexity.selected |> List.length)
                            == 0
                            || (selComplexity.selected |> List.Extra.findIndex (\s -> s == p.attributes.complexity) |> Maybe.map (\i -> i >= 0) |> Maybe.withDefault False)
                in
                passType && passQuality && passRange && passComplexity
            )


emptyControls : List SS.Control -> List VM.Control
emptyControls scs =
    scs
        |> List.map
            (\sc ->
                if sc.type_ == "knob" then
                    VM.KnobCtrl (VM.Knob sc.name Nothing)

                else
                    VM.SwitchCtrl (VM.Switch sc.name Nothing)
            )


view : Model -> Browser.Document Msg
view model =
    let
        downloader = hiddenDownloader model.downloadFile
        page =
            case model.currentRoute of
                Database ->
                    HomePage.page model

                Patch patchTitle ->
                    PatchPage.page False patchTitle model

                PatchGraphical patchTitle ->
                    PatchPage.page True patchTitle model

                About ->
                    AboutPage.page model

    in
    { title = "Synthi100"
    , body =
        -- show content only if API responded with all data
        if model.synthiSchema == Nothing || model.patches == Nothing then
           []
        else
           [ globalCSS
           , fontImport
           , header model
           , page
           , footer
           , downloader
           ]
           |> List.map Html.Styled.toUnstyled
    }


globalCSS : Html Msg
globalCSS =
    global
        [ body
            [ backgroundColor (hex "000000")
            , color (hex "4A90E2")
            , margin (px 0)
            , fontFamilies [ "Metropolis" ]
            , maxWidth (px 1280)
            , margin auto
            , paddingTop (px 30)
            , height (pct 100)
            ]
        , Css.Global.h2
            [ all unset
            , display block
            , margin2 (px 11) (px 0)
            , letterSpacing (px 0.5)
            , fontSize (px 18)
            , fontWeight bold
            ]
        , selector "#about p:first-child"
            [ marginTop (px 0) ]
        , selector "#about p"
            [ margin2 (px 10) (px 0) ]
        , selector "#credits p"
            [ margin2 (px 0) (px 0)]
        , selector "html"
            [ height (pct 100)
              -- fix jumpy initial loading
            , marginLeft (calc (vw 100) minus (pct 100))
            ]
        ]


fontImport : Html Msg
fontImport =
    node "style" [ type_ "text/css" ] [ text """
        @font-face {
            font-family: "Metropolis";
            src: url(/Metropolis-Medium.woff);
        }
        @font-face {
            font-family: "Metropolis";
            src: url(/Metropolis-Bold.woff);
            font-weight: bold;
        }
        @font-face {
            font-family: "Metropolis";
            src: url(/Metropolis-SemiBold.woff);
            font-weight: 600;
        }
    """ ]
