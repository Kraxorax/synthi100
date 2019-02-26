module Main exposing (init, main, subs, update, view)

import AboutPage
import Array
import AudioPlayer exposing (AudioModel, AudioMsg, audioUpdate, emptyAudioModel)
import Browser
import Browser.Navigation exposing (Key, load, pushUrl)
import CreditsPage
import Css exposing (..)
import Css.Global exposing (body, global)
import Debug
import Header exposing (header)
import HomePage
import Html as H
import Html.Attributes as HA
import Html.Styled exposing (Html, audio)
import Html.Styled.Attributes exposing (controls, id)
import Http
import Json.Decode as JD
import Knob exposing (KnobMsg, simpleKnobSvg, simpleSwitchSvg)
import List.Extra exposing (find, getAt)
import Matrix exposing (Matrix, generate, toArray)
import Maybe.Extra exposing (isJust)
import Model exposing (..)
import Msg exposing (Msg(..))
import Patch as P
import PinTable exposing (PinMsg(..), audioPanel, pinTable, setActivePin, setHoverPin)
import Ports exposing (..)
import Routing exposing (Route(..), urlToRoute)
import SynthiSchema as SS
import TestPage exposing (testPage)
import Url exposing (Url)


type alias Flags =
    { synthiSchema : JD.Value
    , patches : JD.Value
    }


init : () -> Url -> Key -> ( Model, Cmd Msg )
init _ url key =
    let
        mdl =
            initModel key

        cmd =
            Cmd.batch
                [ getSchema
                , getPatches
                ]
    in
    ( mdl, cmd )


initModel : Key -> Model
initModel key =
    { navKey = key
    , currentRoute = Database
    , pinModel = PinTable.initModel
    , circleFill = "#0000ff"
    , hoverKnob = ( -1, -1 )
    , synthiSchema = Nothing
    , patches = Nothing
    , error = Nothing
    , activeAudioPin = Nothing
    , activeModules = Nothing
    , hoverAudioPin = Nothing
    , audio = emptyAudioModel
    , nowPlaying = Nothing
    }


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


subs : Model -> Sub msg
subs model =
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
                    ( model, pushUrl model.navKey (Url.toString url) )

                Browser.External href ->
                    ( model, load href )

        KnobEvent knobMsg ->
            ( model, Cmd.none )

        PinEvent pinMsg ->
            ( { model | pinModel = PinTable.update pinMsg model.pinModel }, Cmd.none )

        GotSchema schemaResult ->
            case schemaResult of
                Err err ->
                    ( model, Cmd.none )

                Ok schema ->
                    ( { model | synthiSchema = Just schema }, Cmd.none )

        GotPatches patches ->
            case patches of
                Err err ->
                    ( model, Cmd.none )

                Ok ptcs ->
                    ( { model | patches = Just ptcs }, Cmd.none )

        AudioPinClick ( x, y ) ->
            let
                mdl =
                    case model.synthiSchema of
                        Just schema ->
                            let
                                patch =
                                    model.patches |> Maybe.withDefault [] |> List.head |> Maybe.withDefault P.noPatch

                                arrModules =
                                    schema.audioPanel |> Array.fromList

                                intoPos =
                                    y + 60

                                outPos =
                                    x + 0

                                ( om, im ) =
                                    pinToModules schema patch ( outPos, intoPos )

                                activeModulesText =
                                    om.name ++ " >> " ++ im.name

                                actAudMods =
                                    Just
                                        { out = (Array.get outPos arrModules |> Maybe.withDefault { name = "No module found at : " ++ (outPos |> String.fromInt), module_ = "none" }).name
                                        , into = (Array.get intoPos arrModules |> Maybe.withDefault { name = "No module found at : " ++ (intoPos |> String.fromInt), module_ = "none" }).name
                                        }
                            in
                            { model
                                | activeAudioPin = actAudMods
                                , activeModules = Just ( om, im )
                                , pinModel = setActivePin ( x, y ) model.pinModel
                            }

                        Nothing ->
                            model
            in
            ( mdl, Cmd.none )

        AudioPinHover ( x, y ) ->
            let
                mdl =
                    case model.synthiSchema of
                        Just schema ->
                            let
                                arrModules =
                                    schema.audioPanel |> Array.fromList

                                intoPos =
                                    y + 60

                                outPos =
                                    x + 0

                                actAudMods =
                                    Just
                                        { out = (Array.get outPos arrModules |> Maybe.withDefault { name = "No module found at : " ++ (outPos |> String.fromInt), module_ = "none" }).name
                                        , into = (Array.get intoPos arrModules |> Maybe.withDefault { name = "No module found at : " ++ (intoPos |> String.fromInt), module_ = "none" }).name
                                        }
                            in
                            { model
                                | hoverAudioPin = actAudMods
                                , pinModel = setHoverPin ( x, y ) model.pinModel
                            }

                        Nothing ->
                            model
            in
            ( mdl, Cmd.none )

        AudioPlayer audioMsg ->
            let
                ( audioModel, audioCommand ) =
                    audioUpdate audioMsg model.audio
            in
            ( { model | audio = audioModel }, Cmd.map (\ac -> AudioPlayer ac) audioCommand )

        Play patch ->
            ( { model | nowPlaying = Just ( patch, emptyAudioModel ) }, play patch.title )

        Ended patch ->
            ( { model | nowPlaying = Nothing }, Cmd.none )

        TimeUpdate patch seekerPosition ->
            let
                audioModel =
                    case model.nowPlaying of
                        Just ( p, am ) ->
                            { am | seekerPosition = seekerPosition }

                        Nothing ->
                            emptyAudioModel
            in
            ( { model | nowPlaying = Just ( patch, audioModel ) }, Cmd.none )


pinToModules : SS.SynthiSchema -> P.Patch -> ( Int, Int ) -> ( Module, Module )
pinToModules schema patch ( x, y ) =
    let
        inConnection =
            schema.audioPanel |> getAt x |> Maybe.withDefault { name = "", module_ = "no in" }

        inModuleName =
            inConnection.module_

        outConnection =
            schema.audioPanel |> getAt y |> Maybe.withDefault { name = "", module_ = "no out" }

        outModuleName =
            outConnection.module_

        inModule =
            schema.modules |> find (\m -> m.name == inModuleName) |> Maybe.withDefault SS.noModule

        outModule =
            schema.modules |> find (\m -> m.name == outModuleName) |> Maybe.withDefault SS.noModule

        ims =
            patch.moduleSettings
                |> find (\m -> m.name == inModuleName)

        inModuleSettings =
            case ims of
                Just mdl ->
                    Module inModuleName (patchToControls inModule.controls mdl.controlValues)

                Nothing ->
                    Module inModuleName (emptyControls inModule.controls)

        oms =
            patch.moduleSettings
                |> find (\m -> m.name == inModuleName)

        outModuleSettings =
            case oms of
                Just mdl ->
                    Module outModuleName (patchToControls outModule.controls mdl.controlValues)

                Nothing ->
                    Module outModuleName (emptyControls outModule.controls)
    in
    ( outModuleSettings, inModuleSettings )


patchToControls : List SS.Control -> List P.Control -> List Control
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
                                KnobCtrl (Knob name (Just position))

                            Just (P.SwitchVal { name, case_ }) ->
                                SwitchCtrl (Switch name (Just case_))

                            Nothing ->
                                if sc.type_ == "knob" then
                                    KnobCtrl (Knob sc.name Nothing)

                                else
                                    SwitchCtrl (Switch sc.name Nothing)
                in
                ctrl
            )


emptyControls : List SS.Control -> List Control
emptyControls scs =
    scs
        |> List.map
            (\sc ->
                if sc.type_ == "knob" then
                    KnobCtrl (Knob sc.name Nothing)

                else
                    SwitchCtrl (Switch sc.name Nothing)
            )


view : Model -> Browser.Document Msg
view model =
    let
        page =
            case model.currentRoute of
                Database ->
                    HomePage.page model

                Credits ->
                    CreditsPage.page model

                About ->
                    AboutPage.page model
    in
    { title = "Synthi100"
    , body =
        [ globalCSS
        , header
        , page
        ]
            |> List.map Html.Styled.toUnstyled
    }


globalCSS : Html Msg
globalCSS =
    global
        [ body
            [ backgroundColor (hex "000000")
            , color (hex "4A90E2")
            ]
        ]
