module Main exposing (init, main, subs, update, view)

import Array
import Browser
import Debug
import Html
import Html.Attributes as HA
import Http
import Json.Decode as JD
import Json.Encode as JE
import Knob exposing (KnobMsg, simpleKnobSvg, simpleSwitchSvg)
import List.Extra exposing (find, getAt)
import Matrix exposing (Matrix, generate, toArray)
import Maybe.Extra exposing (isJust)
import Model exposing (..)
import Msg exposing (Msg(..))
import Patch as P
import PinTable exposing (PinMsg(..), audioPanel, pinTable, setActivePin, setHoverPin)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import SynthiSchema as SS
import TestPage exposing (testPage)


type alias Flags =
    { synthiSchema : JD.Value
    , patches : JD.Value
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        mdl =
            initModel

        cmd =
            Cmd.batch
                [ getSchema
                , getPatches
                ]
    in
    ( mdl, cmd )


initModel : Model
initModel =
    { pinModel = PinTable.initModel
    , circleFill = "#0000ff"
    , hoverKnob = ( -1, -1 )
    , synthiSchema = Nothing
    , patches = Nothing
    , error = Nothing
    , activeAudioPin = Nothing
    , activeModules = Nothing
    , hoverAudioPin = Nothing
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
    Browser.document
        { init = init
        , subscriptions = subs
        , update = update
        , view = view
        }



-- type Msg
--     = KnobEvent KnobMsg
--     | PinEvent PinMsg
--     | GotSchema (Result Http.Error SS.SynthiSchema)
--     | GotPatches (Result Http.Error (List P.Patch))
--     | AudioPinClick ( Int, Int )
--     | AudioPinHover ( Int, Int )


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
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
    { title = "Synthi100"
    , body = testPage model
    }
