module Main exposing (init, main, subs, update, view)

import AboutPage
import Array
import AudioModel exposing (emptyAudioModel)
import Browser
import Browser.Navigation exposing (Key, load, pushUrl)
import CreditsPage
import Css exposing (..)
import Css.Global exposing (body, global, selector)
import Debug
import Header exposing (header)
import HomePage
import Html as H
import Html.Events as HE
import Html.Attributes as HA
import Html.Styled exposing (Html, audio, node, text)
import Html.Styled.Attributes exposing (controls, id, type_)
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

        Scroll se ->
            let
                _ = Debug.log "se" se
            in
            
            (model, Cmd.none)

        MovePatch patch step ->
            let
                currPatch =
                    model.patches
                        |> Maybe.map
                            (\patches ->
                                let
                                    currIndex =
                                        patches |> List.Extra.findIndex (\p -> p.title == patch.title) |> Maybe.withDefault 0

                                    nextPos =
                                        currIndex + step

                                    nextIndex =
                                        if nextPos < 0 then
                                            0

                                        else if nextPos >= List.length patches then
                                            currIndex

                                        else
                                            nextPos
                                in
                                patches |> List.Extra.getAt (currIndex + step) |> Maybe.withDefault patch
                            )
                        |> Maybe.withDefault P.noPatch

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
                    ( { model | synthiSchema = Just schema, attributeFilters = filters }, Cmd.none )

        GotPatches patches ->
            case patches of
                Err err ->
                    ( model, Cmd.none )

                Ok ptcs ->
                    ( { model | patches = Just ptcs }, Cmd.none )

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
                            { model
                                | activeModules = Just ( im, om )
                                , pinModel = setActivePin panel ( x, y ) model.controlPinModel
                            }
                            -- case panel of
                            --     PinTable.Audio ->
                            --         { model
                            --             | activeModules = Just ( im, om )
                            --             , audioPinModel = setActivePin ( x, y ) model.audioPinModel
                            --         }

                            --     PinTable.Control ->
                            --         { model
                            --             | activeModules = Just ( im, om )
                            --             , controlPinModel = setActivePin ( x, y ) model.controlPinModel
                            --         }
                        )
                        model.synthiSchema
                        model.patches
                        |> Maybe.withDefault model
            in
            ( mdl, Cmd.none )

        Msg.PinHover pin ( x, y ) ->
            let
                mdl =
                    model.synthiSchema
                        |> Maybe.map
                            (\schema ->
                                { model
                                    | pinModel = setHoverPin pin ( x, y ) model.pinModel
                                }
                                -- case pin of
                                --     PinTable.Audio ->
                                --         { model
                                --             | audioPinModel = setHoverPin ( x, y ) model.audioPinModel
                                --         }

                                --     PinTable.Control ->
                                --         { model
                                --             | controlPinModel = setHoverPin ( x, y ) model.controlPinModel
                                --         }
                            )
                        |> Maybe.withDefault model
            in
            ( mdl, Cmd.none )

        Msg.PinOut panel ->
            ( { model
                -- | controlPinModel = setHoverPin ( -1, -1 ) model.controlPinModel
                -- , audioPinModel = setHoverPin ( -1, -1 ) model.audioPinModel
                | pinModel = setHoverPin panel ( -1, -1) model.pinModel
              }
            , Cmd.none
            )

        Play patch ->
            let
                ps =
                    model.patches
                        |> Maybe.withDefault []
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
            ( { model | patches = Just ps }, cmds )

        Pause patch ->
            let
                ps =
                    model.patches
                        |> Maybe.withDefault []
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
            ( { model | patches = Just ps }, pause patch.title )

        Ended patch ->
            let
                ps =
                    model.patches
                        |> Maybe.withDefault []
                        |> List.map
                            (\p -> { p | audioModel = Nothing })
            in
            ( { model | patches = Just ps }, Cmd.none )

        TimeUpdate patch seekerPosition ->
            let
                ps =
                    model.patches
                        |> Maybe.withDefault []
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
            ( { model | patches = Just ps }, Cmd.none )

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
                    model.patches
                        |> Maybe.withDefault []
                        |> List.map
                            (\p ->
                                if p.title == patch.title then
                                    { p | audioModel = audioModel }

                                else
                                    p
                            )
            in
            ( { model | patches = Just ps }, setCurrentTime ( patch.title, newTime ) )

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
            in
            ( { model | attributeFilters = attrFilters }, Cmd.none )

        VolumeChange vol ->
            ( { model | volume = vol / 100 }, Cmd.none )

        Mute is ->
            ( { model | muted = is }, Cmd.none )

        SortBy sortBy ->
            let
                sortedPatches =
                    model.patches
                        |> Maybe.map
                            (\patches ->
                                case model.sortOrder of
                                    Ascending ->
                                        patches |> sortAscendingBy sortBy

                                    Descending ->
                                        patches |> sortDescendingBy sortBy
                            )
            in
            ( { model | patches = sortedPatches, sortBy = sortBy }, Cmd.none )

        Sort direction ->
            let
                sortedPatches =
                    model.patches
                        |> Maybe.map
                            (\patches ->
                                case direction of
                                    Ascending ->
                                        patches |> sortAscendingBy model.sortBy

                                    Descending ->
                                        patches |> sortDescendingBy model.sortBy
                            )
            in
            ( { model | patches = sortedPatches, sortOrder = direction }, Cmd.none )


getPatchTitle route =
    case route of
        Patch title ->
            title

        PatchGraphical title ->
            title

        _ ->
            ""


sortDescendingBy : String -> List P.Patch -> List P.Patch
sortDescendingBy s =
    sortAscendingBy s >> List.reverse


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
        page =
            case model.currentRoute of
                Database ->
                    HomePage.page model

                Patch patchTitle ->
                    PatchPage.page False patchTitle model

                PatchGraphical patchTitle ->
                    PatchPage.page True patchTitle model

                Credits ->
                    CreditsPage.page model

                About ->
                    AboutPage.page model
    in
    { title = "Synthi100"
    , body =
        [ globalCSS
        , fontImport
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
            , margin (px 0)
            , fontFamilies [ "Metropolis" ]
            , maxWidth (px 1440)
            , margin auto
            , displayFlex
            , flexDirection column
            , height (pct 100)
            ]
        , selector "html"
            [
                height (pct 100)
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
    """ ]

