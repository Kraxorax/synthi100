module Main exposing (Model, Msg(..), init, main, subs, table, update, view)

import Array
import Browser
import Debug
import Html
import Html.Attributes as HA
import Http
import Json.Decode as JD
import Json.Encode as JE
import Knob exposing (KnobMsg, knob10Svg)
import Matrix exposing (Matrix, generate, toArray)
import Patch as P
import PinTable exposing (PinMsg, pinTable)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import SynthiSchema as SS


type alias Flags =
    { synthiSchema : JD.Value
    , patches : JD.Value
    }


init : Flags -> ( Model, Cmd Msg )
init { synthiSchema, patches } =
    case JD.decodeValue SS.schemaDecoder synthiSchema of
        Ok schema ->
            case JD.decodeValue P.patchesDecoder patches of
                Ok p ->
                    ( { initModel | synthiSchema = Just schema, patches = Just p }
                    , Cmd.none
                    )

                Err e ->
                    ( { initModel | error = Just (Debug.log "err" (JD.errorToString e)) }, Cmd.none )

        Err err ->
            ( { initModel | error = Just (Debug.log "err" (JD.errorToString err)) }, Cmd.none )


initModel : Model
initModel =
    { pinModel = PinTable.initModel
    , circleFill = "#0000ff"
    , hoverKnob = ( -1, -1 )
    , synthiSchema = Nothing
    , patches = Nothing
    , error = Nothing
    }


getSchema : Cmd Msg
getSchema =
    Http.get
        { url = "http://http://localhost:8081/api/v1/schema"
        , expect = Http.expectJson GotSchema SS.schemaDecoder
        }


table : ( Int, Int ) -> Matrix (Html.Html KnobMsg)
table ( hx, hy ) =
    generate 8
        7
        (\x y ->
            let
                isActive =
                    x == hx && y == hy
            in
            knob10Svg
                ( x, y )
                isActive
                (toFloat (x + y))
        )


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


type Msg
    = KnobEvent KnobMsg
    | PinEvent PinMsg
    | GotSchema (Result Http.Error SS.SynthiSchema)


type alias Model =
    { circleFill : String
    , hoverKnob : ( Int, Int )
    , pinModel : PinTable.PinModel
    , synthiSchema : Maybe SS.SynthiSchema
    , patches : Maybe (List P.Patch)
    , error : Maybe String
    }


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
                    ( { model | synthiSchema = Just (Debug.log "s" schema) }, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "Synthi100"
    , body = page model
    }


page : Model -> List (Html.Html Msg)
page model =
    let
        knobs =
            table model.hoverKnob |> toArray |> Array.toList

        ss =
            case model.synthiSchema of
                Just s ->
                    s

                Nothing ->
                    { attributes = [], audioPanel = [], controlPanel = [], modules = [] }
    in
    [ Html.div []
        [ renderSchema model
        , svg
            [ width "1400"
            , height "2400"
            , viewBox "0 0 1400 2400"
            ]
            [ Html.map (\knobMsg -> KnobEvent knobMsg)
                (svg [ x "0", y "0" ]
                    knobs
                )
            , Html.map (\pinMsg -> PinEvent pinMsg)
                (svg [ x "0", y "560" ]
                    (pinTable model.pinModel
                        |> toArray
                        |> Array.toList
                    )
                )
            ]
        ]
    ]


renderSchema : Model -> Html.Html Msg
renderSchema model =
    let
        ss =
            case model.synthiSchema of
                Just s ->
                    s

                Nothing ->
                    { attributes = [], audioPanel = [], controlPanel = [], modules = [] }
    in
    Html.div []
        (ss.modules
            |> List.map
                (\m ->
                    Html.div []
                        (List.append
                            [ Html.text m.name, Html.br [] [] ]
                            (m.controls
                                |> List.map
                                    (\c ->
                                        Html.li []
                                            [ Html.text (c.name ++ " - " ++ c.type_) ]
                                    )
                            )
                        )
                )
        )
