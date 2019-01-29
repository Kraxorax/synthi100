module Main exposing (Model, Msg(..), init, main, subs, table, update, view)

import Array
import Browser
import Debug
import Html
import Knob exposing (KnobMsg, knob10Svg)
import Matrix exposing (Matrix, generate, toArray)
import PinTable exposing (PinMsg, pinTable)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)


init : () -> ( Model, Cmd msg )
init flags =
    ( { pinModel = PinTable.initModel, circleFill = "#0000ff", hoverKnob = ( -1, -1 ) }, Cmd.none )


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
    Browser.document { init = init, subscriptions = subs, update = update, view = view }


type Msg
    = KnobEvent KnobMsg
    | PinEvent PinMsg


type alias Model =
    { circleFill : String
    , hoverKnob : ( Int, Int )
    , pinModel : PinTable.PinModel
    }


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        KnobEvent knobMsg ->
            ( model, Cmd.none )

        PinEvent pinMsg ->
            ( { model | pinModel = PinTable.update pinMsg model.pinModel }, Cmd.none )


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
    in
    [ Html.div []
        [ svg
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
