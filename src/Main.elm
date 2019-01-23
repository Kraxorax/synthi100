module Main exposing (Knob10, Model, Msg(..), init, knob10Svg, knobSvg, knobValueToAngle, main, subs, table, update, view)

import Array
import Browser
import Html
import Matrix exposing (Matrix, generate, toArray)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)


init : () -> ( Model, Cmd msg )
init flags =
    ( { circleFill = "#0000ff" }, Cmd.none )


table : Matrix (Svg msg)
table =
    generate 8 7 (\x y -> knob10Svg ( x * 50, y * 80 ) True (toFloat (x + y)))


pinTable : Matrix (Svg msg)
pinTable =
    generate 60
        60
        (\x y ->
            let
                yPos =
                    if y >= 30 then
                        y * 8 + 15

                    else
                        y * 8

                xPos =
                    x * 8
            in
            pinSvg ( xPos, yPos ) (shouldDarkenPin x y)
        )


shouldDarkenPin : Int -> Int -> Bool
shouldDarkenPin x y =
    (x // 4 |> isEven)
        && (if y < 30 then
                y // 3 |> isEven

            else
                y // 3 |> isEven |> not
           )


isEven x =
    case modBy 2 x of
        0 ->
            True

        _ ->
            False


subs : Model -> Sub msg
subs model =
    Sub.none


main =
    Browser.element { init = init, subscriptions = subs, update = update, view = view }


type Msg
    = OverCircle
    | OutCircle


type alias Model =
    { circleFill : String
    }


type alias Knob10 =
    { value : Float
    }


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        OverCircle ->
            ( { model | circleFill = "#00ff00" }, Cmd.none )

        OutCircle ->
            ( { model | circleFill = "#0000ff" }, Cmd.none )


knobValueToAngle : Float -> Float
knobValueToAngle x =
    30 + x * 30


pinSvg : ( Int, Int ) -> Bool -> Svg msg
pinSvg ( xp, yp ) isDarken =
    rect
        [ width "4"
        , height "4"
        , rx "2"
        , ry "2"
        , x (String.fromInt xp)
        , y (String.fromInt yp)
        , fill
            (if isDarken then
                "#333333"

             else
                "#999999"
            )
        ]
        []


knob10Svg : ( Int, Int ) -> Bool -> Float -> Svg msg
knob10Svg ( kx, ky ) active val =
    let
        rotation =
            "rotate(" ++ String.fromFloat (knobValueToAngle val) ++ " 20 20)"
    in
    svg
        [ x (String.fromInt kx)
        , y (String.fromInt ky)
        , width "40"
        , height "70"
        ]
        [ text_
            [ dx "15"
            , dy "15"
            ]
            [ text (String.fromFloat val)
            ]
        , svg [ y "30" ]
            [ knobSvg rotation
            ]
        ]


knobSvg : String -> Svg msg
knobSvg rotation =
    svg
        [ width "40"
        , height "40"
        , viewBox "0 0 40 40"
        , transform rotation
        ]
        [ circle
            [ cx "20"
            , cy "20"
            , r "20"
            , fill "#333333"
            ]
            []
        , rect
            [ x "18"
            , y "20"
            , rx "2"
            , ry "2"
            , width "4"
            , height "20"
            , fill "#999999"
            ]
            []
        ]


view : Model -> Html.Html Msg
view model =
    let
        knobs =
            table |> toArray |> Array.toList
    in
    Html.div []
        [ svg
            [ width "1400"
            , height "2400"
            , viewBox "0 0 1400 2400"
            ]
            [ svg [ x "0", y "0" ]
                knobs
            , svg [ x "0", y "560" ]
                (pinTable
                    |> toArray
                    |> Array.toList
                )
            ]
        ]
