module PinTable exposing (PinModel, PinMsg(..), audioPanel, initModel, pinTable, setActivePin, setHoverPin, update)

import Array
import Html
import Html.Attributes exposing (style)
import List.Extra exposing (find)
import Matrix exposing (Matrix, generate)
import Patch as P
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import SynthiSchema as SS
import Tuple exposing (first, second)


color =
    { offL =
        "#dddddd"
    , offD =
        "#aaaaaa"
    , hover =
        "#66AAFF"
    , active =
        "#FF9999"
    }


type PinMsg
    = PinIn ( Int, Int )
    | PinOut
    | PinClick ( Int, Int )


type alias PinModel =
    { hoverPin : ( Int, Int )
    , activePin : ( Int, Int )
    }


initModel : PinModel
initModel =
    { hoverPin = ( -1, -1 )
    , activePin = ( -1, -1 )
    }


update : PinMsg -> PinModel -> PinModel
update msg model =
    case msg of
        PinIn ( x, y ) ->
            { model | hoverPin = ( x, y ) }

        PinOut ->
            { model | hoverPin = ( -1, -1 ) }

        PinClick ( x, y ) ->
            { model | activePin = ( x, y ) }


setActivePin : ( Int, Int ) -> PinModel -> PinModel
setActivePin xy model =
    { model | activePin = xy }


setHoverPin : ( Int, Int ) -> PinModel -> PinModel
setHoverPin xy model =
    { model | hoverPin = xy }


pinSvg : ( Int, Int ) -> String -> PinModel -> Svg PinMsg
pinSvg ( xp, yp ) pinColor model =
    let
        yPos =
            (if yp >= 30 then
                yp * 8 + 15

             else
                yp * 8
            )
                + 3
                |> String.fromInt

        xPos =
            (xp * 8) + 3 |> String.fromInt
    in
    circle
        [ r "3"
        , cx xPos
        , cy yPos
        , fill pinColor
        , onMouseOver (PinIn ( xp, yp ))
        , onMouseOut PinOut
        , onMouseDown (PinClick ( xp, yp ))
        ]
        []


coordsToPinPos : ( Int, Int ) -> ( Int, Int )
coordsToPinPos ( x, y ) =
    ( x + 1, y + 61 )


pinPosToCoords : ( Int, Int ) -> ( Int, Int )
pinPosToCoords ( into, out ) =
    ( into - 1, out - 61 )


audioPanel : SS.SynthiSchema -> List P.Pin -> PinModel -> Html.Html PinMsg
audioPanel ss pins model =
    let
        table =
            generate 60
                60
                (\x y ->
                    let
                        pin =
                            pins
                                |> find
                                    (\p ->
                                        let
                                            ( into, out ) =
                                                coordsToPinPos ( x, y )
                                        in
                                        p.into == into && p.out == out
                                    )

                        pinColor =
                            case pin of
                                Just p ->
                                    p.color

                                Nothing ->
                                    getPinColor ( x, y ) model
                    in
                    pinSvg ( x, y ) pinColor model
                )
    in
    svg [ width "480", height "495" ]
        (table
            |> Matrix.toArray
            |> Array.toList
        )


pinTable : PinModel -> Html.Html PinMsg
pinTable model =
    let
        table =
            generate 60
                60
                (\x y ->
                    pinSvg ( x, y ) "" model
                )
    in
    svg [ width "480", height "495" ]
        (table
            |> Matrix.toArray
            |> Array.toList
        )


getPinColor : ( Int, Int ) -> PinModel -> String
getPinColor ( x, y ) model =
    if sameCoordinate model.activePin ( x, y ) then
        color.active

    else if sameCoordinate model.hoverPin ( x, y ) then
        color.hover

    else if shouldDarkenPin x y then
        color.offD

    else
        color.offL


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


sameCoordinate : ( Int, Int ) -> ( Int, Int ) -> Bool
sameCoordinate ( x, y ) ( a, b ) =
    x == a || y == b
