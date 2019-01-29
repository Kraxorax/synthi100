module PinTable exposing (PinModel, PinMsg, initModel, pinTable, update)

import Html
import Matrix exposing (Matrix, generate)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)


color =
    { offL =
        "#999999"
    , offD =
        "#333333"
    , hover =
        "#66AAFF"
    , active =
        "#FF0000"
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


pinSvg : ( Int, Int ) -> PinModel -> Svg PinMsg
pinSvg ( xp, yp ) model =
    let
        pinColor =
            getPinColor ( xp, yp ) model

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


pinTable : PinModel -> Matrix (Html.Html PinMsg)
pinTable model =
    generate 60
        60
        (\x y ->
            pinSvg ( x, y ) model
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
