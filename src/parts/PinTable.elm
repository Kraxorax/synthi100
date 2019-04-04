module PinTable exposing (Panel(..), PinModel, PinMsg(..), audioPanel, coordsToPinPos, initModel, pinSvg, pinTable, setActivePin, setHoverPin, update)

import Array
import Html.Attributes exposing (style)
import Html.Styled exposing (..)
import List.Extra exposing (find)
import Matrix exposing (Matrix, generate)
import Patch as P
import Svg.Styled as Svg exposing (..)
import Svg.Styled.Attributes as Svg exposing (..)
import Svg.Styled.Events as Svg exposing (..)
import SynthiSchema as SS
import Tuple exposing (first, second)


color =
    { offL =
        "#c8c8c8"
    , offD =
        "#000000"
    , hover =
        "#dfdfdf"
    , active =
        "#fff"
    }


pinDistanceX =
    12


pinDistanceY =
    12


gap =
    15


pinRadius =
    3


margin =
    30


type Panel
    = Audio
    | Control


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


pinSvg : ( Int, Int ) -> String -> Maybe String -> PinModel -> Svg PinMsg
pinSvg ( xp, yp ) fillColor strokeColor model =
    let
        yPos =
            (if yp >= 30 then
                yp * pinDistanceY + gap

             else
                yp * pinDistanceY
            )
                + pinRadius
                + margin
                |> String.fromInt

        xPos =
            (xp * pinDistanceX) + pinRadius + margin |> String.fromInt

        ( strokeCol, strokeWdth, rad ) =
            strokeColor
                |> Maybe.map
                    (\sc -> ( stroke sc, strokeWidth "3", pinRadius + 1 ))
                |> Maybe.withDefault ( stroke "", strokeWidth "0", pinRadius )
    in
    circle
        [ r (rad |> String.fromInt)
        , cx xPos
        , cy yPos
        , fill fillColor
        , strokeCol
        , strokeWdth
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


audioPanel : List P.Pin -> PinModel -> Html PinMsg
audioPanel pins model =
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

                        pinStrokeColor =
                            pin |> Maybe.map (\p -> pinColorToHex p.color)

                        pinFillColor =
                            getPinColor ( x, y ) model
                    in
                    pinSvg ( x, y ) pinFillColor pinStrokeColor model
                )

        w =
            60 * pinDistanceX + 40 |> String.fromFloat

        h =
            60 * pinDistanceY + 40 |> String.fromFloat

        pinNumbers =
            if first model.hoverPin < 0 || (second model.hoverPin < 0) then
                []

            else
                [ pinNumberY model.hoverPin
                , pinNumberX model.hoverPin
                ]
    in
    svg [ width w, height h ]
        (pinNumbers
            |> List.append
                (table
                    |> Matrix.toArray
                    |> Array.toList
                )
        )


pinNumberY : ( Int, Int ) -> Html PinMsg
pinNumberY ( i, o ) =
    let
        y =
            (if o < 30 then
                o * pinDistanceX + margin

             else
                o * pinDistanceX + margin + gap
            )
                + 6
                |> String.fromInt

        ( ri, ro ) =
            coordsToPinPos ( i, o )
    in
    Svg.text_
        [ dx "15"
        , dy y
        , textAnchor "middle"
        , fontFamily "Metropolis"
        , fontWeight "bold"
        , fontSize "14px"
        , letterSpacing "1.4px"
        , fill "#FFFFFF"
        ]
        [ Svg.text (ro |> String.fromInt) ]


pinNumberX : ( Int, Int ) -> Html PinMsg
pinNumberX ( i, o ) =
    let
        x =
            (i * pinDistanceX + margin)
                |> String.fromInt

        ( ri, ro ) =
            coordsToPinPos ( i, o )
    in
    Svg.text_
        [ dx x
        , dy "14"
        , textAnchor "middle"
        , fontFamily "Metropolis"
        , fontWeight "bold"
        , fontSize "14px"
        , letterSpacing "1.4px"
        , fill "#FFFFFF"
        ]
        [ Svg.text (ri |> String.fromInt) ]


pinTable : PinModel -> Html PinMsg
pinTable model =
    let
        table =
            generate 60
                60
                (\x y ->
                    pinSvg ( x, y ) "" Nothing model
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


pinColorToHex : String -> String
pinColorToHex col =
    if col == "white" then
        "#fff"

    else if col == "blue" then
        "#4a90e2"

    else if col == "orange" then
        "#f5a623"

    else
        col


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
