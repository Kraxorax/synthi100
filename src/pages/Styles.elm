module Styles exposing (next, prev, theBlue, vi, xBox)

-- import Svg exposing (..)

import Css exposing (..)
import Svg.Styled as Svg exposing (..)
import Svg.Styled.Attributes as Svg exposing (..)
import Svg.Styled.Events as Svg exposing (..)


theBlue =
    hex "4a90e2"


theBlueString =
    "#4a90e2"


cross =
    [ line [ Svg.x1 "4px", Svg.y1 "4px", Svg.x2 "16px", Svg.y2 "16px", Svg.stroke "white" ] []
    , line [ Svg.x1 "16px", Svg.y1 "4px", Svg.x2 "4px", Svg.y2 "16px", Svg.stroke "white" ] []
    ]


xBox isCheck =
    let
        c =
            if isCheck then
                cross

            else
                []
    in
    svg [ Svg.width "20px", Svg.height "20px", css [ Css.verticalAlign middle ] ]
        (rect
            [ Svg.x "1px"
            , Svg.y "1px"
            , Svg.width "18px"
            , Svg.height "18px"
            , Svg.stroke "white"
            ]
            []
            :: c
        )


pnBttnImgCss : Style
pnBttnImgCss =
    batch
        [ Css.display block
        , margin auto
        ]


prev =
    svg [ Svg.height "20px", Svg.width "20px", css [ pnBttnImgCss ] ]
        [ polygon [ points "20,0 20,20 0,10", Svg.fill theBlueString ] []
        ]


next =
    svg [ Svg.height "20px", Svg.width "20px", css [ pnBttnImgCss ] ]
        [ polygon [ points "0,0 20,10 0,20", Svg.fill theBlueString ] []
        ]




vi pos =
    let
        x =
            (264 / (100 / pos) |> String.fromFloat) ++ "px"
    in
    svg [ Svg.width "264px", Svg.height "40px" ]
        [ rect [ Svg.x "0px", Svg.y "18px", Svg.width "264px", Svg.height "4px", Svg.fill "white" ] []
        , rect [ Svg.x x, Svg.y "0px", Svg.width "10px", Svg.height "40px", Svg.fill "#4a90e2", Svg.stroke "black" ] []
        ]
