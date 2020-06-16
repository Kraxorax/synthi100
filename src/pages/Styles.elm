module Styles exposing (circ, downBttn, linkUnstyle, next, prev, rightBttn, theBlue, theDarkGray, theDarkGrayString, theGray, theGrayString, theLightGray, theLightGrayString, upBttn, vi, xBox, closeCross)

import Css exposing (..)
import Svg.Styled as Svg exposing (..)
import Svg.Styled.Attributes as Svg exposing (..)
import Svg.Styled.Events as Svg exposing (..)


theBlue =
    hex "4a90e2"


theGray =
    hex "9b9b9b"

theDarkGray =
    hex "4a4a4a"


theLightGray =
    hex "d8d8d8\n"


theGrayString =
    "#9b9b9b"


theDarkGrayString =
    "#4a4a4a"


theBlueString =
    "#4a90e2"

theLightGrayString =
    "#d8d8d8"


cross =
    [ line [ Svg.x1 "4px", Svg.y1 "4px", Svg.x2 "16px", Svg.y2 "16px", Svg.stroke "white" ] []
    , line [ Svg.x1 "16px", Svg.y1 "4px", Svg.x2 "4px", Svg.y2 "16px", Svg.stroke "white" ] []
    ]

closeCross =
    svg [ Svg.width "20px", Svg.height "20px" ]
        [ line [ Svg.x1 "0px", Svg.y1 "0px", Svg.x2 "20px", Svg.y2 "20px", Svg.stroke "black" ] []
        , line [ Svg.x1 "20px", Svg.y1 "0px", Svg.x2 "0px", Svg.y2 "20px", Svg.stroke "black" ] []
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
            , Svg.fillOpacity "0"
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
    svg [ Svg.height "15px", Svg.width "15px", css [ pnBttnImgCss ] ]
        [ polygon [ points "15,0 15,15 0,7.5", Svg.fill theDarkGrayString ] []
        ]


next =
    svg [ Svg.height "15px", Svg.width "15px", css [ pnBttnImgCss ] ]
        [ polygon [ points "0,0 15,7.5 0,15", Svg.fill theDarkGrayString ] []
        ]


circ =
    svg [ Svg.width "25px", Svg.height "25px" ]
        [ Svg.circle [ r "12", cx "12", cy "12", Svg.fill "#d8d8d8" ] [] ]


vi pos =
    let
        x =
            (264 / (100 / pos) |> String.fromFloat) ++ "px"
    in
    svg [ Svg.width "264px", Svg.height "40px" ]
        [ rect [ Svg.x "0px", Svg.y "18px", Svg.width "264px", Svg.height "4px", Svg.fill "white" ] []
        , rect [ Svg.x x, Svg.y "0px", Svg.width "10px", Svg.height "40px", Svg.fill "#4a90e2", Svg.stroke "black" ] []
        ]


linkUnstyle : Style
linkUnstyle =
    batch
        [ Css.color inherit
        , Css.textDecoration none
        ]


downBttn : String -> Svg msg
downBttn bgColor =
    svg [ Svg.width "20", Svg.height "20" ]
        [ rect [ x "0", y "0", Svg.width "20", Svg.height "20", Svg.fill bgColor ] []
        , line [ x1 "4", y1 "6", x2 "10", y2 "13", Svg.stroke "#000", Svg.strokeWidth "2" ] []
        , line [ x1 "16", y1 "6", x2 "10", y2 "13", Svg.stroke "#000", Svg.strokeWidth "2" ] []
        ]


upBttn : String -> Svg msg
upBttn bgColor =
    svg [ Svg.width "20", Svg.height "20" ]
        [ rect [ x "0", y "0", Svg.width "20", Svg.height "20", Svg.fill bgColor ] []
        , line [ x1 "4", y1 "13", x2 "10", y2 "6", Svg.stroke "#000", Svg.strokeWidth "2" ] []
        , line [ x1 "16", y1 "13", x2 "10", y2 "6", Svg.stroke "#000", Svg.strokeWidth "2" ] []
        ]


rightBttn : String -> Svg msg
rightBttn bgColor =
    svg [ Svg.width "20", Svg.height "20" ]
        [ rect [ x "0", y "0", Svg.width "20", Svg.height "20", Svg.fill bgColor ] []
        , line [ x1 "6", y1 "4", x2 "13", y2 "10", Svg.stroke "#000", Svg.strokeWidth "2" ] []
        , line [ x1 "6", y1 "16", x2 "13", y2 "10", Svg.stroke "#000", Svg.strokeWidth "2" ] []
        ]
