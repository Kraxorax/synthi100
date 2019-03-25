module Styles exposing (theBlue, xBox)

-- import Svg exposing (..)

import Css exposing (..)
import Svg.Styled as Svg exposing (..)
import Svg.Styled.Attributes as Svg exposing (..)
import Svg.Styled.Events as Svg exposing (..)


theBlue =
    hex "4a90e2"


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
