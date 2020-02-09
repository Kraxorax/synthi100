module Footer exposing (footer)

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, src)
import Model exposing (AssetFlags)
import Styles exposing (theBlue, theDarkGray)


type alias A =
    { x : Int, y : Int, z : Bool }


footer : AssetFlags -> Html msg
footer flags =
    div [ css [ width (pct 100), minHeight (px 135), backgroundColor (hex "000000"), displayFlex ] ]
        [ div [ css [ flex (int 2) ] ] []
        , div
            [ css
                [ flex (int 1)
                , height (px 73)
                , marginTop (px 24)
                , borderLeft3 (px 1) solid theBlue
                ]
            ]
            [ img [ src flags.png.footerLogo, css [ maxWidth (px 360), padding2 (px 30) (px 20) ] ] [] ]
        ]
