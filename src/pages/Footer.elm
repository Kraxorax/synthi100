module Footer exposing (footer)

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Styles exposing (theBlue, theDarkGray)


footer : Html msg
footer =
    div [ css [ width (pct 100), minHeight (px 135), backgroundColor theDarkGray, displayFlex ] ]
        [ div [ css [ flex (int 2) ] ] []
        , div
            [ css
                [ flex (int 1)
                , height (px 73)
                , marginTop (px 24)
                , borderLeft3 (px 1) solid theBlue
                ]
            ]
            []
        ]
