module Header exposing (header)

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href)
import Msg exposing (Msg)


headerHeight =
    px 100


headerThirdStyle =
    css
        [ width (pct 33)
        , float left
        , height headerHeight
        , displayFlex
        , flexDirection row
        , alignItems flexEnd
        ]


header : Html Msg
header =
    div
        [ css
            [ width (pct 100)
            , height headerHeight
            ]
        ]
        [ div
            [ headerThirdStyle
            ]
            [ text "image"
            , div []
                [ text "EMS SYNTHI 100"
                , br [] []
                , text "on the Web"
                ]
            ]
        , div
            [ headerThirdStyle
            ]
            [ text "Elektronski studio RGB"
            , br [] []
            , text "RGB Electronic Studio"
            ]
        , div
            [ headerThirdStyle
            , css
                [ justifyContent spaceAround
                , borderLeftColor (hex "4A90E2")
                , borderWidth4 (px 0) (px 0) (px 0) (px 1)
                , borderStyle solid
                , color (hex "ffffff")
                , textDecoration underline
                ]
            ]
            navigation
        ]


navigation : List (Html Msg)
navigation =
    [ a
        [ css [ flex2 (num 0) (num 0) ]
        , href "database"
        ]
        [ text "database" ]
    , a
        [ css [ flex2 (num 0) (num 0) ]
        , href "credits"
        ]
        [ text "credits" ]
    , a
        [ css [ flex2 (num 0) (num 0) ]
        , href "about"
        ]
        [ text "about" ]
    ]
