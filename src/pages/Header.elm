module Header exposing (header)

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href, src)
import Msg exposing (Msg)

headerTitleStyle : Style
headerTitleStyle =
    batch
      [ marginLeft (px 10)
      , letterSpacing (px 0.5)
      , lineHeight (num 1.15)
      , fontSize (px 24)
      , fontWeight bold
     ]

headerNavigationStyle : Style
headerNavigationStyle =
    batch
        [ justifyContent spaceAround
        , borderLeftStyle solid
        , height (px 73)
        , flex (num 8)
        , color (hex "4a90e2")
        , displayFlex
        , flexDirection row
        , paddingBottom (px 2)
        , alignItems flexEnd
        , fontSize (px 14)
        , fontWeight bold
        , maxWidth (px 449)
        , letterSpacing (px 1.4)
        ]

headerFlexStyle : Style
headerFlexStyle =
    batch
        [ height (px 150)
        , paddingLeft (px 31)
        , paddingRight (px 31)
        , paddingBottom (px 40)
        , maxWidth (px 1440)
        , displayFlex
        , alignItems flexEnd
        , boxSizing borderBox
        , justifyContent spaceBetween
        ]

navigationLinkStyle : Style
navigationLinkStyle =
    batch
        [ textAlign center
        , textDecoration none
        , color (hex "FFFFFF")
        , visited [(color (hex "FFFFFF"))]
        ]


header : Html Msg
header =
    div
        [ css [ headerFlexStyle ] ]
        [ div
            [ css [flex (num 0)]
            ]
            [ img [src "synthi-logo.svg"] []
            ]
        , div
            [ css [maxWidth (px 225), flex (num 4), headerTitleStyle]
            ]
            [ text "EMS SYNTHI 100 on the Web"
            ]
        , div
            [ css [maxWidth (px 439), flex (num 8), headerTitleStyle]
            ]
            [ text "Elektronski studio Radio Beograda"
            , br [] []
            , text "Radio Belgrade — Electronic Studio"
            ]
        , div
            [ css [ headerNavigationStyle ]
            ]
            navigation
        ]

navLink : String -> String -> Html Msg
navLink href_ text_ =
    a
        [ css [ flex (num 1), navigationLinkStyle ]
        , href href_
        ]
        [ text text_ ]

navigation : List (Html Msg)
navigation =
    [ navLink "/database" "database"
    , navLink "/about" "about / credits"
    ]
