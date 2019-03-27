module Header exposing (header)

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href, src)
import Msg exposing (Msg)


headerTitleCss : Style
headerTitleCss =
    batch
        [ letterSpacing (px 0.5)
        , lineHeight (num 1.2)
        , fontSize (px 24)
        , fontWeight bold
        ]


headerFlexCss : Style
headerFlexCss =
    batch
        [ height (px 111)
        , maxWidth (px 1440)
        , displayFlex
        , alignItems flexEnd
        , boxSizing borderBox
        , justifyContent spaceBetween
        , marginBottom (px 40)
        ]


navigationLinkCss : Style
navigationLinkCss =
    batch
        [ flex (num 1)
        , textDecoration none
        , color (hex "FFFFFF")
        , visited [ color (hex "FFFFFF") ]
        ]


headerNavigationCss : Style
headerNavigationCss =
    batch
        [ justifyContent spaceAround
        , borderLeftStyle solid
        , borderLeftWidth (px 2)
        , height (px 73)
        , color (hex "4a90e2")
        , displayFlex
        , flex (num 1)
        , flexDirection row
        , paddingBottom (px 2)
        , alignItems flexEnd
        , fontSize (px 14)
        , fontWeight bold
        , letterSpacing (px 1.4)
        ]


synthiTitleFlexCss : Style
synthiTitleFlexCss =
    batch
        [ displayFlex
        , flexDirection column
        , paddingLeft (px 31)
        , minHeight (px 78)
        , headerTitleCss
        , flex (num 2)
        , justifyContent spaceBetween
        ]


headerLineCss : Style
headerLineCss =
    batch
        [ width (px 100)
        , height (px 3)
        , backgroundColor (hex "4a90e2")
        ]


header : Html Msg
header =
    div
        [ css [ headerFlexCss ] ]
        [ div
            [ css
                [ flex (num 1)
                , displayFlex
                , alignItems flexEnd
                ]
            ]
            [ div
                [ css [ flex (num 1), paddingLeft (px 31), marginBottom (px 1) ] ]
                [ img [ src "synthi-logo.svg" ] [] ]
            , div
                [ css [ synthiTitleFlexCss ] ]
                [ div [ css [ headerLineCss ] ] []
                , div
                    [ css [ maxWidth (px 230) ] ]
                    [ text "EMS SYNTHI 100 on the Web" ]
                ]
            ]
        , div
            [ css [ headerTitleCss, flex (num 1), marginLeft (px -1) ] ]
            [ text "Elektronski studio Radio Beograda"
            , br [] []
            , text "Radio Belgrade — Electronic Studio"
            ]
        , div
            [ css [ headerNavigationCss ] ]
            navigation
        ]


navLink : List Style -> String -> String -> Html Msg
navLink css_ href_ text_ =
    a
        [ css ([ navigationLinkCss ] ++ css_)
        , href href_
        ]
        [ text text_ ]


navigation : List (Html Msg)
navigation =
    [ navLink [ textAlign center ] "/database" "database"
    , navLink [ textAlign left ] "/about" "about / credits"
    ]
