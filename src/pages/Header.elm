module Header exposing (header)

import Css exposing (..)
import Model exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href, src)
import Msg exposing (Msg)
import Styles exposing (theBlue)
import Html.Styled.Events exposing (onClick)


headerTitleCss : Style
headerTitleCss =
    batch
        [ letterSpacing (px 0.5)
        , fontSize (px 24)
        , fontWeight bold
        ]

headerColumnCss : Style
headerColumnCss =
    batch
        [ flex (pct (100/3.0))
        , borderWidth (px 10)
        , borderStyle solid
        , borderColor (hex "000000")
        , boxSizing borderBox
        ]

headerFlexCss : Style
headerFlexCss =
    batch
        [ maxWidth (px 1280)
        , height (px 90)
        , displayFlex
        , lineHeight (px 27)
        , alignItems flexEnd
        , padding2 (px 0) (px 10)
        , marginBottom (px 30)
        ]


navigationLinkCss : Style
navigationLinkCss =
    batch
        [ flex (num 1)
        , textDecoration none
        , color (hex "ffffff")
        , visited [ color (hex "ffffff") ]
        ]


headerNavigationCss : Style
headerNavigationCss =
    batch
        [ justifyContent spaceAround
        , displayFlex
        , flexDirection row
        , alignItems flexEnd
        , fontSize (px 14)
        , fontWeight bold
        , letterSpacing (px 1.4)
        ]


header : Model -> Html Msg
header model =
    div
        [ css [ headerFlexCss ] ]
        -- title column
        [ div
            [ css [ headerColumnCss
                  , displayFlex
                  , alignItems flexEnd
                  ]
            ]
            -- synthi logo
            [ div
                [ css [ marginLeft (px 17)
                      , boxSizing borderBox
                      , marginRight (px 30)
                      , height (px 73)
                      , width (px 119)
                      , marginBottom (px 1)
                      ]
                ]
                [ a
                    [ href "/database" ]
                    [ img [ src model.assets.png.synthiLogo ] [] ]
                ]
            -- title
            , div
                [ css [ headerTitleCss ] ]
                [ text "EMS SYNTHI 100 on the Web" ]
            ]
        -- radio belgrade column
        , div
            [ css [ headerColumnCss
                  , headerTitleCss
                  ,  fontSize (px 20) ]
                  ]
            [ text "Elektronski studio Radio Beograda"
            , br [] []
            , text "Radio Belgrade — Electronic Studio"
            ]
        -- vertical blue line
        , div
            [ css [ width (px 1)
            , height (px 73)
            , backgroundColor theBlue ] ]
            []
        -- navigation column
        , div
            [ css [ headerColumnCss, headerNavigationCss ] ]
            (navigation model)
        ]


navLink : List Style -> String -> String -> Html Msg
navLink css_ href_ text_ =
    a
        [ css ([ navigationLinkCss ] ++ css_)
        , href href_
        ]
        [ text text_ ]

logOutLink : Html Msg
logOutLink =
    a
        [ css [
                navigationLinkCss
              , textAlign right
              , display Css.block
              , marginBottom (px 20)
              , fontWeight normal
              ]
        , href "/saml/logout"
        , onClick Msg.Logout
        ]
        [ text "[logout]" ]


navigation : Model -> List (Html Msg)
navigation model =
    [ navLink [ textAlign center, flex (num 2) ] "/database" "database"
    , div [css [ flex (num 1) ] ] [
            if model.userInfo == Nothing then text "" else logOutLink
          , navLink [ whiteSpace noWrap ] "/about" "about / credits"
          ]
    , div [css [ flex (num 1) ] ] [] -- filler
    ]
