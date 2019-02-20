module AboutPage exposing (page)

import Css exposing (..)
import Css.Global exposing (body, global)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Model exposing (Model)
import Msg exposing (Msg)


page : Model -> Html Msg
page model =
    div []
        [ h1 []
            [ text "About page"
            ]
        ]
