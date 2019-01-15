import Browser
import Html
import Svg exposing (..)
import Svg.Attributes exposing (..)


init: () -> (Model, Cmd msg)
init flags = (Model, Cmd.none)

subs: Model -> Sub msg
subs model = Sub.none

main =
  Browser.element { init = init, subscriptions = subs, update = update, view = view }

type Msg = Increment | Decrement

type Model = Model 


update: Msg -> Model -> (Model, Cmd msg)
update msg model = (model, Cmd.none)


view: Model -> Html.Html (Msg)
view model =
  Html.div [] [ text "njaaaah", svg
    [ width "120"
    , height "120"
    , viewBox "0 0 120 120"
    ]
    [ rect
        [ x "10"
        , y "10"
        , width "100"
        , height "100"
        , rx "15"
        , ry "15"
        ]
        []
    , circle
        [ cx "50"
        , cy "50"
        , r "50"
        ]
        []
    ] ]