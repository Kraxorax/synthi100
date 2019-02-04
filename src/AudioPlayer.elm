port module AudioPlayer exposing (play)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Html
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as JD
import Json.Encode as JE
import Time exposing (Posix)


audios =
    { ff = "https://mdn.mozillademos.org/files/2587/AudioTest (1).ogg"
    , more = "http://freesound.org/data/previews/458/458248_9581521-lq.mp3"
    }


type alias Model =
    { playing : Bool
    , audioSrc : String
    , duration : Float
    , seekerPosition : Float
    }


type Msg
    = NoOp
    | Tick Float
    | Play
    | Pause
    | TimeUpdate Float
    | Ended
    | ChangeTrack
    | DurationChanged Float


init : () -> ( Model, Cmd msg )
init flags =
    ( { playing = False
      , audioSrc = audios.more
      , seekerPosition = 0.0
      , duration = 0.0
      }
    , Cmd.none
    )


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta onTick
        ]


onTick : Float -> Msg
onTick timeDelta =
    NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Play ->
            ( { model | playing = True }, play () )

        Pause ->
            ( { model | playing = False }, pause () )

        TimeUpdate time ->
            ( { model | seekerPosition = 100 / (model.duration / time) }, Cmd.none )

        Tick deltaTime ->
            if not model.playing then
                ( model, Cmd.none )

            else
                ( { model
                    | seekerPosition =
                        model.seekerPosition + deltaTime
                  }
                , Cmd.none
                )

        ChangeTrack ->
            ( { model
                | audioSrc =
                    if model.audioSrc == audios.ff then
                        audios.more

                    else
                        audios.ff
                , playing = False
                , seekerPosition = 0.0
              }
            , Cmd.none
            )

        DurationChanged duration ->
            ( { model | duration = duration }, Cmd.none )

        Ended ->
            ( { model | playing = False, seekerPosition = 0 }, Cmd.none )



-- Ports


port play : () -> Cmd msg


port pause : () -> Cmd msg



-- JSON Encoders/Decoders


onEnded : msg -> Html.Attribute msg
onEnded msg =
    HE.on "ended" (JD.succeed msg)


onDurationChange : (Float -> msg) -> Html.Attribute msg
onDurationChange msg =
    HE.on "durationchange" (JD.map msg targetDuration)


targetDuration : JD.Decoder Float
targetDuration =
    JD.at [ "target", "duration" ] JD.float


onTimeUpdate : (Float -> msg) -> Html.Attribute msg
onTimeUpdate msg =
    HE.on "timeupdate" (JD.map msg targetCurrentTime)


targetCurrentTime : JD.Decoder Float
targetCurrentTime =
    JD.at [ "target", "currentTime" ] JD.float



-- Views


viewPlayPause : Model -> Html.Html Msg
viewPlayPause model =
    let
        plB =
            if model.playing then
                Html.button [ HE.onClick Pause ] [ Html.text "Pause" ]

            else
                Html.button [ HE.onClick Play ] [ Html.text "Play" ]
    in
    plB


viewSeeker : Model -> Html.Html Msg
viewSeeker model =
    let
        seekerPos =
            String.fromFloat model.seekerPosition
    in
    Html.input [ HA.type_ "range", HA.value seekerPos ] []


viewAudio : Model -> Html.Html Msg
viewAudio model =
    Html.audio
        [ HA.id "elm-audio-file"
        , HA.src model.audioSrc
        , onEnded Ended
        , onDurationChange DurationChanged
        , onTimeUpdate TimeUpdate
        ]
        []


viewDuration : Model -> Html.Html Msg
viewDuration model =
    let
        duration =
            String.fromFloat model.duration
    in
    Html.span [] [ Html.text duration ]


viewChangeSource : Model -> Html.Html Msg
viewChangeSource model =
    Html.button [ HE.onClick ChangeTrack ] [ Html.text "other" ]


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ viewPlayPause model
        , viewSeeker model
        , viewAudio model
        , viewDuration model
        , viewChangeSource model
        ]
