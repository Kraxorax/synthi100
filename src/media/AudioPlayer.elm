port module AudioPlayer exposing (AudioModel, AudioMsg(..), audioUpdate, emptyAudioModel, noPlayingAudioModel)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Html
import Html.Attributes as HA
import Html.Attributes.Extra as HAE
import Html.Events as HE
import Html.Events.Extra as HEE
import Html.Styled as HS exposing (..)
import Html.Styled.Attributes as HSA exposing (..)
import Json.Decode as JD
import Json.Encode as JE
import Patch as P
import Ports exposing (..)
import Time exposing (Posix)


type alias AudioModel =
    { playing : Bool
    , seekerPosition : Float
    , volume : Float
    , position : ( Float, Float )
    }


type AudioMsg
    = NoOp
    | Tick Float
    | Play P.Patch
    | Pause
    | Ended
    | ChangeTrack
    | VolumeDrag Float


emptyAudioModel : AudioModel
emptyAudioModel =
    { playing = True
    , seekerPosition = 0.0
    , volume = 0.5
    , position = ( 0, 0 )
    }


noPlayingAudioModel : AudioModel
noPlayingAudioModel =
    { playing = False
    , seekerPosition = 0.0
    , volume = 0.0
    , position = ( 0, 0 )
    }


audioUpdate : AudioMsg -> AudioModel -> ( AudioModel, Cmd AudioMsg )
audioUpdate msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Play patch ->
            ( { model | playing = True }, play patch.title )

        Pause ->
            ( { model | playing = False }, pause "" )

        -- TimeUpdate time ->
        --     ( { model | seekerPosition = 100 / (model.duration / time) }, Cmd.none )
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
                | playing = False
                , seekerPosition = 0.0
              }
            , Cmd.none
            )

        -- DurationChanged duration ->
        --     ( { model | duration = duration }, Cmd.none )
        Ended ->
            ( { model | playing = False, seekerPosition = 0 }, Cmd.none )

        -- SeekerDrag position ->
        --     let
        --         pos =
        --             position
        --         currentTime =
        --             model.duration / (100 / pos)
        --     in
        --     ( { model | seekerPosition = pos }
        --     , setCurrentTime currentTime
        --     )
        VolumeDrag volume ->
            ( { model | volume = volume / 100 }, Cmd.none )



-- JSON Encoders/Decoders
-- onEnded : msg -> Html.Attribute msg
-- onEnded msg =
--     HE.on "ended" (JD.succeed msg)
-- onDurationChange : (Float -> msg) -> Html.Attribute msg
-- onDurationChange msg =
--     HE.on "durationchange" (JD.map msg targetDuration)
-- targetDuration : JD.Decoder Float
-- targetDuration =
--     JD.at [ "target", "duration" ] JD.float
-- onTimeUpdate : (Float -> msg) -> Html.Attribute msg
-- onTimeUpdate msg =
--     HE.on "timeupdate" (JD.map msg targetCurrentTime)
-- targetCurrentTime : JD.Decoder Float
-- targetCurrentTime =
--     JD.at [ "target", "currentTime" ] JD.float
-- onInputFloat : (Float -> msg) -> Html.Attribute msg
-- onInputFloat msg =
--     HE.on "input" (JD.map msg HEE.targetValueFloat)
-- View
-- viewPlayPause : AudioModel -> Html.Html AudioMsg
-- viewPlayPause model =
--     let
--         plB =
--             if model.playing then
--                 Html.button [ HE.onClick Pause ] [ Html.text "Pause" ]
--             else
--                 Html.button [ HE.onClick (Play "") ] [ Html.text "Play" ]
--     in
--     plB
-- viewSeeker : AudioModel -> Html.Html AudioMsg
-- viewSeeker model =
--     let
--         seekerPos =
--             String.fromFloat model.seekerPosition
--     in
--     Html.input
--         [ HA.type_ "range"
--         , HA.value seekerPos
--         , onInputFloat SeekerDrag
--         ]
--         []
-- viewVolume : AudioModel -> Html.Html AudioMsg
-- viewVolume model =
--     let
--         volPos =
--             model.volume * 100 |> String.fromFloat
--     in
--     Html.input
--         [ HA.type_ "range"
--         , HA.value volPos
--         , onInputFloat VolumeDrag
--         ]
--         []
-- viewAudio : AudioModel -> P.Patch -> Html AudioMsg
-- viewAudio model patch =
--     audio
--         [ id patch.title
--         , src patch.soundUrl
--         , HAE.volume model.volume |> HSA.fromUnstyled
--         , onEnded Ended |> HSA.fromUnstyled
--         , onDurationChange DurationChanged |> HSA.fromUnstyled
--         , onTimeUpdate TimeUpdate |> HSA.fromUnstyled
--         ]
--         []
-- viewDuration : AudioModel -> Html.Html AudioMsg
-- viewDuration model =
--     let
--         duration =
--             String.fromFloat model.duration
--     in
--     Html.span [] [ Html.text duration ]


viewChangeSource : AudioModel -> Html.Html AudioMsg
viewChangeSource model =
    Html.button [ HE.onClick ChangeTrack ] [ Html.text "other" ]



-- view : AudioModel -> Html.Html AudioMsg
-- view model =
--     Html.div []
--         [ viewPlayPause model
--         , viewSeeker model
--         , viewAudio model
--         , viewDuration model
--         , viewChangeSource model
--         , viewVolume model
--         ]
