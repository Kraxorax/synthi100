port module AudioPlayer exposing (AudioFile, Model, Msg(..), UserConfig, controlButton, fileLoaded, formatTime, init, main, onEnded, onInputRange, onLoadedMetadata, onPause, onPlaying, onTimeUpdate, padTimeString, pause, pauseIcon, pauseIt, play, playIcon, playIt, positionPercentage, setCurrentTime, setPlaybackRate, subscriptions, targetCurrentTime, targetDuration, targetRangeValue, update, updateAudioFile, validatePlaybackRate, view, viewAudioFile, viewClock, viewImg, viewPlayButton, viewSpan, viewSpeedControls, viewTimeline, viewTitle)

import Browser
import Html exposing (Attribute, Html)
import Html.Attributes
import Html.Events
import Html.Lazy
import Json.Decode as Json exposing (Decoder)
import String
import Svg exposing (Svg)
import Svg.Attributes



-- MODEL


type alias AudioFile =
    { mediaUrl : Maybe String
    , thumbnail : Maybe String
    , title : Maybe String
    , artist : Maybe String
    }


type alias UserConfig =
    { logo : Maybe String
    , speedControl : Bool
    , volumeControl : Bool
    }


type alias Model =
    { audioFile : AudioFile
    , currentTime : Float
    , duration : Float
    , playing : Bool
    , playbackRate : Float
    , playbackStep : Float
    , userConfig : UserConfig
    }



-- INIT


init : UserConfig -> ( Model, Cmd Msg )
init flags =
    ( { audioFile =
            { mediaUrl = Nothing
            , thumbnail = Nothing
            , title = Nothing
            , artist = Nothing
            }
      , currentTime = 0.0
      , duration = 0.0
      , playing = False
      , playbackRate = 1.0
      , playbackStep = 0.25
      , userConfig = flags
      }
    , Cmd.none
    )



-- MSG


type Msg
    = FileUpdate AudioFile
    | TimeUpdate Float
    | SetDuration Float
    | Playing
    | Paused
    | Play
    | Pause
    | SetTime Float
    | Slower
    | Faster
    | ResetPlayback


msgToString : Msg -> String
msgToString msg =
    case msg of
        FileUpdate af ->
            "FileUpdate"

        TimeUpdate f ->
            "TimeUpdate"

        SetDuration f ->
            "SetDuration"

        Playing ->
            "Playing"

        Paused ->
            "Paused"

        Play ->
            "Play"

        Pause ->
            "Pause"

        SetTime f ->
            "SetTime"

        Slower ->
            "Slower"

        Faster ->
            "Faster"

        ResetPlayback ->
            "ResetPlayback"



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FileUpdate file ->
            ( { model
                | audioFile = file
                , playing = True
              }
            , playIt
            )

        TimeUpdate time ->
            ( { model | currentTime = time }, Cmd.none )

        SetDuration duration ->
            ( { model | duration = duration }, Cmd.none )

        Playing ->
            ( { model | playing = True }, Cmd.none )

        Paused ->
            ( { model | playing = False }, Cmd.none )

        Play ->
            ( model, playIt )

        Pause ->
            ( model, pauseIt )

        SetTime time ->
            ( model, setCurrentTime (Debug.log "setting time: " time) )

        Slower ->
            let
                newPlaybackRate =
                    model.playbackRate - model.playbackStep

                validatedRate =
                    validatePlaybackRate model.playbackRate
                        newPlaybackRate
            in
            ( { model | playbackRate = validatedRate }
            , setPlaybackRate validatedRate
            )

        Faster ->
            let
                newPlaybackRate =
                    model.playbackRate + model.playbackStep

                validatedRate =
                    validatePlaybackRate model.playbackRate
                        newPlaybackRate
            in
            ( { model | playbackRate = validatedRate }
            , setPlaybackRate validatedRate
            )

        ResetPlayback ->
            ( { model | playbackRate = 1 }, setPlaybackRate 1 )


validatePlaybackRate : Float -> Float -> Float
validatePlaybackRate currentRate newRate =
    if newRate > 0.0 && newRate < 3.0 then
        newRate

    else
        currentRate



-- PORTS


port setCurrentTime : Float -> Cmd msg


port setPlaybackRate : Float -> Cmd msg


port play : () -> Cmd msg


port pause : () -> Cmd msg


port updateAudioFile : (AudioFile -> msg) -> Sub msg


playIt : Cmd msg
playIt =
    play ()


pauseIt : Cmd msg
pauseIt =
    pause ()



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    updateAudioFile FileUpdate



-- JSON Decoders


onEnded : msg -> Attribute msg
onEnded msg =
    Html.Events.on "ended" (Json.succeed msg)


onInputRange : (Float -> msg) -> Attribute msg
onInputRange msg =
    Html.Events.on "input" (Json.map msg targetRangeValue)


onLoadedMetadata : (Float -> msg) -> Attribute msg
onLoadedMetadata msg =
    Html.Events.on "loadedmetadata" (Json.map msg targetDuration)


onPause : msg -> Attribute msg
onPause msg =
    Html.Events.on (Debug.log "pause" "pause") (Json.succeed msg)


onPlaying : msg -> Attribute msg
onPlaying msg =
    Html.Events.on (Debug.log "play" "play") (Json.succeed msg)


onTimeUpdate : (Float -> msg) -> Attribute msg
onTimeUpdate msg =
    Html.Events.on "timeupdate" (Json.map msg targetCurrentTime)


targetCurrentTime : Decoder Float
targetCurrentTime =
    Json.at [ "target", "currentTime" ] Json.float


targetDuration : Decoder Float
targetDuration =
    Json.at [ "target", "duration" ] Json.float


targetRangeValue : Decoder Float
targetRangeValue =
    Json.at [ "target", "valueAsNumber" ] Json.float



-- VIEW


view : Model -> Html Msg
view model =
    Html.div
        [ Html.Attributes.classList
            [ ( "wrapper", True )
            , ( "open", fileLoaded model.audioFile.mediaUrl )
            ]
        ]
        [ Html.Lazy.lazy viewAudioFile model.audioFile
        , Html.div [ Html.Attributes.class "player" ]
            [ Html.Lazy.lazy2 viewImg model.audioFile.thumbnail "thumbnail"
            , Html.Lazy.lazy viewPlayButton model.playing
            , Html.Lazy.lazy2 viewSpeedControls
                model.userConfig.speedControl
                model.playbackRate
            , Html.div [ Html.Attributes.class "info" ]
                [ Html.Lazy.lazy2 viewTimeline model.currentTime model.duration
                , Html.Lazy.lazy viewTitle model.audioFile
                , Html.Lazy.lazy2 viewClock model.currentTime model.duration
                ]
            , Html.Lazy.lazy2 viewImg model.userConfig.logo "logo"
            ]
        ]


viewAudioFile : AudioFile -> Html Msg
viewAudioFile file =
    case file.mediaUrl of
        Just url ->
            Html.audio
                [ Html.Attributes.id "elm-audio-file"
                , Html.Attributes.src url
                , onLoadedMetadata SetDuration
                , onTimeUpdate TimeUpdate
                , onPause (Debug.log "Paused" Paused)
                , onPlaying (Debug.log "Playing" Playing)
                , onEnded Paused
                ]
                []

        Nothing ->
            Html.audio [ Html.Attributes.id "elm-audio-file" ] []


viewImg : Maybe String -> String -> Html Msg
viewImg src class =
    case src of
        Just s ->
            Html.img [ Html.Attributes.src s, Html.Attributes.class class ] []

        Nothing ->
            Html.text ""


viewPlayButton : Bool -> Html Msg
viewPlayButton playing =
    if playing then
        Html.button
            [ Html.Attributes.class "pause"
            , Html.Attributes.name "pause"
            , Html.Events.onClick Pause
            ]
            [ pauseIcon ]

    else
        Html.button
            [ Html.Attributes.class "play"
            , Html.Attributes.name "play"
            , Html.Events.onClick Play
            ]
            [ playIcon ]


viewSpeedControls : Bool -> Float -> Html Msg
viewSpeedControls display playbackRate =
    if display then
        Html.div [ Html.Attributes.class "playback" ]
            [ controlButton display Slower "-"
            , controlButton display
                ResetPlayback
                (String.fromFloat playbackRate ++ "x")
            , controlButton display Faster "+"
            ]

    else
        Html.text ""


controlButton : Bool -> Msg -> String -> Html Msg
controlButton display msg label =
    if display then
        let
            name =
                msg
                    |> msgToString
                    |> String.toLower
        in
        Html.button
            [ Html.Attributes.class name
            , Html.Attributes.name name
            , Html.Events.onClick msg
            ]
            [ Html.text label ]

    else
        Html.text ""


viewTimeline : Float -> Float -> Html Msg
viewTimeline position duration =
    Html.input
        [ Html.Attributes.class "timeline"
        , Html.Attributes.max (String.fromFloat duration)
        , Html.Attributes.name "timeline"
        , Html.Attributes.step "0.01"
        , Html.Attributes.style
            "background-size"
            ((String.fromFloat <| positionPercentage position duration)
                ++ "% 100%"
            )
        , Html.Attributes.type_ "range"
        , Html.Attributes.value (String.fromFloat position)
        , onInputRange SetTime
        ]
        []


viewTitle : AudioFile -> Html Msg
viewTitle file =
    Html.div []
        [ viewSpan file.artist "artist"
        , viewSpan file.title "title"
        ]


viewSpan : Maybe String -> String -> Html Msg
viewSpan text class =
    case text of
        Just t ->
            if t /= "" then
                Html.span [ Html.Attributes.class class ] [ Html.text t ]

            else
                Html.text ""

        Nothing ->
            Html.text ""


viewClock : Float -> Float -> Html Msg
viewClock currentTime duration =
    Html.div [ Html.Attributes.class "time" ]
        [ Html.text
            ((currentTime
                |> round
                |> formatTime
             )
                ++ " / "
                ++ (duration
                        |> round
                        |> formatTime
                   )
            )
        ]



-- SVG ICONS


pauseIcon : Svg Msg
pauseIcon =
    Svg.svg [ Svg.Attributes.viewBox "0 0 100 100" ]
        [ Svg.path [ Svg.Attributes.d "M40.53 19.96c-.096 0-.19.015-.282.028v-.03H29.912c-1.195 0-2.164.97-2.164 2.164V77.88c0 1.193.968 2.16 2.162 2.16h10.62c1.194 0 2.162-.967 2.162-2.16V22.126v-.005c0-1.195-.968-2.163-2.16-2.163zM72.25 77.87V22.127l.002-.005c0-1.195-.97-2.163-2.162-2.163-.097 0-.19.015-.283.028v-.03H59.47c-1.194 0-2.163.97-2.163 2.164V77.88c0 1.193.968 2.16 2.162 2.16h10.617c1.194 0 2.162-.967 2.162-2.16v-.01z" ] []
        ]


playIcon : Svg Msg
playIcon =
    Svg.svg [ Svg.Attributes.viewBox "0 0 100 100" ]
        [ Svg.path [ Svg.Attributes.d "M76.982 50c0-.847-.474-1.575-1.167-1.957L26.54 19.595c-.362-.253-.802-.404-1.278-.404-1.24 0-2.244 1.005-2.244 2.244 0 .087.016.17.026.253h-.026v57.13h.026c.127 1.12 1.066 1.99 2.218 1.99.41 0 .787-.116 1.117-.307l.02.035L75.874 51.97l-.02-.035c.67-.388 1.127-1.105 1.127-1.935z" ] []
        ]



-- UTILITY FUNCTIONS


fileLoaded : Maybe String -> Bool
fileLoaded url =
    case url of
        Just u ->
            True

        Nothing ->
            False


formatTime : Int -> String
formatTime time =
    let
        hours =
            time // 3600 |> padTimeString

        -- remainderBy may suck
        minutes =
            remainderBy time 3600 // 60 |> padTimeString

        -- remainderBy may suck
        seconds =
            -- remainderBy time (3600 // 60) |> padTimeString
            "66"

        timeList =
            if hours == "00" then
                [ minutes, seconds ]

            else
                [ hours, minutes, seconds ]
    in
    String.join ":" timeList


padTimeString : Int -> String
padTimeString timeUnit =
    String.padLeft 2 '0' (String.fromInt timeUnit)


positionPercentage : Float -> Float -> Float
positionPercentage position duration =
    (position / duration) * 100.0



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
