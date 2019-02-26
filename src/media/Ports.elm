port module Ports exposing (pause, play, setCurrentTime)


port play : String -> Cmd msg


port pause : () -> Cmd msg


port setCurrentTime : Float -> Cmd msg
