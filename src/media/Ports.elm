port module Ports exposing (pause, play, setCurrentTime)


port play : String -> Cmd msg


port pause : String -> Cmd msg


port setCurrentTime : ( String, Float ) -> Cmd msg
