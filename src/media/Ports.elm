port module Ports exposing (loop, pause, play, setCurrentTime, toTop)


port play : String -> Cmd msg


port pause : String -> Cmd msg


port loop : Bool -> Cmd msg


port toTop : () -> Cmd msg


port setCurrentTime : ( String, Float ) -> Cmd msg
