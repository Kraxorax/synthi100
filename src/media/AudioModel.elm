module AudioModel exposing (AudioModel, emptyAudioModel, noPlayingAudioModel)


type alias AudioModel =
    { playing : Bool
    , seekerPosition : Float
    }


emptyAudioModel : AudioModel
emptyAudioModel =
    { playing = True
    , seekerPosition = 0.0
    }


noPlayingAudioModel : AudioModel
noPlayingAudioModel =
    { playing = False
    , seekerPosition = 0.0
    }
