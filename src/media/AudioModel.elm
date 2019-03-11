module AudioModel exposing (AudioModel, emptyAudioModel, noPlayingAudioModel)


type alias AudioModel =
    { playing : Bool
    , seekerPosition : Float
    , volume : Float
    }


emptyAudioModel : AudioModel
emptyAudioModel =
    { playing = True
    , seekerPosition = 0.0
    , volume = 0.5
    }


noPlayingAudioModel : AudioModel
noPlayingAudioModel =
    { playing = False
    , seekerPosition = 0.0
    , volume = 0.0
    }
