module AudioMsg exposing (AudioMsg(..))

import Patch as P


type AudioMsg
    = NoOp
    | Tick Float
    | Play P.Patch
    | Pause
    | Ended
    | ChangeTrack
    | VolumeDrag Float
