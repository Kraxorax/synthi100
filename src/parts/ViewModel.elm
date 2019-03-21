module ViewModel exposing (Control(..), Knob, Module, Switch)


type alias Knob =
    { name : String
    , value : Maybe Float
    }


type alias Module =
    { name : String
    , controls : List Control
    }


type Control
    = KnobCtrl Knob
    | SwitchCtrl Switch


type alias Switch =
    { name : String
    , case_ : Maybe String
    }
