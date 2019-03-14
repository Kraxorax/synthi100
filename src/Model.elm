module Model exposing (AttrFilter, Control(..), Knob, Model, Module, PinConnection, Switch)

import AudioModel exposing (AudioModel)
import Browser.Navigation exposing (Key)
import Patch as P
import PinTable
import Routing exposing (Route)
import SynthiSchema as SS


type alias Model =
    { navKey : Key
    , currentRoute : Route
    , circleFill : String
    , hoverKnob : ( Int, Int )
    , pinModel : PinTable.PinModel
    , synthiSchema : Maybe SS.SynthiSchema
    , patches : Maybe (List P.Patch)
    , error : Maybe String
    , activeAudioPin : Maybe PinConnection
    , activeModules : Maybe ( Module, Module )
    , hoverAudioPin : Maybe PinConnection
    , attributeFilters : List AttrFilter
    , volume : Float
    }


type alias AttrFilter =
    { attrName : String
    , selected : List String
    }


type alias PinConnection =
    { out : String
    , into : String
    }


type alias Module =
    { name : String
    , controls : List Control
    }


type Control
    = KnobCtrl Knob
    | SwitchCtrl Switch


type alias Knob =
    { name : String
    , value : Maybe Float
    }


type alias Switch =
    { name : String
    , case_ : Maybe String
    }
