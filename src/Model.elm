module Model exposing (AttrFilter, Model, PinConnection, SortDirection(..), initModel)

import AudioModel exposing (AudioModel)
import Browser.Navigation exposing (Key)
import Patch as P
import PinTable
import Routing exposing (Route(..), urlToRoute)
import SynthiSchema as SS
import Url exposing (Url)
import ViewModel exposing (..)


type alias Model =
    { navKey : Key
    , currentRoute : Route
    , scroll : Int
    , hoverKnob : ( Int, Int )
    , audioPinModel : PinTable.PinModel
    , controlPinModel : PinTable.PinModel
    , synthiSchema : Maybe SS.SynthiSchema
    , patches : Maybe (List P.Patch)
    , error : Maybe String
    , activeModules : Maybe ( Module, Module )
    , attributeFilters : List AttrFilter
    , activeControl : ( Maybe String, Maybe String )
    , volume : Float
    , sortOrder : SortDirection
    , sortBy : String
    }


initModel : Url -> Key -> Model
initModel url key =
    { navKey = key
    , currentRoute = urlToRoute url
    , scroll = 0
    , audioPinModel = PinTable.initModel
    , controlPinModel = PinTable.initModel
    , hoverKnob = ( -1, -1 )
    , synthiSchema = Nothing
    , patches = Nothing
    , error = Nothing
    , activeModules = Nothing
    , attributeFilters = []
    , activeControl = ( Nothing, Nothing )
    , volume = 0.5
    , sortOrder = Ascending
    , sortBy = "title"
    }


type SortDirection
    = Ascending
    | Descending


type alias AttrFilter =
    { attrName : String
    , selected : List String
    }


type alias PinConnection =
    { out : String
    , into : String
    }
