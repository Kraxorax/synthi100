module Model exposing (AttrFilter, Model, PinConnection, SortDirection(..), initModel)

import AudioModel exposing (AudioModel)
import Browser.Navigation exposing (Key)
import Url exposing (Url)
import Patch as P
import PinTable
import Routing exposing (Route(..), urlToRoute)
import SynthiSchema as SS
import ViewModel exposing (..)


type alias Model =
    { navKey : Key
    , currentRoute : Route
    , scroll : Int
    , hoverKnob : ( Int, Int )
    , pinModel : PinTable.PinModel
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
    , pinModel = PinTable.initModel
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
