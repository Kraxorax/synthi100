module Model exposing (AttrFilter, Model, PinConnection, SortDirection(..), initModel, AssetFlags)

import AudioModel exposing (AudioModel)
import Browser.Navigation exposing (Key)
import Patch as P
import PinTable
import Routing exposing (Route(..), urlToRoute, extractDownloadParam)
import SynthiSchema as SS
import Url exposing (Url)
import ViewModel exposing (..)


type alias A =
    { x : Int, y : String, f : Bool, d : Int }

type alias AssetFlags =
    { woff :
        { metropolisBold : String
        , metropolisMedium : String
        , metropolisSemiBold : String
        }
    , svg :
        { mute : String
        , unmute : String
        , pause : String
        , play : String
        , selectArrowDown : String
        , sortArrowDown : String
        , sortArrowDownSelected : String
        }
    , png :
        { synthiLogo : String
        , footerLogo : String
        }
    }


type alias Model =
    { navKey : Key
    , currentRoute : Route
    , scroll : Int
    , hoverKnob : ( Int, Int )
    , audioPinModel : PinTable.PinModel
    , controlPinModel : PinTable.PinModel
    , synthiSchema : Maybe SS.SynthiSchema
    , patches : Maybe (List P.Patch)
    , filteredPatches : List P.Patch
    , error : Maybe String
    , attributeFilters : List AttrFilter
    , activeControl : ( Maybe String, Maybe String )
    , volume : Float
    , loop : Bool
    , sortOrder : SortDirection
    , sortBy : String
    , downloadFile : Maybe String
    , userInfo : Maybe String
    , assets : AssetFlags
    }


initModel : Url -> Key -> AssetFlags ->  Model
initModel url key flags =
    { navKey = key
    , currentRoute = urlToRoute url
    , scroll = 0
    , audioPinModel = PinTable.initModel
    , controlPinModel = PinTable.initModel
    , hoverKnob = ( -1, -1 )
    , synthiSchema = Nothing
    , patches = Nothing
    , filteredPatches = []
    , error = Nothing
    , attributeFilters = []
    , activeControl = ( Nothing, Nothing )
    , volume = 0.5
    , loop = True
    , sortOrder = Ascending
    , sortBy = "title"
    , downloadFile = extractDownloadParam url
    , userInfo = Nothing
    , assets = flags
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
