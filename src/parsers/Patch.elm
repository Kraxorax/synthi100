module Patch exposing (Control(..), ModuleSettings, Patch, Pin, noPatch, patchesDecoder)

import AudioModel exposing (AudioModel)
import Json.Decode exposing (..)
import Json.Decode.Extra exposing (optionalField, when, withDefault)
import Json.Decode.Pipeline exposing (hardcoded, optional, optionalAt, required, requiredAt)
import Maybe.Extra exposing (isJust)


type alias Patch =
    { soundUrl : String
    , waveformSmall : String
    , waveformBig : String
    , title : String
    , duration : Float
    , attributeValues : List String
    , score : String
    , audioPins : List Pin
    , controlPins : List Pin
    , moduleSettings : List ModuleSettings
    , audioModel : Maybe AudioModel
    }


type alias Pin =
    { into : Int
    , out : Int
    , color : String
    }


type alias ModuleSettings =
    { name : String
    , controlValues : List Control
    }


type Control
    = KnobVal Knob
    | SwitchVal Switch


type alias Knob =
    { name : String
    , position : Float
    }


type alias Switch =
    { name : String
    , case_ : String
    }


patchesDecoder : Decoder (List Patch)
patchesDecoder =
    list patchDecoder


patchDecoder : Decoder Patch
patchDecoder =
    succeed Patch
        |> required "sound_url" string
        |> required "waveform_small" string
        |> required "waveform_big" string
        |> required "title" string
        |> required "duration" float
        |> required "attribute_values" (list string)
        |> required "score" string
        |> required "audio_pins" (list pinDecoder)
        |> required "control_pins" (list pinDecoder)
        |> required "module_settings" (list moduleSettingsDecoder)
        |> hardcoded Nothing


pinDecoder : Decoder Pin
pinDecoder =
    succeed Pin
        |> required "in" int
        |> required "out" int
        |> required "color" string


moduleSettingsDecoder : Decoder ModuleSettings
moduleSettingsDecoder =
    succeed ModuleSettings
        |> required "name" string
        |> required "control_values" (list controlDecoder)


controlDecoder : Decoder Control
controlDecoder =
    oneOf
        [ map KnobVal <| when controlType isJust knobDecoder
        , map SwitchVal <| when controlType (isJust >> not) switchDecoder
        ]


knobDecoder : Decoder Knob
knobDecoder =
    succeed Knob
        |> requiredAt [ "0" ] string
        |> requiredAt [ "1" ] knobValDecoder


knobValDecoder : Decoder Float
knobValDecoder =
    field "position" float


switchDecoder : Decoder Switch
switchDecoder =
    succeed Switch
        |> requiredAt [ "0" ] string
        |> requiredAt [ "1" ] switchValDecoder


switchValDecoder : Decoder String
switchValDecoder =
    field "case" string


controlType : Decoder (Maybe Float)
controlType =
    index 1 (maybe (field "position" float))


noPatch =
    { soundUrl = ""
    , waveformSmall = ""
    , waveformBig = ""
    , title = ""
    , duration = -0.1
    , attributeValues = []
    , score = ""
    , audioPins = []
    , controlPins = []
    , moduleSettings = []
    , audioModel = Nothing
    }


noPin =
    { into = -1
    , out = -1
    , color = ""
    }
