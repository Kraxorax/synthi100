module SynthiSchema exposing (Attribute, Connection, Control, Module, SynthiSchema, noModule, schemaDecoder)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (optional, required)


type alias SynthiSchema =
    { attributes : List Attribute
    , audioPanel : List Connection
    , controlPanel : List Connection
    , modules : List Module
    , userInfo : Maybe String
    }


type alias Attribute =
    { name : String
    , values : List String
    }


type alias Connection =
    { name : String
    , module_ : String
    }


gc : Connection
gc =
    { name = "a", module_ = "b" }


gc1 : Connection
gc1 =
    Connection "a" "b"


type alias Module =
    { name : String
    , controls : List Control
    }


type alias Control =
    { name : String
    , type_ : String
    , positions : Maybe (List String)
    }


schemaDecoder : Decoder SynthiSchema
schemaDecoder =
    succeed SynthiSchema
        |> required "attributes" (list attributeDecoder)
        |> required "audio_panel" (list connectionDecoder)
        |> required "control_panel" (list connectionDecoder)
        |> required "modules" (list moduleDecoder)
        |> optional "user_info" (nullable string) Nothing


connectionDecoder : Decoder Connection
connectionDecoder =
    succeed Connection
        |> required "name" string
        |> required "module" string


attributeDecoder : Decoder Attribute
attributeDecoder =
    succeed Attribute
        |> required "name" string
        |> required "values" (list string)


moduleDecoder : Decoder Module
moduleDecoder =
    succeed Module
        |> required "name" string
        |> required "controls" (list controlDecoder)


controlDecoder : Decoder Control
controlDecoder =
    succeed Control
        |> required "name" string
        |> required "type" string
        |> optional "positions" (nullable (list string)) Nothing


noModule =
    { name = "!-module not found"
    , controls = []
    }
