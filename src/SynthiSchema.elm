module SynthiSchema exposing (Module, SynthiSchema, schemaDecoder)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (optional, required)


type alias SynthiSchema =
    { modules : List Module
    }


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
        |> required "modules" (list moduleDecoder)


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
        |> required "positioins" (nullable (list string))
