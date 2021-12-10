module Codec.Document exposing
    ( decodeMiniFileRecord
    , documentDecoder
    , documentEncoder
    , documentListDecoder
    , extendedDocumentEncoder
    , messageDecoder
    , metadataEncoder
    )

import Document exposing (Document, Metadata)
import Json.Decode as D exposing (Decoder, bool, int, list, nullable, string)
import Json.Decode.Pipeline as JP exposing (required)
import Json.Encode as Encode



-- DECODERS


documentListDecoder : D.Decoder (List Metadata)
documentListDecoder =
    D.list decodeMiniFileRecord


documentDecoder : Decoder Document
documentDecoder =
    D.succeed Document
        |> required "fileName" string
        |> required "content" string


decodeMiniFileRecord : Decoder Metadata
decodeMiniFileRecord =
    D.succeed Metadata
        |> required "fileName" string


type alias MessageContainer =
    { msg : String }


messageDecoder : Decoder String
messageDecoder =
    (D.succeed MessageContainer
        |> required "msg" string
    )
        |> D.map .msg



-- ENCODERS


metadataEncoder : Metadata -> Encode.Value
metadataEncoder record =
    Encode.object
        [ ( "fileName", Encode.string record.fileName )
        ]


documentEncoder : Document -> Encode.Value
documentEncoder doc =
    Encode.object
        [ ( "fileName", Encode.string doc.fileName )
        , ( "content", Encode.string doc.content )
        ]


extendedDocumentEncoder : String -> Document -> Encode.Value
extendedDocumentEncoder token doc =
    Encode.object
        [ ( "fileName", Encode.string doc.fileName )
        , ( "content", Encode.string doc.content )
        , ( "token", Encode.string token )
        ]
