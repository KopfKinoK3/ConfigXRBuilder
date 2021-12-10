port module Outside exposing
    ( InfoForElm(..)
    , InfoForOutside(..)
    , getInfo
    , sendInfo
    )

{-| This module manages all interactions with the external JS-world.
-}

import Codec.Document
import Document exposing (Document, Metadata)
import Json.Decode as D exposing (Decoder, bool, int, list, nullable, string)
import Json.Decode.Pipeline as JP exposing (required)
import Json.Encode as Encode


port infoForOutside : GenericOutsideData -> Cmd msg


port infoForElm : (GenericOutsideData -> msg) -> Sub msg


type alias GenericOutsideData =
    { tag : String, data : Encode.Value }


type InfoForElm
    = GotClipboard String
    | GotFileList (List Metadata)
    | GotFile Document


type InfoForOutside
    = AskForClipBoard Encode.Value
    | WriteToClipBoard String
    | WriteFile Document
    | CreateFile Document
    | AskForFileList
    | AskForFile String
    | Convert String
    --| DeleteFileFromLocalStorage String
    --| Highlight ( Maybe String, String )
    --| WriteMetadata Metadata


getInfo : (InfoForElm -> msg) -> (String -> msg) -> Sub msg
getInfo tagger onError =
    infoForElm
        (\outsideInfo ->
            case outsideInfo.tag of
                "GotClipboard" ->
                    case D.decodeValue clipboardDecoder outsideInfo.data of
                        Ok result ->
                            tagger <| GotClipboard result

                        Err e ->
                            onError <| ""

                "GotFileList" ->
                    case D.decodeValue Codec.Document.documentListDecoder outsideInfo.data of
                        Ok fileList ->
                            tagger <| GotFileList fileList

                        Err _ ->
                            onError <| "Error getting file list"

                "GotFile" ->
                    case D.decodeValue Codec.Document.documentDecoder outsideInfo.data of
                        Ok file ->
                            tagger <| GotFile file

                        Err _ ->
                            onError <| "Error decoding file from value"

                _ ->
                    onError <| "Unexpected info from outside"
        )


sendInfo : InfoForOutside -> Cmd msg
sendInfo info =
    case info of
        AskForClipBoard _ ->
            infoForOutside { tag = "AskForClipBoard", data = Encode.null }

        WriteToClipBoard str ->
            infoForOutside { tag = "WriteToClipboard", data = Encode.string str }

        WriteFile document ->
            infoForOutside { tag = "WriteFile", data = Codec.Document.documentEncoder document }

        Convert str ->
            infoForOutside { tag = "Convert", data = Encode.string str }
            
        CreateFile document ->
            infoForOutside { tag = "CreateFile", data = Codec.Document.documentEncoder document }

        AskForFileList ->
            infoForOutside { tag = "AskForFileList", data = Encode.null }

        AskForFile fileName ->
            infoForOutside { tag = "AskForFile", data = Encode.string fileName }

        --DeleteFileFromLocalStorage fileName ->
        --    infoForOutside { tag = "DeleteFileFromLocalStorage", data = Encode.string fileName }

        --Highlight idPair ->
        --    infoForOutside { tag = "Highlight", data = encodeSelectedIdData idPair }

        --WriteMetadata metadata ->
        --    infoForOutside { tag = "WriteMetadata", data = Codec.Document.metadataEncoder metadata }



--encodeSelectedIdData : ( Maybe String, String ) -> Encode.Value
--encodeSelectedIdData ( maybeLastId, id ) =
--    Encode.object
--        [ ( "lastId", Encode.string (maybeLastId |> Maybe.withDefault "nonexistent") )
--        , ( "id", Encode.string id )
--        ]


encodeFile : ( String, String ) -> Encode.Value
encodeFile ( fileName, fileContents ) =
    Encode.object
        [ ( "fileName", Encode.string fileName )
        , ( "fileContents", Encode.string fileContents )
        ]


-- DECODERS --


clipboardDecoder : D.Decoder String
clipboardDecoder =
    --    D.field "data" D.string
    D.string
