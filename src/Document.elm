module Document exposing
    ( DocType(..)
    , Document
    , Metadata
    , NewDocumentData
    , miniFileRecord
    , new
    )


type DocType
    = ConfigJSON
    | HtmlDoc
    | JpegDoc
    | PngDoc
    | RealityDoc
    | UsdzDoc


type alias Document =
    { fileName : String
    , content : String
    }


type alias Metadata =
    { fileName : String
    }


type alias NewDocumentData =
    { fileName : String
    , docType : DocType
    , content : String
    }


new : NewDocumentData -> Document
new data =
    let
        ext =
            case data.docType of
                ConfigJSON ->   ".cfg"
                HtmlDoc ->      ".html"
                UsdzDoc ->      ".usdz"
                RealityDoc ->   ".reality"
                JpegDoc ->      ".jpg"
                PngDoc ->       ".png"

        fileName =
            data.fileName ++ ext
    in
    { fileName = fileName, content = data.content }


miniFileRecord : Document -> Metadata
miniFileRecord doc =
    { fileName = doc.fileName }
