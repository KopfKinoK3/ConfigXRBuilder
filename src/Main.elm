module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Events
import Cmd.Extra exposing (withCmd, withCmds, withNoCmd)
import Delay exposing (sequence)
import Element
    exposing
        ( Element
        , alignRight
        , centerX, centerY
        , column
        , el
        , fill, fillPortion
        , height
        , paddingXY, padding
        , px
        , rgb255
        , row
        , spacing, scrollbarY
        , text
        , width, wrappedRow
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Input exposing (labelRight, labelHidden)
import File exposing (File)
import Html exposing (Attribute, Html, s)
import Html.Attributes as Attribute
import Json.Decode as Decode exposing (Decoder, int, string, float, dict, list, bool, oneOf, andThen, keyValuePairs, field, maybe)
import Json.Decode.Pipeline exposing (required, hardcoded, resolve, custom, optional)
import Json.Encode as Encode
import Json.Encode.Extra
import Task exposing (Task)
import Time

import Outside
import Char exposing (isDigit, isAlphaNum, toCode)
import Dict exposing (Dict)
import String exposing (indices)

import TestScene exposing (testScene, cubeMesh, k3Mesh)

import ConfigXR.Types exposing 
    ( Anchor(..)
    , AnchorImage
    , AnimationMeta
    , SubdivisionScheme(..)
    , Interpolation(..)
    , Texture, defaultTx
    , Mesh
    , SkelAnim
    , Xform
    , Material
    , ColliderShape(..)
    , Physics
    , HorAlign(..)
    , VerAlign(..)
    , RodOrient(..)
    , TextAttr
    , UsdInput
    , Look
    , LookGroup
    , LanguageString
    , ToggleItem
    , StaticItem
    , SelectItem
    , SelectToggleItem
    , MultiItem
    , Product
    , Form
    , Item(..)
    , Translation
    , ConfigScene
    , InfoScene
    , Panel
    , Page
    , UsdTrigger(..)
    , UsdAction(..)
    , UsdBehavior
    , newLook
    , defaultPhysics
    , defaultTranslations
    , defaultAnimMeta
    , origin
    , verticalOrientation
    , floorOrientation
    )

import ConfigXR.Helper exposing (simpleLooks, sanitize, fixDescr, toLinearStr, ralLook)
import ConfigXR.UIMeshes exposing (..)
import ConfigXR.KultKaffeeProject exposing (infoScene)
--import ConfigXR.CosmosLacProject exposing (configScene)
--import ConfigXR.RALProject exposing (configScene)
--import ConfigXR.SofaProject exposing (configScene)
--import ConfigXR.SwivelOrangeProject exposing (configScene)
--import ConfigXR.GroheBlueProject exposing (configScene)
import ConfigXR.GandaProject exposing (configScene)
import Intro.K3Company exposing (company)

type Msg
    = NoOp
    | NoOpStr String
    | WindowSize Int Int
    | LogErr String
    | OutsideInfo Outside.InfoForOutside
    | Outside Outside.InfoForElm
    | AddLanguage
    | SetExportLanguage
    | ChangeLanguage String
    | ToggleLanguage String
    | ExportScene
    | LoadTestDoc
    | LoadChairDoc
    | LoadInfoDoc
    | SaveScene Int
    | SaveElm
    | NewDoc
    | OpenDoc
    | DeleteLook Int String String
    | ChangeGrpName Int String String
    | ChangeGrpDescr Int String String String
    | DeleteGrpById Int String 
    | AddLookToGrp Int String
    | AddRALToGrp Int String String String
    | AddLookGroupToItemIdx Int 
    | ChangeItemId Int String 
    | DeleteItemByIdx Int

    | ChangeMaterialId String Int String String String
    | ChangeItemDescr String Int String String
    | ChangeLook String Int String String (String -> Look -> Look) String
    | ChangeConfigScene String (ConfigScene -> String -> ConfigScene) String
    | ChangeSelectItem String String (SelectItem -> String -> SelectItem) String
    | ChangeSelectToggleItem String String (SelectToggleItem -> String -> SelectToggleItem) String

    | ChangeRALCode String
    | ChangeRALSuffix String

    | ChangeInfoScene String (InfoScene -> String -> InfoScene) String
    | ChangeInfoPanel String String (Panel -> String -> Panel) String
    | ChangePanelStartOpen String String Bool
    | ChangePanelTitle String Int String String
    | ChangePanelSubTitle String Int String String
    | DeletePanelByIdx Int
    | ChangePage String String String (Page -> String -> Page) String

reduce : Dict String Bool -> LanguageString -> LanguageString
reduce languages ls =
    Dict.filter
        (\l _ ->
            Maybe.withDefault True <| Dict.get l languages
        ) 
        ls


fallBackLang= "de"
langGet : String -> LanguageString -> String
langGet l ls =
    case Dict.get l ls of
        Nothing ->
            case String.split "-" l of
                master :: _ ->
                    case Dict.get master ls of
                        Nothing -> 
                            (Dict.get fallBackLang ls 
                                |> Maybe.withDefault ("?? "++l++ " ??"))
                        Just str -> str
                _ ->
                    (Dict.get fallBackLang ls 
                        |> Maybe.withDefault ("?? "++l++ " ??"))
        Just str -> str


langAdd : String -> String -> LanguageString -> LanguageString
langAdd newLang newStr dict =
    Dict.update
        newLang 
        (\mbValue ->
            case mbValue of
                Just _ -> 
                    mbValue 
                Nothing ->
                    Just newStr
        )
        dict


type alias Flags =
    { width : Float, height : Float }


type SceneModel
    = MainMenu
    | Config ConfigScene
    | Info InfoScene


type alias Model =
    { width : Float
    , height : Float
    , scenes : List SceneModel
    , newLang : String
    , exportLang : String
    , languages : Dict String Bool
    }


offsetPointsX : 
    { offset : Float
    , min : Float
    , max : Float
    } -> Mesh -> Mesh
offsetPointsX p mesh =
    { mesh | points=
        List.map
            (\(x,y,z) ->
                ( if x <= p.max && x >= p.min
                    then x + p.offset
                    else x
                , y
                , z
                )
            )
            mesh.points
    }


offsetPointsY : 
    { offset : Float
    , min : Float
    , max : Float
    } -> Mesh -> Mesh
offsetPointsY p mesh =
    { mesh | points=
        List.map
            (\(x,y,z) ->
                ( x
                , if y <= p.max && y >= p.min
                    then y + p.offset
                    else y
                , z
                )
            )
            mesh.points
    }


offsetPointsZ : 
    { offset : Float
    , min : Float
    , max : Float
    } -> Mesh -> Mesh
offsetPointsZ p mesh =
    { mesh | points=
        List.map
            (\(x,y,z) ->
                ( x
                , y
                , if z <= p.max && z >= p.min
                    then z + p.offset
                    else z
                )
            )
            mesh.points
    }


resizeRoundedBox : Float -> Float -> Mesh -> Mesh
resizeRoundedBox newW newH mesh =
    { mesh | points=
        List.map
            (\(x,y,z) ->
                let
                    left = x < -0.47
                    right = x > 0.47
                    top = z > 0.47
                    bottom = z < -0.47
                    xOff = (newW - 1.08) / 2.0
                    zOff = (newH - 1.08) / 2.0
                in
                    case ( (left, right), (top, bottom) ) of
                        ( (True, False), (True, False) ) -> -- topLeft
                            (x - xOff, y, z + zOff)
                        ( (False, True), (True, False) ) -> -- topRight
                            (x + xOff, y, z + zOff)
                        ( (False, False),(True, False) ) -> -- topCenter
                            (x, y, z + zOff)
                        ( (True, False), (False, True) ) -> -- bottomLeft
                            (x - xOff, y, z - zOff)
                        ( (False, True), (False, True) ) -> -- bottomRight
                            (x + xOff, y, z - zOff)
                        ( (False, False),(False, True) ) -> -- bottomCenter
                            (x, y, z - zOff)
                        ( (True, False), _) -> -- left
                            (x - xOff, y, z)
                        ( (False, True), _) -> -- right
                            (x + xOff, y, z)
                        --( _, (True, False)) -> -- top
                        --    (x, y, z + zOff)
                        --( _, (False, True)) -> -- bottom
                        --    (x, y, z - zOff)
                        _ -> (x,y,z) -- unchanged
            )
            mesh.points
    }

usdPanel : String -> String -> Float -> Float -> Float -> Float -> Xform -> List String
usdPanel scene name w h hBelow factor xForm =
    let
        hTop= h - hBelow
        scale= String.fromFloat (1.0/factor)
        scaleLine= "double3 xformOp:scale = (" ++ scale ++", "++ scale ++", "++ scale ++")"
    in
    usdGroup name Nothing xForm
      [ "def Xform \"Ground\" {"
      , "rel material:binding = </Root/Scenes/"++scene++"/UIPanelMaterial>"
      , "quatf xformOp:orient = (1, 0, 0, 0)"
      , scaleLine
      , "double3 xformOp:translate = (0, 0.0032, 0)"
      , "uniform token[] xformOpOrder = [\"xformOp:translate\", \"xformOp:orient\", \"xformOp:scale\"]"
      , usdMesh "GroundMesh" Nothing <| resizeRoundedBox (w*factor) (h*factor) uiPanel100Ground
      , "}"
      , "def Xform \"Border\" {"
      , "rel material:binding = </Root/Scenes/"++scene++"/UIPanelAccent>"
      , "quatf xformOp:orient = (1, 0, 0, 0)"
      , scaleLine
      , "double3 xformOp:translate = (0, 0.0016, 0)"
      , "uniform token[] xformOpOrder = [\"xformOp:translate\", \"xformOp:orient\", \"xformOp:scale\"]"
      , usdMesh "BorderMesh" Nothing <| resizeRoundedBox (w*factor) (h*factor) uiPanel100Border
      , "}"
      , "def Xform \"FaceBelow\" {"
      , "rel material:binding = </Root/Scenes/"++scene++"/UIPanelMaterial>"
      , "quatf xformOp:orient = (1, 0, 0, 0)"
      , scaleLine
      , "double3 xformOp:translate = (0, 0, "++ String.fromFloat ((hBelow - h) * 0.5) ++")"
      , "uniform token[] xformOpOrder = [\"xformOp:translate\", \"xformOp:orient\", \"xformOp:scale\"]"
      , usdMesh "FaceMesh" Nothing <| resizeRoundedBox (w*factor) (hBelow*factor) uiPanel100Face
      , "}"
      --, "def Xform \"FaceTop\" {"
      --, "rel material:binding = </Root/Scenes/"++scene++"/UIPanelMaterial>"
      --, "quatf xformOp:orient = (1, 0, 0, 0)"
      --, scaleLine
      --, "double3 xformOp:translate = (0, 0, "++ String.fromFloat ((h-hTop) * 0.5) ++")"
      --, "uniform token[] xformOpOrder = [\"xformOp:translate\", \"xformOp:orient\", \"xformOp:scale\"]"
      --, usdMesh "FaceMesh" Nothing <| resizeRoundedBox (w*factor) (hTop*factor) uiPanel100Face
      --, "}"
      ]


usdMaterial : String -> String -> Material -> String
usdMaterial prefix name mat =
    let
        inputWithTx= List.filterMap
            (\i -> case i.texture of
                Nothing -> Nothing
                Just tx -> Just (i.id, tx)
            )
            mat.inputs
        (needsCoordReader, needsTransform)= 
            case inputWithTx of
                [] -> (False, [])
                txList ->
                    (True
                    , List.filter
                        (\(_, tx) ->
                            case (tx.rotation, tx.scale, tx.translation) of
                                ( Nothing, Nothing, Nothing ) -> False
                                _ -> True
                        )
                        txList
                    )
    in
    String.join "\n"
      ( [ "def Material \"" ++ name ++ "\" {"
        , "token outputs:surface.connect = <" ++ prefix ++ "/" ++ name ++ "/PBRShader.outputs:surface>"
        --, "token outputs:displacement"  -- TODO connect
        , "def Shader \"PBRShader\" {"
        , "uniform token info:id = \"UsdPreviewSurface\""
        ]
      ++ ( List.filterMap (\x -> x)
            ( List.concatMap
                (\input ->
                    if String.contains "," input.values
                        then
                            [ Just ("color3f inputs:"++ input.id ++" = " ++ input.values)
                            , Maybe.map (\_ -> "color3f inputs:"++ input.id ++".connect = <" ++ prefix ++ "/" ++ name ++ "/"++ input.id ++"_texture.outputs:rgb>") input.texture
                            ]
                        else
                            [ Just ("float inputs:" ++ input.id ++ " = " ++ input.values)
                            , Maybe.map (\_ -> "float inputs:"++ input.id ++".connect = <" ++ prefix ++ "/" ++ name ++ "/"++ input.id ++"_texture.outputs:r>") input.texture
                            ]
                )
                mat.inputs
        ) )
      ++
        [ "int inputs:useSpecularWorkflow = 0"
        , "token outputs:surface"
        , "token outputs:displacement"
        , "}"
        ]
      ++( List.map
            (\(id, tx) ->
                String.join "\n"
                    [ "def Shader \"transform2d"++ id ++"\""
                    , "{"
                    , "uniform token info:id = \"UsdTransform2d\""
                    , "float2 inputs:in.connect = <" ++ prefix ++ "/" ++ name ++ "/"++ id ++"_texture.outputs:result>"
                    , "float inputs:rotation = (0.0)"
                    , "float2 inputs:scale = (1.0, 1.0)"
                    , "float2 inputs:translation = (0.0, 0.0)"
                    , "float2 outputs:result"
                    , "}"
                    ]
            )
            needsTransform
        )
      ++( if needsCoordReader
            then
                [ "def Shader \"texCoordReader\""
                , "{"
                , "uniform token info:id = \"UsdPrimvarReader_float2\""
                , "token inputs:varname = \"st\""
                , "float2 outputs:result"
                , "}"
                , "def Shader \"texCoordReader1\""
                , "{"
                , "uniform token info:id = \"UsdPrimvarReader_float2\""
                , "token inputs:varname = \"st1\""
                , "float2 outputs:result"
                , "}"
                ]
                ++ 
                ( List.concat <| List.filterMap
                    (\input ->
                        Maybe.map
                            (\tex ->
                                let
                                    suffix= if tex.useSecondUV then "1" else ""
                                in
                                [ "def Shader \"" ++ input.id ++"_texture\""
                                , "{"
                                , "uniform token info:id = \"UsdUVTexture\""
                                , "float4 inputs:bias = " ++ (
                                    case tex.pixelBias of
                                        Nothing -> "(0, 0, 0, 0)"
                                        Just p -> usdPixel p
                                )
                                , "float4 inputs:scale = "++ (
                                    case tex.pixelScale of
                                        Nothing -> "(1, 1, 1, 1)"
                                        Just p -> usdPixel p
                                )
                                , "asset inputs:file = @" ++ tex.file ++"@"
                                , "float2 inputs:st.connect = <" ++ prefix ++ "/"++ name ++"/texCoordReader"++ suffix ++".outputs:result>"
                                , if String.contains "," input.values
                                    then "float outputs:rgb"
                                    else "float outputs:r"
                                , "}"
                                ]
                            )
                            input.texture
                    )
                    mat.inputs
                )
            else []
        )
      ++ [ "}" ]
      )


main : Program Flags Model Msg
main =
    Browser.element
        { init= init
        , view= view
        , update= update
        , subscriptions= subscriptions
        }


emptyScene : ConfigScene
emptyScene =
    { id= "Scene"
    , urlPrefix= "http://www.kreativekk.de/Test" 
    , description= Dict.fromList [ ("en", "Sample Scene description"), ( "de", "Beispiel Szenenbeschreibung") ]
    , urlAR= ""
    , urlPreview= ""
    , shopUrl= Dict.fromList []
    , thumbnail= Nothing
    , uiScale= 1.0
    , uiXform= { scale= Just (1, 1, 1), orient = Just [1, 0, 0, 0], translate= Just (0,0,0) }
    , codeXform= { scale= Just (1, 1, 1), orient = Just [1, 0, 0, 0], translate= Just (0,0.1,0) }
    , anchor= Horizontal
    , translations= defaultTranslations
    , items= []
    , ralId= ""
    , ralSuffix= ""
    , animMeta= defaultAnimMeta
    , expandedView= False
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { width= flags.width
      , height= flags.height
      , scenes= [ MainMenu ]
      , newLang= "de"
      , exportLang= "de"
      , languages= Dict.fromList [ ("de", True) ]
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onResize WindowSize
        --, ContextMenu.subscriptions (Editor.getContextMenu model.editor)
        --    |> Sub.map ContextMenuMsg
        --    |> Sub.map EditorMsg
        , Outside.getInfo Outside LogErr
        --, Time.every Config.tickInterval Tick
        ]


usdHeader : String -> AnimationMeta -> String -> String
usdHeader id anim masters =
    String.concat [ """#usda 1.0
(
customLayerData = {
string creator = "mobi.config-xr.CXRBuilder Version 0.1 beta"
string identifier = """, "\"", id,  "\"", """
dictionary Apple = { int preferredIblVersion = 2 }
}
defaultPrim = "Root"
metersPerUnit = 1
upAxis = "Y"
)
""", masters , """
def Xform "Root"
{
def Scope "Scenes" (
kind = "sceneLibrary"
)
{
""" ]

{-
timeCodesPerSecond = """ , String.fromFloat anim.timeCodesPerSecond, """
startTimeCode = """ , String.fromFloat anim.startTimeCode, """
endTimeCode  = """ , String.fromFloat anim.endTimeCode, """
autoPlay = """, if anim.autoPlay then "true" else "false", """
-}


usdSceneStart : String -> Anchor -> String
usdSceneStart scene anchor =
    """def Xform \"""" ++ scene ++ """\" (
apiSchemas = ["Preliminary_AnchoringAPI"]
customData = {
bool preliminary_collidesWithEnvironment = 0
string sceneName = \"""" ++ scene ++ """\"
}
sceneName = \"""" ++ scene ++ """\"
)
{
""" ++ ( case anchor of
            Image ai     ->
                String.join "\n"
                    [ "uniform token preliminary:anchoring:type = \"image\""
                    , "rel preliminary:imageAnchoring:referenceImage = </Root/Scenes/"++scene++"/AnchoringReferenceImage>"
                    , "def Preliminary_ReferenceImage \"AnchoringReferenceImage\""
                    , "{"
                    , "uniform asset image = @" ++ ai.filePath ++ "@"
                    , "uniform double physicalWidth = " ++ String.fromFloat ai.physicalWidth
                    , "}"
                    ]
            Horizontal  -> "token preliminary:anchoring:type = \"plane\"\ntoken preliminary:planeAnchoring:alignment = \"horizontal\""
            Vertical    -> "token preliminary:anchoring:type = \"plane\"\ntoken preliminary:planeAnchoring:alignment = \"vertical\""
        )
    ++ """
def Xform "Gravity"
{
double3 physics:gravitationalForce:acceleration = (0, -9.800000190734863, 0)
}

def Mesh "sceneGroundPlane" (
apiSchemas = ["Preliminary_PhysicsColliderAPI"]
customData = {
bool preliminary_isSceneGroundPlane = 1
}
)
{
rel material:binding = </Root/Scenes/"""++scene++"""/sceneGroundPlane/physicsMaterial>
float3 normal = (0, 1, 0)
float3 position = (0, 0, 0)
rel preliminary:physics:collider:convexShape = </Root/Scenes/"""++scene++"""/sceneGroundPlane>

def Material "physicsMaterial" (
apiSchemas = ["Preliminary_PhysicsMaterialAPI"]
)
{
uniform double preliminary:physics:material:friction:dynamic = 0.58
uniform double preliminary:physics:material:friction:static = 0.58
uniform double preliminary:physics:material:restitution = 0.48
}
}"""


usd2dList : List (Float, Float) -> String
usd2dList l =
    "[ " ++ ( String.join ", " 
        ( List.map 
            (\(s,t) -> "( " ++ String.fromFloat s ++ ", " ++ String.fromFloat t ++ " )" )
            l
        ) 
    ) ++ " ]"



usdPixel : List Float -> String
usdPixel p =
    String.concat
        [ "("
        , String.join ", " ( List.map  (\v -> String.fromFloat v ) p )
        , ")"
        ]


usd3dList : List (Float, Float, Float) -> String
usd3dList l =
    "[ " ++ ( String.join ", "
        ( List.map
            (\(x,y,z) -> "( " ++ String.fromFloat x ++ ", " ++ String.fromFloat y ++ ", " ++ String.fromFloat z ++ " )" )
            l
        ) 
    ) ++ " ]"


usdQuadList : List (List Float) -> String
usdQuadList outerList =
    "( " ++ ( String.join "), (" <| List.map (\innerList -> String.join ", " <| List.map String.fromFloat innerList ) outerList
    ) ++ " )"


usdMatrixList : List (List (List Float)) -> String
usdMatrixList matrix = 
    "[ ( (" ++
        (String.join ")), ((" <| List.map
            (\outerList ->
                ( String.join "), (" <| List.map (\innerList -> String.join ", " <| List.map String.fromFloat innerList ) outerList )
            )
            matrix
        )
    ++ ") ) ]"


usdIntList : List Int -> String
usdIntList indices =
    "[ " ++ (String.join ", " (List.map String.fromInt indices)) ++ " ]"



usdShapeAsset : String -> String -> String -> String
usdShapeAsset name meshRef matRef =
    "def \"" ++ name ++ """\" { def "Mesh0" {
rel material:binding = </AssetFactories/Masters/Factories/""" ++ name ++ """/Mesh0/Material>
def "Mesh0" (\ninstanceable = true\nprepend references = """ ++ meshRef ++"""\n) {}
def "Material" (\nprepend references = """ ++ matRef ++ """\n) {}\n}\n}"""
    -- TODO Skel ?

usdPlace : String -> String -> Bool ->  Maybe String -> Xform -> Physics -> String
usdPlace path name visible overwriteRef xForm physics =
    let
        ( shape, extra ) =
            case physics.colliderShape of
                Cube s -> 
                    ( "Cube"
                    , "double size = " ++ String.fromFloat s
                    )
                Capsule cap -> 
                    ( "Capsule"
                    , "uniform token axis = \""++ 
                        cap.axis ++"\"\ndouble height = "++
                        String.fromFloat cap.height ++ 
                        "\ndouble radius = "++ String.fromFloat cap.radius
                    )
    in
    "def Xform \"" ++ name ++ """" (
prepend apiSchemas = ["Preliminary_PhysicsColliderAPI"]
) {
rel material:binding = """++ path ++ name ++ "/PhysicsMaterial>\n" 
    {- ++ "token visibility = \"" ++ (if visible then "inherited" else "invisible") ++ "\"\n" -} 
    ++ usdXformBlock xForm ++ """
def Xform "Generated" (
prepend references = """ ++ 
    Maybe.withDefault ("</AssetFactories/Masters/Factories/PrimitiveShapeAssetFactory_" ++ name ++ ">") overwriteRef
    ++"""
) {}
def Xform "Children" {}
def Material "PhysicsMaterial" (
prepend apiSchemas = ["Preliminary_PhysicsMaterialAPI"]
) {
uniform double preliminary:physics:material:friction:dynamic = """ ++ String.fromFloat physics.frictionDynamic ++ """
uniform double preliminary:physics:material:friction:static = """ ++ String.fromFloat physics.frictionStatic ++ """
uniform double preliminary:physics:material:restitution = """ ++ String.fromFloat physics.restitution ++ """
}
over """
    ++ shape ++ """ "collider" {
float3[] extent = """
    ++ usd3dList physics.extent ++ """
uniform token purpose = "guide"
""" ++ extra ++ "\n"
    ++ usdXformBlock physics.xForm 
    ++ "\n}\n}\n"


interpolationToStr : Interpolation -> String
interpolationToStr i =
    String.concat
        [ " (\ninterpolation = \""
        , case i of
            Constant -> "constant"
            Uniform -> "uniform"
            Varying -> "varying"
            FaceVarying -> "faceVarying"
            Vertex -> "vertex"
        , "\"\n)\n"
        ]

subDToStr : SubdivisionScheme -> String
subDToStr s =
    String.concat
        [ "\nuniform token subdivisionScheme = \""
        , case s of
            None -> "none"
            CatmullClark -> "catmullClark"
            Loop -> "loop"
            Bilinear -> "bilinear"
        , "\"\n"
        ]

usdExtend : Mesh -> String
usdExtend mesh =
    let
        gatherExtend= List.foldl
            (\(x, y, z) e ->
                { maxX= if x > e.maxX then x else e.maxX
                , minX= if x < e.minX then x else e.minX
                , maxY= if y > e.maxY then x else e.maxY
                , minY= if y < e.minY then x else e.minY
                , maxZ= if z > e.maxZ then x else e.maxZ
                , minZ= if z < e.minZ then x else e.minZ
                }
            )
            { minX= 999999.0, maxX= -999999.0
            , minY= 999999.0, maxY= -999999.0
            , minZ= 999999.0, maxZ= -999999.0
            }
            mesh.points
        extend =
            [ ( gatherExtend.minX, gatherExtend.minY, gatherExtend.minZ)
            , ( gatherExtend.maxX, gatherExtend.maxY, gatherExtend.maxZ)
            ]
    in
    String.concat
        [ "\nfloat3[] extent = "
        , if mesh.noExtend then "[ (0,0,0), (0,0,0) ]" else usd3dList extend
        ]


-- für vertex
usdMesh : String -> Maybe SkelAnim -> Mesh -> String
usdMesh name skelAnim mesh =
    String.concat
        [ "def Mesh \""++ name ++"\" {"
        , "\nuniform bool doubleSided = " ++ if mesh.doubleSided then "1" else "0"
        , usdExtend mesh
        , "\nint[] faceVertexCounts = ", usdIntList mesh.faceVertexCounts
        , "\nint[] faceVertexIndices = ", usdIntList mesh.faceVertexIndices
        , "\npoint3f[] points = ", usd3dList mesh.points
        , case mesh.normals of
            [] -> ""
            _ -> String.concat ["\nnormal3f[] primvars:normals = ", usd3dList mesh.normals, interpolationToStr mesh.normalsInterpolation]
        , case mesh.normalIndices of
            Nothing -> ""
            Just i -> "\nint[] primvars:normals:indices = " ++ usdIntList i
        , case mesh.st of
            [] -> ""
            _ -> String.concat ["\ntexCoord2f[] primvars:st = ", usd2dList mesh.st, interpolationToStr mesh.stInterpolation]
        , case mesh.stIndices of
            Nothing -> ""
            Just i -> "\nint[] primvars:st:indices = " ++ usdIntList i
        , case mesh.st1 of
            [] -> ""
            _ -> String.concat ["\ntexCoord2f[] primvars:st1 = ", usd2dList mesh.st1, interpolationToStr mesh.st1Interpolation]
        , case mesh.st1Indices of
            Nothing -> ""
            Just i -> "\nint[] primvars:st1:indices = " ++ usdIntList i
        , case skelAnim of
            Nothing -> ""
            Just a ->
                let
                    attr = " (\nelementSize = " ++ String.fromInt a.elementSize ++ "\ninterpolation = \"vertex\"\n)"
                in
                String.concat
                    [ "\nrel skel:skeleton = </Root/Scenes/Ganda/Children/Object_Bezug/Skeleton>"   -- TODO
                    , "\nmatrix4d primvars:skel:geomBindTransform = ( " ++ usdQuadList a.geomBindTransform ++" )"
                    , "\nint[] primvars:skel:jointIndices = " ++ (usdIntList a.jointIndices) ++ attr
                    , "\nfloat[] primvars:skel:jointWeights = [ " ++ (String.join ", " <| List.map String.fromFloat a.jointWeights) ++ " ]" ++ attr
                    ]
        , subDToStr mesh.subdivisionScheme
        , "}\n"
        ]

usdSkel : SkelAnim -> String
usdSkel s =
    String.join "\n"
        [ "def Skeleton \"Skeleton\""
        , "{"
        , "rel skel:animationSource = </Root/Scenes/Ganda/Children/Object_Bezug/SkelAnimation>"   -- TODO
        , "uniform token[] joints = [ \""++ (String.join "\", \"" s.joints) ++"\" ]"
        , "uniform matrix4d[] bindTransforms = " ++ usdMatrixList s.bindTransforms
        , "uniform matrix4d[] restTransforms = " ++ usdMatrixList s.restTransforms
        , "}"
        ]


usdSkelAnim : SkelAnim -> String
usdSkelAnim s =
    String.join "\n"
        [ "def SkelAnimation \"SkelAnimation\""
        , "{"
        , case s.rotations of
            [(_,q)] -> "quatf[] rotations = " ++ usdQuadList q
            tsList ->
                "quatf[] rotations.timeSamples = {\n" ++
                    ( String.join ",\n" <|
                        List.map
                            (\(t,q) -> String.fromFloat t ++ " : [ " ++ usdQuadList q ++ " ]"
                            )
                            tsList
                    ) ++ "\n}"
        , "}"
        , case s.translations of
            [(_,trans)] -> "float3[] translations = " ++ (usd3dList trans)
            tsList ->
                "float3[] translations.timeSamples = {\n" ++
                    ( String.join ",\n" <|
                        List.map
                            (\(t,trans) -> String.fromFloat t ++ " : " ++ usd3dList trans
                            )
                            tsList
                    ) ++ "\n}"
        , case s.scales of
            [(_,scales)] -> "half3[] scales = " ++ (usd3dList scales)
            tsList ->
                "half3[] scales.timeSamples = {\n" ++
                    ( String.join ",\n" <|
                        List.map
                            (\(t,scales) -> String.fromFloat t ++ " : " ++ usd3dList scales
                            )
                            tsList
                    ) ++ "\n}"
        --, "}"
        --, ""
        ]


usdOver : String -> Maybe SkelAnim -> Mesh -> String
usdOver name skelAnim mesh =
    String.concat
        [ "over Mesh \""++ name ++"\" {"
        , usdExtend mesh
        , "\nint[] faceVertexCounts = ", usdIntList mesh.faceVertexCounts
        , "\nint[] faceVertexIndices = ", usdIntList mesh.faceVertexIndices
        , "\npoint3f[] points = ", usd3dList mesh.points
        , case mesh.normals of
            [] -> ""
            _ -> String.concat ["\nnormal3f[] primvars:normals = ", usd3dList mesh.normals, interpolationToStr mesh.normalsInterpolation]
        , case mesh.normalIndices of
            Nothing -> ""
            Just [] -> ""
            Just i -> "\nint[] primvars:normals:indices = " ++ usdIntList i
        , case mesh.st of
            [] -> ""
            _ -> String.concat ["\ntexCoord2f[] primvars:st = ", usd2dList mesh.st, interpolationToStr mesh.stInterpolation]
        , case mesh.stIndices of
            Nothing -> ""
            Just [] -> ""
            Just i -> "\nint[] primvars:st:indices = " ++ usdIntList i
        , case mesh.st1 of
            [] -> ""
            _ -> String.concat ["\ntexCoord2f[] primvars:st1 = ", usd2dList mesh.st1, interpolationToStr mesh.st1Interpolation]
        , case mesh.st1Indices of
            Nothing -> ""
            Just [] -> ""
            Just i -> "\nint[] primvars:st1:indices = " ++ usdIntList i
        , case skelAnim of
            Nothing -> ""
            Just a ->
                let
                    attr = " (\nelementSize = " ++ String.fromInt a.elementSize ++ "\ninterpolation = \"vertex\"\n)"
                in
                String.concat
                    [ "\nprepend rel skel:skeleton = <Skeleton>" -- TODO
                    , "\nmatrix4d primvars:skel:geomBindTransform = ( " ++ usdQuadList a.geomBindTransform ++" )"
                    , "\nint[] primvars:skel:jointIndices = " ++ (usdIntList a.jointIndices) ++ attr
                    , "\nfloat[] primvars:skel:jointWeights = [ " ++ (String.join ", " <| List.map String.fromFloat a.jointWeights) ++ " ]" ++ attr
                    ]
        , subDToStr mesh.subdivisionScheme
        , "}\n"
        ]


usdFlattened : (String, Mesh) -> String
usdFlattened (id, mesh) =
    usdOver (String.replace ">" "" (String.replace "</" "" id)) Nothing mesh


usdScope : String -> String -> String -> String
usdScope name attr content =
    "\ndef Scope \"" ++ name ++"\" "++ attr ++ " {\n" ++ content ++ "\n}\n"


usdXformBlock : Xform -> String
usdXformBlock f =
    String.concat
        [ case f.orient of
            Just [x,y,z,w] -> "quatf xformOp:orient = (" ++ String.fromFloat x ++ ", " ++ String.fromFloat y ++ ", " ++ String.fromFloat z ++ ", " ++ String.fromFloat w ++ " )\n"
            _ -> ""
        , case f.scale of
            Nothing -> ""
            Just (x,y,z) -> "double3 xformOp:scale = (" ++ String.fromFloat x ++ ", " ++ String.fromFloat y ++ ", " ++ String.fromFloat z ++ " )\n"
        , case f.translate of
            Nothing -> ""
            Just (x,y,z) -> "double3 xformOp:translate = (" ++ String.fromFloat x ++ ", " ++ String.fromFloat y ++ ", " ++ String.fromFloat z ++ " )\n"
        , case (f.translate, f.orient, f.scale) of
            (Just _, Just _, Just _)   -> """uniform token[] xformOpOrder = ["xformOp:translate", "xformOp:orient", "xformOp:scale"]"""
            (Just _, Just _, Nothing)  -> """uniform token[] xformOpOrder = ["xformOp:translate", "xformOp:orient"]"""
            (Just _, Nothing, Just _)  -> """uniform token[] xformOpOrder = ["xformOp:translate", "xformOp:scale"]"""
            (Nothing, Just _, Just _)  -> """uniform token[] xformOpOrder = ["xformOp:orient", "xformOp:scale"]"""
            (Nothing, Nothing, Just _) -> """uniform token[] xformOpOrder = ["xformOp:scale"]"""
            (Nothing, Just _, Nothing) -> """uniform token[] xformOpOrder = ["xformOp:orient"]"""
            (Just _, Nothing, Nothing) -> """uniform token[] xformOpOrder = ["xformOp:translate"]"""
            _ -> ""
        ]


usdXform : ( String, Xform) -> String
usdXform (name, f) =
        ( "def Xform \"" ++ name ++"\" {\n"
        ++ usdXformBlock f
        ++ "\n}"
        )


offsetX : (Float, Float) -> Float -> List (Float, Float, Float) -> List (Float, Float, Float)
offsetX (min, max) offset points =
    List.map
        (\(x,y,z) ->
            if x >= min && x <= max
                then ( x + offset, y, z )
                else (x,y,z)
        )
        points


offsetZ : (Float, Float) -> Float -> List (Float, Float, Float) -> List (Float, Float, Float)
offsetZ (min, max) offset points =
    List.map
        (\(x,y,z) ->
            if z >= min && z <= max
                then ( x, y, z + offset )
                else (x,y,z)
        )
        points



xFormsForToggle : String -> String -> String
xFormsForToggle scene name =
    """def Xform "_Root_Scenes_"""++scene++"""_Behaviors_"""++ name ++"""_TogglePos_ON"
{
double3 xformOp:translate = (0.015, 0, 0)
uniform token[] xformOpOrder = ["xformOp:translate"]
}
def Xform "_Root_Scenes_"""++scene++"""_Behaviors_"""++ name ++"""_TogglePos_OFF"
{
double3 xformOp:translate = (-0.015, 0, 0)
uniform token[] xformOpOrder = ["xformOp:translate"]
}
def Xform "_Root_Scenes_"""++scene++"""_Behaviors_"""++ name ++"""_Switch_ON"
{
double3 xformOp:translate = (0.03, 0, 0)
uniform token[] xformOpOrder = ["xformOp:translate"]
}
def Xform "_Root_Scenes_"""++scene++"""_Behaviors_"""++ name ++"""_Switch_OFF"
{
double3 xformOp:translate = (-0.03, 0, 0)
uniform token[] xformOpOrder = ["xformOp:translate"]
}
"""

usdAction : String -> String -> UsdAction -> String
usdAction prefix name action =
    String.join "\n" <|
      case action of
        Visibility v ->
            [ "def Preliminary_Action \""++ name ++"\" {"
            , "token info:id = \"Visibility\""
            , "rel affectedObjects = ["++ String.join ", " v.affected ++"]"
            , "token type = \"" ++ (if v.show then "show" else "hide") ++ "\""
            , "double duration = " ++ String.fromFloat v.duration
            , "token easeType = \"inout\""
            , "token motionType = \"none\""
            , "double moveDistance = 0"
            , "token style = \"basic\""
            , "}"
            ]
        Group g ->
            (   [ "def Preliminary_Action \""++ name ++"\" {"
                , "token info:id = \"Group\""
                , "rel actions = [ "++ String.join ", " (List.map (\(n,_) -> "<"++prefix++name++"/"++n++">") g.actions) ++" ]"
                , "bool loops = "++ ( if g.loop then "1" else "0" )
                , "int performCount = " ++ String.fromInt g.performCount
                , "token type = \"" ++ ( if g.parallel then "parallel" else "serial" ) ++ "\""
                ]
            ++  ( List.map
                    (\(n,a) -> usdAction (prefix++name++"/") n a)
                    g.actions
                )
            ++ [ "}" ]
            )
        Transform t ->
            [ "def Preliminary_Action \""++ name ++"\" {"
            , "token info:id = \"Transform\""
            , "rel affectedObjects = "++ t.affected
            , "double duration = "++ String.fromFloat t.duration
            , "token easeType = \"none\""
            , "token type = \"" ++ (if t.absolute then "absolute" else "relative" ) ++ "\""
            , "rel xformTarget = "++ t.xformTarget
            , "}"
            ]
        LookAtCamera t ->
            [ "def Preliminary_Action \""++ name ++"\" {"
            , "token info:id = \"LookAtCamera\""
            , "rel affectedObjects = [ "++ (String.join ", " t.affected) ++" ]"
            , "double duration = "++ String.fromFloat t.duration
            , "double3 front = (0, 0, 1)"
            , "double3 upVector = (0, 0, 0)"    -- TODO
            , "}"
            ]
        StartAnimation t ->
            [ "def Preliminary_Action \""++ name ++"\" {"
            , "token info:id = \"StartAnimation\""
            , "rel affectedObjects = [ "++ (String.join ", " t.affected) ++" ]"
            , "double animationSpeed = 1"
            , "double duration = 0"
            , "token multiplePerformOperation = \"ignore\""
            , "bool reversed = 0"
            , "bool reverses = 0"
            , "double start = 0"
            , "}"
            ]


usdTrigger : String -> UsdTrigger -> String
usdTrigger name trigger =
    String.join "\n"
        [ "def Preliminary_Trigger \"" ++ name ++ "\" {\n"
        , case trigger of
            TapGesture o ->
                "token info:id = \"TapGesture\"\nrel affectedObjects = " ++ o
            SceneTransition x ->
                "token info:id = \"SceneTransition\"\ntoken type = \""++ x ++"\""
        , "}"
        ]


usdBehavior : String -> UsdBehavior -> String
usdBehavior scene b =
    let
        prefix = "/Root/Scenes/"++scene++"/Behaviors/"++ b.id ++"/"
        indexedTriggers =
            List.indexedMap
                (\i t ->
                    ( "Trigger_"++ String.fromInt i, t )
                )
                b.triggers
    in
    String.join "\n"
      ( [ "def Preliminary_Behavior \"" ++ b.id ++"\" {"
        , "uniform bool exclusive = "++ (if b.exclusive then "1" else "0")
        , "rel actions = [<" ++ prefix ++ "ActionRoot>]"
        , "rel triggers = ["
        , String.join ", " <| List.map 
            (\(n,_) -> "<" ++ prefix ++ n ++ ">")
            indexedTriggers
        , "]"
        ]
        ++( List.map
            (\(n,t) -> usdTrigger n t)
            indexedTriggers
        )
        ++[ usdAction prefix "ActionRoot" b.actionRoot 
          , "}"
          ]
      )

justHide : List String -> UsdAction
justHide a =
    Visibility { show= False, duration= 0, affected= a }


justShow : List String -> UsdAction
justShow a =
    Visibility { show= True, duration= 0, affected= a }


behaviorsForMulti : String -> MultiItem -> (List (String, UsdAction), List UsdBehavior)
behaviorsForMulti scene item =
    let
        name= item.id
        panelPath= "</Root/Scenes/"++scene++"/Children/ConfigUI/Children/Hideable/Children/Multi_"++name++"/Children/Hideable/Children/Panel/Children/"
        matrixPath= "</Root/Scenes/"++scene++"/Children/ConfigUI/Children/Hideable/Children/Multi_"++name++"/Children/Hideable/Children/Matrix/Children/"
        urlPath= "</Root/Scenes/"++scene++"/Children/Order/Children/URL/Children/Item_"++name++"/Children/"
    in
    ( [ ( "HideObjects_"++name
        , Group { parallel= False, loop= False, performCount= 1, actions= 
            List.filterMap
                (\p ->
                    if item.startProduct == p.product
                        then Nothing
                        else Just
                            ( "HideObject_"++p.product
                            , justHide [ "</Root/Scenes/"++scene++"/Children/Object_"++name++"_"++p.product++">" ]
                            )
                )
                item.products
          }
        )
      , ( "HideCodes_"++name
        , Group { parallel= False, loop= False, performCount= 1, actions= 
            List.filterMap
                (\p ->
                    if item.startProduct == p.product
                        then Nothing
                        else Just
                            ( "HideCode_"++p.product
                            , justHide [ urlPath ++ "Product_"++ p.product ++">" ]
                            )
                )
                item.products
          }
        )
      , ( "HidePanels_"++name
        , Group { parallel= False, loop= False, performCount= 1, actions= 
            List.filterMap
                (\p ->
                    if item.startProduct == p.product
                        then Nothing
                        else Just
                            ( "HidePanel_"++p.product
                            , justHide [ panelPath++"Product_"++ p.product ++">" ]
                            )
                )
                item.products
          }
        )
      , ( "HideOtherButtons_"++name
        , Group { parallel= False, loop= False, performCount= 1, actions= 
            List.filterMap
                (\p ->
                    if item.startProduct == p.product
                        then Just
                            ( "HideButton_"++p.product
                            , justHide [ matrixPath ++ "Pick_"++ p.product ++">" ]
                            )
                        else Just
                            ( "HideIndicator_"++p.product
                            , justHide [ matrixPath ++ "Picked_"++ p.product ++">" ]
                            )
                )
                item.products
          }
        )
      --, ( "ShowButton_"++name
      --  , Group { parallel= False, loop= False, performCount= 1, actions= 
      --      List.filterMap
      --          (\p ->
      --              if item.startProduct == p.product
      --                  then Just
      --                      ( "ShowIndicator_"++p.product
      --                      , justShow [ matrixPath ++ "PickedProduct_"++ p.product ++">" ]
      --                      )
      --                  else Nothing
      --          )
      --          item.products
      --    }
      --  )
      ]
    , List.map
        (\p ->
            let
                hideShowOthers=
                    Group { parallel= True, loop= False, performCount= 1, actions= List.concat <|
                        List.filterMap
                            (\otherProduct ->
                                if otherProduct.product == p.product
                                    then Nothing
                                    else Just
                                        [ ( "HideObject_"++otherProduct.product
                                          , justHide [ "</Root/Scenes/"++scene++"/Children/Object_"++name++"_"++otherProduct.product++">" ]
                                          )
                                        , ( "HideCode_"++otherProduct.product
                                          , justHide [ urlPath ++ "Product_"++ otherProduct.product ++">" ]
                                          )
                                        , ( "HidePanel_"++otherProduct.product
                                          , justHide [ panelPath ++ "Product_"++ otherProduct.product ++">" ]
                                          )
                                        , ( "HideIndicator_"++otherProduct.product
                                          , justHide [ matrixPath ++ "Picked_"++ otherProduct.product ++">" ]
                                          )
                                        , ( "ShowPick_"++otherProduct.product
                                          , justShow [ matrixPath ++ "Pick_"++ otherProduct.product ++">" ]
                                          )
                                        ]
                            )
                            item.products
                    }
                hideShowThis= 
                    Group { parallel= True, loop= False, performCount= 1, actions=
                        [ ( "HidePick_"++p.product
                          , justHide [ matrixPath ++ "Pick_"++ p.product ++">" ]
                          )
                        , ( "ShowObject_"++p.product
                          , justShow [ "</Root/Scenes/"++scene++"/Children/Object_"++name++"_"++p.product++">" ]
                          )
                        , ( "ShowPanel_"++p.product
                          , justShow [ panelPath ++ "Product_"++ p.product ++">" ]
                          )
                        , ( "ShowIndicator"++p.product
                          , justShow [ matrixPath ++ "Picked_"++ p.product ++">" ]
                          )
                        , ( "ShowCode"++p.product
                          , justShow [ urlPath ++ "Product_"++ p.product ++">" ]
                          )
                        ]
                    }
            in
                    { id= "PickProduct_" ++name++ "_" ++ p.product
                    , exclusive= True
                    , triggers=
                        [ -- TapGesture ( panelPath++"Product_"++p.product++"/Children/Pick_"++p.form++">")
                          TapGesture ( matrixPath ++ "Pick_"++ p.product ++">")
                        ]
                    , actionRoot=
                        Group { parallel= False, loop= False, performCount= 1, actions=
                            [ ( "HideShowOthers", hideShowOthers )
                            , ( "HideShowThis", hideShowThis )
                            ]
                        }
                    }
        )
        item.products
    {-
    , List.filterMap
        (\p ->
            let
                hideOthers=
                    Group { parallel= False, loop= False, performCount= 1, actions=
                        List.filterMap
                            (\otherProduct ->
                                if otherProduct.product == p.product
                                    then Nothing
                                    else Just
                                        ( "Hide_"++otherProduct.product
                                        , justHide
                                            [ "</Root/Scenes/"++scene++"/Children/ConfigUI/Children/Hideable/Children/Multi_"++name++"/Children/Hideable/Children/Product_"++ otherProduct.product ++">"
                                            , "</Root/Scenes/"++scene++"/Children/Order/Children/URL/Children/Item_"++name++"/Children/Product_"++ otherProduct.product ++">"
                                            , "</Root/Scenes/"++scene++"/Children/Object_"++name++"_"++otherProduct.product++">"
                                            ]
                                        )
                            )
                            item.products
                    }
                showThis=
                    justShow
                        [ "</Root/Scenes/"++scene++"/Children/ConfigUI/Children/Hideable/Children/Multi_"++name++"/Children/Hideable/Children/Product_"++ p.product ++">"
                        , "</Root/Scenes/"++scene++"/Children/Order/Children/URL/Children/Item_"++name++"/Children/Product_"++ p.product ++">"
                        , "</Root/Scenes/"++scene++"/Children/Object_"++name++"_"++p.product++">"
                        ]

                similarInForm = List.filter (\o -> o.product /= p.product && o.form == p.form {--&& o.look /= p.look-}) item.products
                similarInLook = List.filter (\o -> o.product /= p.product && o.look == p.look {--&& o.form /= p.form-}) item.products
            in
            if (List.length similarInForm) + (List.length similarInLook) > 0
                then Just
                    { id= "PickProduct_" ++name++ "_" ++ p.product
                    , exclusive= True
                    , triggers= List.concat
                        [ List.map
                            (\otherProduct ->
                                TapGesture ( panelPath++"Product_"++otherProduct.product++"/Children/Pick_"++otherProduct.form++">")
                            )
                            similarInForm
                        , List.map
                            (\otherProduct ->
                                TapGesture ( panelPath++"Product_"++otherProduct.product++"/Children/Pick_"++otherProduct.look++">")
                            )
                            similarInLook
                        ]
                    , actionRoot=
                        Group { parallel= False, loop= False, performCount= 1, actions=
                            [ ( "HideOthers" , hideOthers )
                            , ( "Show", showThis )
                            ]
                        }
                    }
                else Nothing
        )
        item.products
    -}
    )


behaviorsForSelectToggle : String -> SelectToggleItem -> (List (String, UsdAction), List UsdBehavior)
behaviorsForSelectToggle scene item =
    let
        name        = item.id
        objPath     = "</Root/Scenes/"++scene++"/Children/Object_"++name++"/Children/"
        editBtnPath = uiPath++"EditButton/Children/"
        uiPath      = "</Root/Scenes/"++scene++"/Children/ConfigUI/Children/Hideable/Children/UI_"++name++"/Children/"
        urlPath     = "</Root/Scenes/"++scene++"/Children/Order/Children/URL/Children/Item_"++name++"/Children/"
        panelPath   = "</Root/Scenes/"++scene++"/Children/ConfigUI/Children/Hideable/Children/Select_"++name++"/Children/Hideable/Children/"
    in
    ( [ --( "TogglePos_"++name
        --, Transform 
        --    { absolute= False
        --    , duration= 0
        --    , xformTarget= 
        --        if item.startActive
        --            then "</_Root_Scenes_"++scene++"_Behaviors_"++ name ++"_TogglePos_ON>"
        --            else "</_Root_Scenes_"++scene++"_Behaviors_"++ name ++"_TogglePos_OFF>"
        --    , affected= uiPath++"ToggleGroup>"
        --    }
        --)
        --, 
        if item.startActive
            then
                ( "Toggle_"++name
                , justHide
                    [ uiPath++"ToggleGroup/Children/ToggleOFF>"
                    , uiPath++"OFF>"
                    , uiPath++"StateOFF>"
                    ]
                )
            else
                ( "Hide_"++name
                , Group { parallel= True, loop= False, performCount= 1, actions=
                    [ ( "Toggle"
                      , justHide
                            [ uiPath++"ToggleGroup/Children/ToggleON>"
                            , uiPath++"ON>"
                            , uiPath++"StateON>"
                            ]
                      )
                    , ( "Edit"
                      , justHide 
                            [ uiPath++"EditButton>"
                            , uiPath++"SelectLabels>"
                            , "</Root/Scenes/"++scene++"/Children/Object_" ++ name ++ ">"
                            , "</Root/Scenes/"++scene++"/Children/ConfigUI/Children/Hideable/Children/Select_" ++ name ++ "/Children/Hideable>" 
                            ]
                        )
                    ]
                    }
                )
      , if item.startActive
            then
                ( "CodeActive_"++name
                , justHide [ urlPath++"WithOut>" ]
                )
            else
                ( "CodeInactive"++name
                , justHide 
                    [ urlPath++"Look_"
                      ++ Tuple.first item.startGrpLook
                      ++ "_"
                      ++ Tuple.second item.startGrpLook
                    ]
                )
      , ( "EditButtonStart_"++name
        , justHide <|
            if item.startOpen
              then
                [ editBtnPath++"Open>"
                , editBtnPath++"OpenBG>"
                ]
              else
                [ editBtnPath++"Close>"
                , editBtnPath++"CloseBG>"
                , "</Root/Scenes/"++scene++"/Children/ConfigUI/Children/Hideable/Children/Select_" ++name++ ">"
                ]
        )
      , ( "HideLooksAndLabels_"++name -- Hide most objects, codes, indicators and labels
        , Group { parallel= True, loop= False, performCount= 1, actions= 
            List.map 
            (\group -> 
                ( "Group_"++group.id
                , Group { parallel= True, loop= False, performCount= 1, actions=
                    List.filterMap
                        (\look -> 
                            if (group.id, look.id) /= item.startGrpLook
                                then
                                    let
                                        suffix= group.id ++ "_" ++ look.id ++ ">"
                                    in
                                    Just 
                                        ( "Look_"++look.id
                                        , justHide 
                                            [ (objPath++"Instance_" ++ suffix)
                                            , (uiPath++"SelectLabels/Children/Look_" ++ suffix)
                                            , (urlPath++"Look_" ++ suffix)
                                            ]
                                        )
                                else
                                    Nothing
                        )
                        group.looks
                    }
                )
            )
            item.lookGroups
          }
        )
      , ( "HideGroupsAndLabels_"++name -- Hide most UI groups and group labels
        , Group { parallel= False, loop= False, performCount= 1, actions=
            List.map
                (\group ->
                    ( "Group_"++group.id
                    , Group { parallel= True, loop= False, performCount= 1, actions=
                        [   ("Hide_"++group.id
                            , justHide <|
                                if group.id /= (Tuple.first item.startGrpLook)
                                    then [ (panelPath++"Panel_"++group.id++">"), (panelPath++"Overview/Children/Active_"++group.id++">") ]
                                    else [ (panelPath++"Overview/Children/Pick_"++group.id++">") ]
                            )
                        ]
                      }
                    )
                )
                item.lookGroups
            }
        )
      ]
    , [ { id= "SwitchON_"++name
        , exclusive= True
        , triggers=
            [ TapGesture (uiPath++"ToggleGroup/Children/ToggleOFF>")
            , TapGesture (uiPath++"OFF>")
            , TapGesture (uiPath++"StateOFF>")
            --, TapGesture (uiPath++"ONText>")
            ]
        , actionRoot=
          Group { parallel= False, loop= False, performCount= 1, actions= 
            [ ( "MoveToggleON"
              , Transform 
                    { absolute= False
                    , duration= 0.333
                    , xformTarget= "</_Root_Scenes_"++scene++"_Behaviors_"++ name ++"_Switch_ON>"
                    , affected= uiPath++"ToggleGroup>"
                    }
              )
            , ( "ShowToggle"
              , Visibility { show= True, duration= 0, affected=
                    [ uiPath++"ToggleGroup/Children/ToggleON>"
                    , uiPath++"ON>"
                    , uiPath++"StateON>"
                    ]
                  }
              )
            , ( "ShowEdit"
              , Visibility { show= True, duration= 0, affected=
                    [ uiPath++"EditButton>"
                    , uiPath++"SelectLabels>"
                    , "</Root/Scenes/"++scene++"/Children/Object_" ++ name ++">"
                    , "</Root/Scenes/"++scene++"/Children/ConfigUI/Children/Hideable/Children/Select_" ++ name ++ "/Children/Hideable>" 
                    ]
                  }
              )
            , ( "Hide"
              , Visibility { show= False, duration= 0, affected=
                    [ uiPath++"ToggleGroup/Children/ToggleOFF>"
                    , uiPath++"OFF>"
                    , uiPath++"StateOFF>"
                    ]
                  }
              )
            ] }
        }
      , { id= "SwitchOFF_"++name
        , exclusive= True
        , triggers=
            [ TapGesture (uiPath++"ToggleGroup/Children/ToggleON>")
            , TapGesture (uiPath++"ON>")
            , TapGesture (uiPath++"StateON>")
            --, TapGesture (uiPath++"OFFText>")
            ]
        , actionRoot=
          Group { parallel= False, loop= False, performCount= 1, actions= 
            [ ( "MoveToggleOFF"
              , Transform 
                    { absolute= False
                    , duration= 0.333
                    , xformTarget= "</_Root_Scenes_"++scene++"_Behaviors_"++ name ++"_Switch_OFF>"
                    , affected= uiPath++"ToggleGroup>"
                    }
              )
            , ( "Show"
              , Visibility { show= True, duration= 0, affected=
                    [ uiPath++"ToggleGroup/Children/ToggleOFF>"
                    , uiPath++"OFF>"
                    , uiPath++"StateOFF>"
                    ] }
              )
            , ( "HideToggle"
              , Visibility { show= False, duration= 0, affected=
                    [ uiPath++"ToggleGroup/Children/ToggleON>"
                    , uiPath++"ON>"
                    , uiPath++"StateON>"
                    ] }
              )
            , ( "HideEdit"
              , Visibility { show= False, duration= 0, affected=
                    [ uiPath++"EditButton>"
                    , uiPath++"SelectLabels>"
                    , "</Root/Scenes/"++scene++"/Children/Object_" ++ name ++">"
                    , "</Root/Scenes/"++scene++"/Children/ConfigUI/Children/Hideable/Children/Select_" ++ name ++ "/Children/Hideable>" 
                    ] }
              )
            ] }
        }
      , { id= "ShowPanels_"++name
        , exclusive= True
        , triggers=
            [ TapGesture (editBtnPath++"Open>") 
            , TapGesture (editBtnPath++"OpenBG>")
            ]
        , actionRoot= Group { parallel= False, loop= False, performCount= 1, actions= 
            [ ( "Hide"
              , justHide [ (editBtnPath++"Open>"), (editBtnPath++"OpenBG>") ] )
            , ( "FadeIn"
              , Visibility 
                { show= True, duration= 0.333, affected=
                    [ uiPath++"EditPanels>"
                    , "</Root/Scenes/"++scene++"/Children/ConfigUI/Children/Hideable/Children/Select_" ++name++ ">"
                    ]
                }
              )
            , ( "Show"
              , justShow [ (editBtnPath++"Close>"), (editBtnPath++"CloseBG>") ]
              )
            ]
          }
        }
      , { id= "HidePanels_"++name
        , exclusive= True
        , triggers=
            [ TapGesture (editBtnPath++"Close>")
            , TapGesture (editBtnPath++"CloseBG>")
            , TapGesture (uiPath++"ToggleGroup/Children/ToggleON>")
            , TapGesture (uiPath++"ON>")
            , TapGesture (uiPath++"StateON>")
            --, TapGesture (uiPath++"OFFText>")
            ]
        , actionRoot= Group { parallel= True, loop= False, performCount= 1, actions= 
            [ ( "Hide"
              , justHide [ editBtnPath++"Close>", editBtnPath++"CloseBG>" ]
              )
            , ( "FadeOut"
              , Visibility { show= False, duration= 0.333, affected=
                    [ uiPath++"EditPanels>"
                    , "</Root/Scenes/"++scene++"/Children/ConfigUI/Children/Hideable/Children/Select_" ++name++ ">"
                    ]
                }
              )
            , ( "Show", justShow [ editBtnPath++"Open>", editBtnPath++"OpenBG>" ] )
            ]
          }
        }
      ]
      ++ ( List.concatMap  -- Look buttons
            (\outerGroup ->
                List.map
                    (\look ->
                        { id= "Select_"++name ++ "_"++ outerGroup.id ++"_"++look.id 
                        , exclusive= True
                        , triggers=
                            [ TapGesture (panelPath ++ "Panel_" ++ outerGroup.id ++ "/Children/Pick_"++ look.id ++">") ]
                        , actionRoot= 
                            Group { parallel= False, loop= False, performCount= 1, actions= 
                                [ ( "HideOthers"
                                  , Group { parallel= True, loop= False, performCount= 1, actions= 
                                        List.map 
                                            (\group -> 
                                                ( "Group_"++group.id
                                                , Group { parallel= False, loop= False, performCount= 1, actions=
                                                    List.filterMap
                                                        (\innerLook -> 
                                                            if (group.id, innerLook.id) /= (outerGroup.id, look.id)
                                                                then
                                                                    let
                                                                        suffix = group.id ++ "_" ++ innerLook.id ++ ">"
                                                                    in
                                                                    Just
                                                                        ( "Hide_"++group.id++"_"++innerLook.id
                                                                        , Group { parallel= True, loop= False, performCount= 1, actions=
                                                                            [ ( "Look_"++group.id++"_"++innerLook.id
                                                                              , justHide 
                                                                                  [ (objPath++"Instance_" ++ suffix )
                                                                                  , (uiPath++"SelectLabels/Children/Look_" ++ suffix )
                                                                                  , (urlPath++"Look_"++ suffix)
                                                                                  ]
                                                                              )
                                                                            ]
                                                                          }
                                                                        )
                                                                else
                                                                    Nothing
                                                        )
                                                        group.looks
                                                    }
                                                )
                                            )
                                            item.lookGroups
                                    }
                                  )
                                , ( "ShowLook"
                                  , justShow
                                          [ (objPath ++ "Instance_" ++ outerGroup.id ++ "_" ++ look.id ++ ">")
                                          , (uiPath++"SelectLabels/Children/Look_" ++ outerGroup.id ++ "_" ++ look.id ++ ">")
                                          --, (panelPath ++ outerGroup.id ++ "/Children/Indicate_"++ look.id ++">") -- TODO create
                                          , (urlPath++"Look_" ++ outerGroup.id ++ "_" ++ look.id ++ ">")
                                          ]
                                  )
                                ]
                            }
                        }
                    )
                    outerGroup.looks
            )
            item.lookGroups
      )
      ++ ( List.map  -- LookGroup buttons
            (\outerGroup ->
                { id= "Select_"++name ++ "_"++ outerGroup.id
                , exclusive= True
                , triggers=
                    [ TapGesture (panelPath++"Overview/Children/Pick_" ++ outerGroup.id ++">")
                    , TapGesture (panelPath++"Overview/Children/Label_" ++ outerGroup.id ++">")
                    ]
                , actionRoot=
                    Group { parallel= False, loop= False, performCount= 1, actions= 
                        [ ( "ShowGroup"
                        , justShow
                                [ panelPath++"Panel_" ++outerGroup.id ++">"
                                , panelPath++"Overview/Children/Active_"++outerGroup.id ++">"
                                ]
                        )
                        , ( "ShowButtons"
                        , Group { parallel= True, loop= False, performCount= 1, actions= 
                                List.filterMap
                                    (\innerGroup ->
                                        if innerGroup.id /= outerGroup.id
                                            then Just 
                                                ( "Show_"++innerGroup.id
                                                , justShow [panelPath++"Overview/Children/Pick_" ++ innerGroup.id ++">"]
                                                )
                                            else Nothing
                                    )
                                    item.lookGroups
                            }
                        )
                        , ( "HideOthers"
                          , Group { parallel= True, loop= False, performCount= 1, actions=
                                List.map
                                    (\innerGroup ->
                                        ( "Hide_"++outerGroup.id ++"_"++innerGroup.id
                                        , justHide <|
                                            if innerGroup.id /= outerGroup.id
                                                then
                                                    [ (panelPath++"Panel_" ++ innerGroup.id ++">")
                                                    , (panelPath++"Overview/Children/Active_" ++ innerGroup.id ++">")
                                                    ]
                                                else
                                                    [ (panelPath++"Overview/Children/Pick_" ++ innerGroup.id ++">") ]
                                        )
                                    )
                                    item.lookGroups
                              }
                          )
                        ]
                    }
                }
            )
            item.lookGroups
      )
    )


behaviorsForCode : String -> (List (String, UsdAction), List UsdBehavior)
behaviorsForCode scene =
    let
        orderPath = "</Root/Scenes/"++scene++"/Children/Order/Children/"
        explainPath = "</Root/Scenes/"++scene++"/Children/Explain/Children/"
        explain = "</Root/Scenes/"++scene++"/Children/Explain>"
    in
    ( [ ( "CodeUI"
        , justHide
            [ orderPath++"URL>"
            , orderPath++"Hint>"
            , explainPath++"CloseSummaryBtn>"
            , explainPath++"CloseSummaryText>"
            , explain
            ]
        )
      ]
    , [ { id= "ShowOrderPanel"
        , exclusive= True
        , triggers=
            [ TapGesture (orderPath++"OpenSummaryBtn>")
            , TapGesture (orderPath++"OpenSummaryText>")
            ]
        , actionRoot=
          Group { parallel= True, loop= False, performCount= 1, actions= 
            [ ( "AnimateIn"
              , Transform 
                    { absolute= True
                    , duration= 0.8
                    , xformTarget= "</_Root_Scenes_"++ scene ++"_Behaviors_Order_Show>"
                    , affected= "</Root/Scenes/"++ scene ++"/Children/Order>"
                    }
              )
            , ( "GrowPanel"
              , Transform
                    { absolute= False
                    , duration= 0.333
                    , xformTarget= "</_Root_Scenes_"++ scene ++"_Behaviors_Order_Grow>"
                    , affected= "</Root/Scenes/"++ scene ++"/Children/Order/Children/Panel>"
                    }
              )
            , ( "FadeOutUI"
              , Visibility
                { show= False, duration= 0.333, affected=
                    [ "</Root/Scenes/"++scene++"/Children/ConfigUI/Children/Hideable>"
                    , orderPath++"OpenSummaryBtn>"
                    , orderPath++"OpenSummaryText>"
                    ]
                }
              )
            , ( "FadeInOrderUI"
              , Visibility
                { show= True, duration= 0.333, affected=
                    [ orderPath++"URL>"
                    , orderPath++"Hint>"
                    , explainPath++"CloseSummaryBtn>"
                    , explainPath++"CloseSummaryText>"
                    , explain
                    ]
                }
              )
            ]
          }
        }
      , { id= "HideOrderPanel"
        , exclusive= True
        , triggers=
            [ TapGesture (explainPath++"CloseSummaryBtn>")
            , TapGesture (explainPath++"CloseSummaryText>")
            ]
        , actionRoot=
          Group { parallel= True, loop= False, performCount= 1, actions= 
            [ ( "AnimateOut"
              , Transform 
                    { absolute= True
                    , duration= 0.8
                    , xformTarget= "</_Root_Scenes_"++ scene ++"_Behaviors_Order_Hide>"
                    , affected= "</Root/Scenes/"++ scene ++"/Children/Order>"
                    }
              )
            , ( "ShrinkPanel"
              , Transform
                    { absolute= False
                    , duration= 0.333
                    , xformTarget= "</_Root_Scenes_"++ scene ++"_Behaviors_Order_Shrink>"
                    , affected= "</Root/Scenes/"++ scene ++"/Children/Order/Children/Panel>"
                    }
              )
            , ( "FadeInUI"
              , Visibility
                { show= True, duration= 0.333, affected=
                    [ "</Root/Scenes/"++scene++"/Children/ConfigUI/Children/Hideable>"
                    , orderPath++"OpenSummaryBtn>"
                    , orderPath++"OpenSummaryText>"
                    ]
                }
              )
            , ( "FadeOutOrderUI"
              , Visibility
                { show= False, duration= 0.333, affected=
                    [ orderPath++"URL>"
                    , orderPath++"Hint>"
                    , explainPath++"CloseSummaryBtn>"
                    , explainPath++"CloseSummaryText>"
                    , explain
                    ]
                }
              )
            ]
          }
        }
      ]
    )


behaviorsForSelect : String -> SelectItem -> (List (String, UsdAction), List UsdBehavior)
behaviorsForSelect scene item =
    let
        name = item.id
        objPath     = "</Root/Scenes/"++scene++"/Children/Object_"++name++"/Children/"
        editBtnPath = uiPath++"EditButton/Children/"
        uiPath      = "</Root/Scenes/"++scene++"/Children/ConfigUI/Children/Hideable/Children/UI_"++name++"/Children/"
        urlPath     = "</Root/Scenes/"++scene++"/Children/Order/Children/URL/Children/Item_"++name++"/Children/"
        panelPath   = "</Root/Scenes/"++scene++"/Children/ConfigUI/Children/Hideable/Children/Select_"++name++"/Children/Hideable/Children/"
    in
    ( [ ( "EditButtonStart_"++name
        , if item.startOpen
            then justHide
                [ editBtnPath++"Open>"
                , editBtnPath++"OpenBG>"
                ]
            else justHide
                [ editBtnPath++"Close>"
                , editBtnPath++"CloseBG>"
                , "</Root/Scenes/"++scene++"/Children/ConfigUI/Children/Hideable/Children/Select_" ++name++ ">"
                ]
        )
      , ( "HideLooksAndLabels_"++name -- Hide most objects, indicators and labels
        , Group { parallel= True, loop= False, performCount= 1, actions= 
            List.map 
            (\group -> 
                ( "Group_"++group.id
                , Group { parallel= True, loop= False, performCount= 1, actions=
                    List.filterMap
                        (\look -> 
                            if (group.id, look.id) /= item.startGrpLook
                                then
                                    let
                                        suffix= group.id ++ "_" ++ look.id ++ ">"
                                    in
                                    Just 
                                        ( "Look_"++look.id
                                        , justHide 
                                            [ (objPath++"Instance_" ++ suffix)
                                            , (uiPath++"SelectLabels/Children/Look_" ++ suffix)
                                            , (urlPath++"Look_" ++ suffix)
                                            ]
                                        )
                                else
                                    Nothing
                        )
                        group.looks
                    }
                )
            )
            item.lookGroups
          }
        )
      , ( "HideGroupsAndLabels_"++name -- Hide most UI groups and group labels
        , Group { parallel= True, loop= False, performCount= 1, actions=
            List.map
                (\group ->
                    ( "Group_"++group.id
                    , justHide <|
                        if group.id /= (Tuple.first item.startGrpLook)
                            then [ (panelPath++"Panel_"++group.id++">"), (panelPath++"Overview/Children/Active_"++group.id++">") ]
                            else [ (panelPath++"Overview/Children/Pick_"++group.id++">") ]
                    )
                )
                item.lookGroups
            }
        )
      ]
    , [ { id= "ShowPanels_"++name
        , exclusive= True
        , triggers=
            [ TapGesture (editBtnPath++"Open>") 
            , TapGesture (editBtnPath++"OpenBG>")
            ]
        , actionRoot= Group { parallel= False, loop= False, performCount= 1, actions= 
            [ ( "Hide"
              , justHide [ (editBtnPath++"Open>"), (editBtnPath++"OpenBG>") ] )
            , ( "FadeIn"
              , Visibility 
                { show= True, duration= 0.333, affected=
                    [ uiPath++"EditPanels>"
                    , "</Root/Scenes/"++scene++"/Children/ConfigUI/Children/Hideable/Children/Select_" ++name++ ">"
                    ]
                }
              )
            , ( "Show"
              , justShow [ (editBtnPath++"Close>"), (editBtnPath++"CloseBG>") ]
              )
            ]
          }
        }
      , { id= "HidePanels_"++name
        , exclusive= False
        , triggers=
            [ TapGesture (editBtnPath++"Close>")
            , TapGesture (editBtnPath++"CloseBG>")
            ]
        , actionRoot= Group { parallel= False, loop= False, performCount= 1, actions= 
            [ ( "Hide"
              , justHide [ editBtnPath++"Close>", editBtnPath++"CloseBG>" ]
              )
            , ( "FadeOut"
              , Visibility { show= False, duration= 0.333, affected=
                    [ uiPath++"EditPanels>"
                    , "</Root/Scenes/"++scene++"/Children/ConfigUI/Children/Hideable/Children/Select_" ++name++ ">"
                    ]
                }
              )
            , ( "Show", justShow [ editBtnPath++"Open>", editBtnPath++"OpenBG>" ] )
            ]
          }
        }
      ]
      ++ ( List.concatMap  -- Look buttons
            (\outerGroup ->
                List.map
                    (\look ->
                        { id= "Select_"++name ++ "_"++ outerGroup.id ++"_"++look.id 
                        , exclusive= True
                        , triggers=
                            [ TapGesture (panelPath ++ "Panel_" ++ outerGroup.id ++ "/Children/Pick_"++ look.id ++">") ]
                        , actionRoot= 
                            Group { parallel= True, loop= False, performCount= 1, actions= 
                                [ ( "ShowLook"
                                , justShow
                                    [ (objPath ++ "Instance_" ++ outerGroup.id ++ "_" ++ look.id ++ ">")
                                    , (uiPath++"SelectLabels/Children/Look_" ++ outerGroup.id ++ "_" ++ look.id ++ ">")
                                    --, (panelPath ++ outerGroup.id ++ "/Children/Indicate_"++ look.id ++">") -- TODO create
                                    , (urlPath++"Look_" ++ outerGroup.id ++ "_" ++ look.id ++ ">")
                                    ]
                                )
                                , ( "HideOthers"
                                  , Group { parallel= False, loop= False, performCount= 1, actions=
                                        List.concatMap
                                            (\group ->
                                                List.filterMap
                                                    (\innerLook ->
                                                        if (group.id, innerLook.id) /= (outerGroup.id, look.id)
                                                            then
                                                                let
                                                                    suffix = group.id ++ "_" ++ innerLook.id ++ ">"
                                                                in
                                                                Just 
                                                                    ( "Look_"++group.id ++"_"++innerLook.id
                                                                    , justHide 
                                                                        [ (objPath++"Instance_" ++ suffix )
                                                                        , (uiPath++"SelectLabels/Children/Look_" ++ suffix )
                                                                        , (urlPath++"Look_"++ suffix)
                                                                        ]
                                                                    )
                                                            else
                                                                Nothing
                                                    )
                                                    group.looks
                                            )
                                            item.lookGroups
                                      }
                                  )
                                ]
                          }
                        }
                    )
                    outerGroup.looks
            )
            item.lookGroups
        )
        ++ ( List.map  -- LookGroup buttons
            (\outerGroup ->
                { id= "Select_"++name ++ "_"++ outerGroup.id
                , exclusive= True
                , triggers=
                    [ TapGesture (panelPath++"Overview/Children/Pick_" ++ outerGroup.id ++">")
                    , TapGesture (panelPath++"Overview/Children/Label_" ++ outerGroup.id ++">")
                    ]
                , actionRoot= Group { parallel= True, loop= False, performCount= 1, actions= 
                    [ ( "ShowGroup"
                      , justShow
                            [ panelPath++"Panel_" ++outerGroup.id ++">"
                            , panelPath++"Overview/Children/Active_"++outerGroup.id ++">"
                            ]
                      )
                    , ( "ShowButtons"
                      , Group { parallel= True, loop= False, performCount= 1, actions= 
                            List.filterMap
                                (\innerGroup ->
                                    if innerGroup.id /= outerGroup.id
                                        then Just 
                                            ( "Show_"++innerGroup.id
                                            , justShow [panelPath++"Overview/Children/Pick_" ++ innerGroup.id ++">"]
                                            )
                                        else Nothing
                                )
                                item.lookGroups
                         }
                      )
                    , ( "HideOthers"
                      , Group { parallel= True, loop= False, performCount= 1, actions=
                            List.map
                                (\innerGroup ->
                                    ( "Hide_"++outerGroup.id++"_"++innerGroup.id
                                    , justHide <|
                                        if innerGroup.id /= outerGroup.id
                                            then
                                                [ (panelPath++"Panel_" ++ innerGroup.id ++">")
                                                , (panelPath++"Overview/Children/Active_" ++ innerGroup.id ++">")
                                                ]
                                            else
                                                [ (panelPath++"Overview/Children/Pick_" ++ innerGroup.id ++">") ]
                                    )
                                )
                                item.lookGroups
                          }
                      )
                    ]
                    }
                }
            )
            item.lookGroups
        )
    )


behaviorsForToggle : String -> ToggleItem -> (List (String, UsdAction), List UsdBehavior)
behaviorsForToggle scene item =
    let
        name = item.id
        uiPath  = "</Root/Scenes/"++scene++"/Children/ConfigUI/Children/Hideable/Children/UI_"++name++"/Children/"
        urlPath = "</Root/Scenes/"++scene++"/Children/Order/Children/URL/Children/Item_"++name++"/Children/"
    in
    ( [ --( "TogglePos_"++name
        --, Transform 
        --    { absolute= False
        --    , duration= 0
        --    , xformTarget= 
        --        if item.startActive
        --            then "</_Root_Scenes_"++scene++"_Behaviors_"++ name ++"_TogglePos_ON>"
        --            else "</_Root_Scenes_"++scene++"_Behaviors_"++ name ++"_TogglePos_OFF>"
        --    , affected= uiPath++"ToggleGroup>"
        --    }
        --)
        --, 
        ( "Hide_"++name
        , justHide
            ( if item.startActive
                then 
                    [ uiPath++"ToggleGroup/Children/ToggleOFF>"
                    , uiPath++"OFF>"
                    , uiPath++"StateOFF>"
                    ]
                else
                    [ uiPath++"ToggleGroup/Children/ToggleON>"
                    , uiPath++"ON>"
                    , uiPath++"StateON>"
                    , "</Root/Scenes/"++scene++"/Children/Object_" ++ name ++">"
                    ]
            )
        )
      , if item.startActive
            then
                ( "CodeActive_"++name
                , justHide [ urlPath++"WithOut>" ]
                )
            else
                ( "CodeInactive"++name
                , justHide [ urlPath++"With>" ]
                )
      ]
    , [ { id= "SwitchON_"++name
        , exclusive= True
        , triggers=
            [ TapGesture (uiPath++"ToggleGroup/Children/ToggleOFF>")
            , TapGesture (uiPath++"OFF>")
            , TapGesture (uiPath++"StateOFF>")
            --, TapGesture (uiPath++"ONText>")
            ]
        , actionRoot=
          Group { parallel= True, loop= False, performCount= 1, actions= 
            [ ( "MoveToggleON"
              , Transform 
                    { absolute= False
                    , duration= 0.333
                    , xformTarget= "</_Root_Scenes_"++scene++"_Behaviors_"++ name ++"_Switch_ON>"
                    , affected= uiPath++"ToggleGroup>"
                    }
              )
            , ( "Show"
              , Visibility { show= True, duration= 0, affected=
                    [ uiPath++"ToggleGroup/Children/ToggleON>"
                    , uiPath++"ON>"
                    , uiPath++"StateON>"
                    , "</Root/Scenes/"++scene++"/Children/Object_" ++ name ++">"
                    , urlPath++"With>"
                    ] }
              )
            , ( "Hide"
              , Visibility { show= False, duration= 0, affected=
                    [ uiPath++"ToggleGroup/Children/ToggleOFF>"
                    , uiPath++"OFF>"
                    , uiPath++"StateOFF>"
                    , urlPath++"WithOut>"
                    ] }
              )
            ] }
        }
      , { id= "SwitchOFF"++name
        , exclusive= True
        , triggers=
            [ TapGesture (uiPath++"ToggleGroup/Children/ToggleON>")
            , TapGesture (uiPath++"ON>")
            , TapGesture (uiPath++"StateON>")
            --, TapGesture (uiPath++"OFFText>")
            ]
        , actionRoot=
          Group { parallel= True, loop= False, performCount= 1, actions= 
            [ ( "MoveToggleOFF"
              , Transform 
                    { absolute= False
                    , duration= 0.333
                    , xformTarget= "</_Root_Scenes_"++scene++"_Behaviors_"++ name ++"_Switch_OFF>"
                    , affected= uiPath++"ToggleGroup>"
                    }
              )
            , ( "Show"
              , Visibility { show= True, duration= 0, affected=
                    [ uiPath++"ToggleGroup/Children/ToggleOFF>"
                    , uiPath++"OFF>"
                    , uiPath++"StateOFF>"
                    , urlPath++"WithOut>"
                    ] }
              )
            , ( "Hide"
              , Visibility { show= False, duration= 0, affected=
                    [ uiPath++"ToggleGroup/Children/ToggleON>"
                    , uiPath++"ON>"
                    , uiPath++"StateON>"
                    , "</Root/Scenes/"++scene++"/Children/Object_" ++ name ++">"
                    , urlPath++"With>"
                    ] }
              )
            ] }
        }
      ]
    )


usdGroup : String -> Maybe SkelAnim -> Xform -> List String -> List String
usdGroup name mbSkelAnim xForm children =
    let
        (schema, anim)=
            case mbSkelAnim of
                Nothing -> ("Xform", "")
                Just a -> ( "SkelRoot", usdSkel a ++ "\n" ++ usdSkelAnim a )
    in
    (  [ "def "++ schema ++" \""++ name ++"\" {"
       , usdXformBlock xForm
       , anim
       --, """def Xform "Generated" {}"""
       , """def Xform "Children" {"""
      ]
    ++ children
    ++ [ "}", "}" ]
    )


type alias GatherCodes =
    { x: Float
    , lines: List String 
    }

codeText : String -> String -> String -> String
codeText scene name text =
    String.join "\n"
        [ "def Preliminary_Text \""++ name ++"\" {"
        , "string content = \""++ text ++"\""
        , "float width = " ++ String.fromFloat (toFloat (String.length text) * codeCharWidth)
        , "float height = 0.225"
        , "float pointSize = 283.46457"
        , "float depth = 0.005"
        , "string[] font = [\"Menlo\"]"
        , "token horizontalAlignment = \"left\""
        , "rel material:binding = </Root/Scenes/"++scene++"/UITextBody>"
        , "token verticalAlignment = \"bottom\""
        , "token wrapMode = \"hardBreaks\""
        , "}"
        ]


codesForItem : String -> String -> Item -> GatherCodes -> GatherCodes
codesForItem scene exportLang i { x, lines } =
    let
        uiXform xOff =
            { scale= Nothing
            , orient= Nothing
            , translate= Just (x + (xOff/2.0), 0.0, 0.0) 
            }
        newLines=
            case i of
                Multi item ->
                    usdGroup
                        ("Item_" ++ item.id) Nothing
                        (uiXform <| if useShopIdAsCode then shopIdLength * codeCharWidth else codeCharWidth) <|
                        List.map
                            (\p ->
                                codeText
                                    scene
                                    ( "Product_" ++ p.product )
                                    ( langGet exportLang p.url )
                            )
                            item.products

                Select item ->
                    usdGroup
                        ("Item_" ++ item.id) Nothing
                        (uiXform <| if useShopIdAsCode then shopIdLength * codeCharWidth else codeCharWidth) <|
                        List.concatMap
                            (\group ->
                                List.map
                                    (\look ->
                                        codeText
                                            scene
                                            ("Look_" ++ group.id ++ "_" ++ look.id) 
                                            (if useShopIdAsCode then Maybe.withDefault "???" look.shopId else look.id)
                                    )
                                    group.looks
                            )
                            item.lookGroups
                SelectToggle item ->
                    usdGroup
                        ("Item_" ++ item.id) Nothing
                        (uiXform codeCharWidth) <|
                        (codeText scene ("WithOut") "NO") ::
                        List.concatMap
                            (\group ->
                                List.map
                                    (\look ->
                                        codeText
                                            scene 
                                            ("Look_" ++ group.id ++ "_" ++ look.id) 
                                            look.id
                                    )
                                    group.looks
                            )
                            item.lookGroups
                Toggle item ->
                    usdGroup
                        ("Item_" ++ item.id) Nothing
                        (uiXform 0.0)
                        [ codeText scene ("With"   ) "Y"
                        , codeText scene ("WithOut") "N"
                        ]
                _ -> []
    in
    { x= case i of
        Select _ -> 
            if useShopIdAsCode
                then x + ( shopIdLength * codeCharWidth )
                else x + ( 2.0 * codeCharWidth )
        SelectToggle _ -> x + ( 2.0 * codeCharWidth )
        Toggle _ -> x + codeCharWidth
        Multi item ->
            let
                maxLength= Maybe.withDefault 40 <| List.head <|List.sort <| List.map (\p -> String.length (langGet exportLang p.url)) item.products
            in
            x + ( (toFloat maxLength) * codeCharWidth )
        _ -> x
    , lines= List.append newLines lines
    }


codesForScene : ConfigScene -> Xform -> String -> List String
codesForScene scene xForm exportLang =
    let
        orderPath= "</Root/Scenes/"++scene.id++"/Children/ConfigUI/Children/Hideable/Children/Order/Children/"
        explainPath= "</Root/Scenes/"++scene.id++"/Children/ConfigUI/Children/Hideable/Children/Explain/Children/"
        inactiveTextMat = "</Root/Scenes/"++scene.id++"/UITextInactive>"
        activeTextMat = "</Root/Scenes/"++scene.id++"/UITextActive>"
        sceneBodyText = "</Root/Scenes/"++scene.id++"/UITextBody>"
        prefix= --scene.urlPrefix ++ "?l="++exportLang++"&c="
            if useShopIdAsCode then scene.urlPrefix else scene.urlPrefix ++ "?"
        xOff= codeCharWidth * (toFloat (String.length prefix))
        gather =
            List.foldl
                (codesForItem scene.id exportLang)
                { x= xOff
                , lines=
                    usdGroup
                        "Prefix" Nothing
                        { origin | translate= Just (xOff/2.0, 0.0, 0.0) }
                        [ codeText scene.id "Prefix" prefix ]
                }
                scene.items
        size= scene.uiScale
        urlScale = 0.455 / gather.x * size
    in
    List.concat
        [ usdGroup "Explain" Nothing
            { scale= Just (size/1.2, size/1.2, size/1.2)
            , orient= verticalOrientation
            , translate= Just (0, 0.005, 0.90)
            }
            [ usdPlace
                explainPath
                "Panel"
                True
                (Just "</AssetFactories/Masters/Factories/UIOrder>")
                { scale= Just (size*1.2, size, size*8.6)
                , orient= verticalOrientation
                , translate= Just (0, 0, 0.255)
                }
                defaultPhysics
            , usdText scene.id "Explain0"
                { orient= floorOrientation
                , scale= Just (size, size, size)
                , translate= Just (0.0014, 0.0055, -0.16)
                }
                sceneBodyText
                { width= 0.545
                , height= 0.12
                , pointSize= 56.7
                , verticalAlignment= Top, horizontalAlignment= Left
                }
                (langGet exportLang scene.translations.explain0)
            , usdText scene.id "Explain1"
                { orient= floorOrientation
                , scale= Just (size, size, size)
                , translate= Just (0.0164, 0.0055, -0.0834)
                }
                sceneBodyText
                { width= 0.515
                , height= 0.05 --0.048
                , pointSize= 56.7
                , verticalAlignment= Top, horizontalAlignment= Left
                }
                (langGet exportLang scene.translations.explain1)
            , usdText scene.id "Explain1Bullet"
                { orient= floorOrientation
                , scale= Just (size, size, size)
                , translate= Just (-0.2652, 0.0055, -0.096)
                }
                sceneBodyText
                { width= 0.01455
                , height= 0.025 --0.024
                , pointSize= 56.7
                , verticalAlignment= Top, horizontalAlignment= Left
                }
                "A"
            , usdText scene.id "Explain2"
                { orient= floorOrientation
                , scale= Just (size, size, size)
                , translate= Just (0.0164, 0.0055, 0)
                }
                sceneBodyText
                { width= 0.515
                , height= 0.1 --0.072
                , pointSize= 56.7
                , verticalAlignment= Top, horizontalAlignment= Left
                }
                (String.replace "<URL>" scene.urlPrefix <| langGet exportLang scene.translations.explain2)
            , usdText scene.id "Explain2Bullet"
                { orient= floorOrientation
                , scale= Just (size, size, size)
                , translate= Just (-0.2652, 0.0055, -0.0384)
                }
                sceneBodyText
                { width= 0.01455
                , height= 0.024
                , pointSize= 56.7
                , verticalAlignment= Top, horizontalAlignment= Left
                }
                "B"
            , usdText scene.id "Explain3"
                { orient= floorOrientation
                , scale= Just (size, size, size)
                , translate= Just (0.0164, 0.0055, 0.1066)
                }
                sceneBodyText
                { width= 0.515
                , height= 0.1 --0.096
                , pointSize= 56.7
                , verticalAlignment= Top, horizontalAlignment= Left
                }
                (langGet exportLang scene.translations.explain3)
            , usdText scene.id "Explain3Bullet"
                { orient= floorOrientation
                , scale= Just (size, size, size)
                , translate= Just (-0.2652, 0.0055, 0.0706)
                }
                sceneBodyText
                { width= 0.01455
                , height= 0.024
                , pointSize= 56.7
                , verticalAlignment= Top, horizontalAlignment= Left
                }
                "C"
            , usdPlace orderPath "CloseSummaryBtn" True
                (Just "</AssetFactories/Masters/Factories/UICloseButton>")
                { orient= floorOrientation
                , scale= Just (0.3, 0.22, 0.22)
                , translate= Just (0.0, 0.0055, 0.21)
                }
                defaultPhysics
            , usdText scene.id "CloseSummaryText"
                { orient= floorOrientation
                , scale= Just (1, 1, 1)
                , translate= Just (0.0, 0.0075, 0.21)
                }
                activeTextMat
                { verticalAlignment= Middle, horizontalAlignment= Center
                , width= 0.25
                , height= 0.03
                , pointSize= 70.86614
                }
                (langGet exportLang scene.translations.closeSummary)
            ]
        , usdGroup "Order" Nothing xForm
            [ usdPlace
                orderPath
                "Panel" True
                (Just "</AssetFactories/Masters/Factories/UIOrder>")
                { scale= Just (size, size, size)
                , orient= verticalOrientation
                , translate= Just (0, 0.0015/0.267 *size, (-0.005/0.267 + 0.03) * size)
                }
                defaultPhysics
            , usdText scene.id "Hint" 
                { orient= floorOrientation
                , scale= Just (size*0.75, size*0.75, size*0.75)
                , translate= Just (0, 0.011236 * size, -0.03 * size)
                }
                sceneBodyText
                { width= 0.5
                , height= 0.03
                , pointSize= 48.0
                , verticalAlignment= Middle, horizontalAlignment= Center
                }
                (langGet exportLang scene.translations.order)
            , String.join "\n" <| usdGroup "URL" Nothing
                { scale= Just (urlScale, urlScale, urlScale)
                , orient= floorOrientation
                , translate= Just (-0.225 *size, 0.011236 *size, -0.02 *size)
                } 
                gather.lines
            , usdPlace orderPath "OpenSummaryBtn" True
                (Just "</AssetFactories/Masters/Factories/UIOpenButton>")
                { orient= floorOrientation
                , scale= Just (0.4, 0.22, 0.22)
                , translate= Just (0.0, 0.0125, -0.0185)
                }
                defaultPhysics
            , usdText scene.id "OpenSummaryText"
                { orient= floorOrientation
                , scale= Just (1, 1, 1)
                , translate= Just (0.0, 0.013, -0.0185)
                }
                inactiveTextMat
                { verticalAlignment= Middle, horizontalAlignment= Center
                , width= 0.35
                , height= 0.03
                , pointSize= 70.86614
                }
                (langGet exportLang scene.translations.openSummary)
            ]
        ]


elmForLangStr : LanguageString -> String
elmForLangStr ls =
    "Dict.fromList [ " ++
    ( Dict.toList ls
        |> List.map
            (\(l,s) ->
                "(\""++ l ++"\", \""++ s ++ "\")"
            )
        |> String.join ", "
    ) ++ " ]"


elmForItem : Item -> Maybe String
elmForItem i =
    case i of
        Select item -> Just <|
            String.concat
            [ "{ item= \""++ item.id ++"\""
            , "\n    , descr= "++ elmForLangStr item.description
            , "\n    , codeWidth= 2"
            , "\n    , codes="
            , "\n        [ "
            , String.join "\n        , " <|
                List.concatMap
                    (\group ->
                        List.map
                            (\look ->
                                "{ id= \"" ++ look.id ++ "\", shopId= \""++ Maybe.withDefault "" look.shopId ++"\", group= "++ elmForLangStr group.description ++", descr= "++ elmForLangStr look.description ++" }"
                            )
                            group.looks
                    )
                    item.lookGroups
            , "\n        ]"
            , "\n    }"
            ]
        SelectToggle item -> Just <|
            String.concat
            [ "{ item= \""++ item.id ++"\""
            , "\n    , descr= "++ elmForLangStr item.description
            , "\n    , codeWidth= 2"
            , "\n    , codes="
            , "\n        [ "
            , String.join "\n        , " <|
                ("{ id= \"NO\", shopId= \""++ Maybe.withDefault "" item.shopIdForNone ++"\", group= Dict.fromList [], descr= " ++ elmForLangStr defaultTranslations.without ++ " }" ) ::
                List.concatMap
                    (\group ->
                        List.map
                            (\look ->
                                "{ id= \"" ++ look.id ++ "\", shopId= \""++ Maybe.withDefault "" look.shopId ++"\", group= "++ elmForLangStr group.description ++", descr= "++ elmForLangStr look.description ++" }"
                            )
                            group.looks
                    )
                    item.lookGroups
            , "\n        ]"
            , "\n    }"
            ]
        Toggle item ->
            let
                (noId, yesId) =
                    case item.shopIds of
                        Just (off, on) -> (off, on)
                        _ -> ("","")
            in
            Just <| String.concat
            [ "{ item= \""++ item.id ++"\""
            , "\n    , descr= "++ elmForLangStr item.description
            , "\n    , codeWidth= 1"
            , "\n    , codes="
            , "\n        [ { id= \"Y\", shopId=\""++ yesId ++"\", group= Dict.fromList [], descr= " ++ elmForLangStr defaultTranslations.with ++ " }"
            , "\n        , { id= \"N\", shopId=\""++ noId  ++"\", group= Dict.fromList [], descr= " ++ elmForLangStr defaultTranslations.without ++ " }"
            , "\n        ]"
            , "\n    }"
            ]
        _ -> Nothing


elmForInfo : InfoScene -> String
elmForInfo info =
    ""


elmForScene : ConfigScene -> String
elmForScene scene =
    String.concat
        [ "module Intro.Project exposing (items, fallBackLang, descr, sceneId, urlPrefix, shopUrlPrefix)\n\n"
        , "import Dict exposing (Dict)\n\n"
        , "fallBackLang = \""++ fallBackLang ++"\"\n\n"
        , "sceneId = \""++ scene.id ++"\"\n\n"
        , "urlPrefix = \""++ scene.urlPrefix ++"\"\n\n"
        , "urlAR = \""++ scene.urlAR ++"#allowsContentScaling=0&canonicalWebPageURL="++ scene.urlPrefix ++"\"\n\n"
        , "urlPreview = \""++ scene.urlPreview ++"\"\n\n"
        , "shopUrlPrefix = "++ elmForLangStr scene.shopUrl ++ "\n\n"
        , "descr = " ++ elmForLangStr scene.description ++ "\n\n"
        , "items =\n  [ "
        , String.join "\n  , " <| List.filterMap elmForItem scene.items
        , "\n  ]\n"
        ]


type alias GatherUsd =
    { uiXform: Xform
    , lines: List String 
    }


uiForItem : String -> Float -> String -> Translation -> Item -> GatherUsd -> GatherUsd
uiForItem scene size exportLang t i old =
    let
        labelTextMat    = "</Root/Scenes/"++scene++"/UITextBody>"
        activeTextMat   = "</Root/Scenes/"++scene++"/UITextActive>"
        inactiveTextMat = "</Root/Scenes/"++scene++"/UITextInactive>"
        xOffset = case i of
            SelectToggle item -> if item.startActive then 0.015 else -0.015
            Toggle item -> if item.startActive then 0.015 else -0.015
            _ -> 0.0

        toggleLines : String -> String -> String -> List String
        toggleLines name panelAsset description =
            let
                uiPath = "</Root/Scenes/"++scene++"/Children/ConfigUI/Children/Hideable/Children/UI_"++name++"/Children/"
                togglePath = uiPath++"ToggleGroup/Children/"
            in
                [ usdPlace uiPath "StateON" True
                    (Just "</AssetFactories/Masters/Factories/UIToggleActive>")
                    { scale= Just (size, size, size)
                    , orient= verticalOrientation
                    --, translate= Just (0.121166156559804 * size, 0.0025 * size, -0.023616233215451 * size)
                    , translate= Just (0.121166156559804 * size, 0.0025 * size, -0.0175 * size)
                    }
                    { colliderShape= Cube 1
                    , xForm=
                        { scale= Just (0.07/0.267 *size, 0.002/0.267 *size, 0.05/0.267 *size)
                        , orient= verticalOrientation
                        , translate= Just (0, 0, 0)
                        }
                    , extent= [(-0.035/0.267 *size, -0.001/0.267 *size, -0.025/0.267 *size), (0.035/0.267 *size, 0.001/0.267 *size, 0.025/0.267 *size)]
                    , frictionDynamic= 0.58
                    , frictionStatic= 0.58
                    , restitution= 0.48
                    }
                , usdPlace uiPath "StateOFF" True
                    (Just "</AssetFactories/Masters/Factories/UIToggleInactive>")
                    { scale= Just (size, size, size)
                    , orient= verticalOrientation
                    , translate= Just (0.121166156559804 * size, 0.0025 * size, -0.0175 * size)
                    }
                    { colliderShape= Cube 1
                    , xForm=
                        { scale= Just (0.07/0.267 *size, 0.002/0.267 *size, 0.05/0.267 *size)
                        , orient= verticalOrientation
                        , translate= Just (0, 0, 0)
                        }
                    , extent= [(-0.035/0.267 *size, -0.001/0.267 *size, -0.025/0.267 *size), (0.035/0.267 *size, 0.001/0.267 *size, 0.025/0.267 *size)]
                    , frictionDynamic= 0.58
                    , frictionStatic= 0.58
                    , restitution= 0.48
                    }
                , usdPlace uiPath "ON" True (Just panelAsset)
                    { scale= Just (size, size, size)
                    , orient= verticalOrientation
                    , translate= Just (0, 0.0015/0.267 *size, -0.005/0.267 *size)
                    }
                    { colliderShape= Cube 1
                    , xForm=
                        { scale= Just (0.07, 0.002, 0.05)
                        , orient= verticalOrientation
                        , translate= Just (0, 0, 0)
                        }
                    , extent= [(-0.25/0.267 *size, -0.005/0.267 *size, -0.03/0.267 *size), (0.25/0.267 *size, 0.005/0.267 *size, 0.03/0.267 *size)]
                    , frictionDynamic= 0.58
                    , frictionStatic= 0.58
                    , restitution= 0.48
                    }
                , usdPlace uiPath "OFF" True (Just panelAsset)
                    { scale= Just (size, size, size)
                    , orient= verticalOrientation
                    , translate= Just (0, 0.0015/0.267 *size, -0.005/0.267 *size)
                    --, translate= Just (0, 0.0015, -0.005)
                    }
                    { colliderShape= Cube 1
                    , xForm=
                        { scale= Just (size, size, size)
                        , orient= floorOrientation
                        , translate= Just (0, 0, 0)
                        }
                    , extent= [(-0.25/0.267 *size, -0.005/0.267 *size, -0.03/0.267 *size), (0.25/0.267 *size, 0.005/0.267 *size, 0.03/0.267 *size)]
                    , frictionDynamic= 0.58
                    , frictionStatic= 0.58
                    , restitution= 0.48
                    }
                , usdText scene "Label"
                    { orient= floorOrientation
                    , scale= Just (size, size, size)
                    , translate= Just (-0.14045 *size, 0.011236 *size, -0.0165 *size)
                    }
                    labelTextMat
                    { verticalAlignment= Middle, horizontalAlignment= Left
                    , width= 0.1914
                    , height= 0.03
                    , pointSize= 70.86614
                    }
                    description
                , usdText scene "ONText"
                    { scale= Just (size, size, size)
                    , orient= floorOrientation
                    , translate= Just (0.20225 *size, 0.011236 *size, -0.0165 *size)
                    }
                    labelTextMat
                    { width= 0.0827
                    , height= 0.03
                    , pointSize= 70.86614
                    , verticalAlignment= Middle, horizontalAlignment= Left
                    }
                    (langGet exportLang t.with)
                , usdText scene "OFFText"
                    { scale= Just (size, size, size)
                    , orient= floorOrientation
                    , translate= Just (0.03745 *size, 0.011236 *size, -0.0165 *size)
                    }
                    labelTextMat
                    { width= 0.09
                    , height= 0.03
                    , pointSize= 70.86614
                    , verticalAlignment= Middle, horizontalAlignment= Right
                    }
                    (langGet exportLang t.without)
                ,  String.join "\n" <| 
                    usdGroup "ToggleGroup" Nothing
                        { scale= Just (size, size, size)
                        , orient= verticalOrientation
                        , translate= Just (0.0325/0.267 *size + xOffset, 0.0085 *size, -0.0051/0.267 *size)
                        }
                        [ usdPlace togglePath "ToggleON" True (Just "</AssetFactories/Masters/Factories/UIToggleButton>")
                            { scale= Just (1, 1, 1)
                            , orient= verticalOrientation
                            , translate= Just (0, 0, 0)
                            }
                            { colliderShape= Capsule { axis= "Z", height= 0.019321170635521412/0.267 *size, radius= 0.005339414346963167/0.267 *size }
                            , xForm=
                                { scale= Just (0.07/0.267 *size, 0.002/0.267 *size, 0.05/0.267 *size)
                                , orient= verticalOrientation
                                , translate= Just (0, 0, 0)
                                }
                            , extent= [(-0.015/0.267 *size, -0.005/0.267 *size, -0.015/0.267 *size), (0.015/0.267 *size, 0.005/0.267 *size, 0.015/0.267 *size)]
                            , frictionDynamic= 0.58
                            , frictionStatic= 0.58
                            , restitution= 0.48
                            }
                        , usdPlace togglePath "ToggleOFF" True (Just "</AssetFactories/Masters/Factories/UIToggleButton>")
                            { scale= Just (1, 1, 1)
                            , orient= verticalOrientation
                            , translate= Just (0, 0, 0)
                            }
                            { colliderShape= Capsule { axis= "Z", height= 0.019321170635521412/0.267 *size, radius= 0.005339414346963167/0.267 *size }
                            , xForm=
                                { scale= Just (0.07/0.267 *size, 0.002/0.267 *size, 0.05/0.267 *size)
                                , orient= verticalOrientation
                                , translate= Just (0, 0, 0)
                                }
                            , extent= [(-0.015/0.267 *size, -0.005/0.267 *size, -0.015/0.267 *size), (0.015/0.267 *size, 0.005/0.267 *size, 0.015/0.267 *size)]
                            , frictionDynamic= 0.58
                            , frictionStatic= 0.58
                            , restitution= 0.48
                            }
                        ]
                ]

        selectLines : List LookGroup -> List String
        selectLines lookGroups =
            --let
            --    labelsPath  = "</Root/Scenes/"++scene++"/Children/ConfigUI/Children/Hideable/Children/UI_"++name++"/Children/SelectLabels/Children/"
            --in
            usdGroup "SelectLabels" Nothing
                { orient= floorOrientation
                , scale= Just (size, size, size)
                , translate= Just (0.0167/0.267 *size, 0.011236 *size, 0.0085/0.267 *size)
                }
                <| List.concatMap
                    (\group ->
                        List.map
                            (\look ->
                                let
                                    gd = String.replace "\n" " " (fixDescr <| langGet exportLang group.description)
                                    ld = String.replace "\n" " " (fixDescr <| langGet exportLang look.description)
                                    descr= if gd == "_" then ld else String.join "\n" [ gd, ld ]
                                in
                                usdText scene ( "Look_"++ group.id ++ "_" ++ look.id )
                                    { scale= Nothing
                                    , orient= Nothing
                                    , translate= Just (0,0,0)
                                    }
                                    labelTextMat
                                    { verticalAlignment= Middle, horizontalAlignment= Left
                                    , width= 0.6
                                    , height= 0.07
                                    , pointSize= 70.86614
                                    }
                                    descr
                            )
                            group.looks
                    )
                    lookGroups

        editLines : String -> List String
        editLines name =
            let
                editPath= "</Root/Scenes/"++scene++"/Children/ConfigUI/Children/Hideable/Children/UI_"++name++"/Children/EditButton/Children/"
            in
            usdGroup "EditButton" Nothing
                { orient= floorOrientation
                , scale= Just (size, size, size)
                --, translate= Just (0.0324 /0.267*size, 0.0035 /0.267*size, 0.0085 /0.267*size)
                , translate= Just (0.0324 /0.267*size, 0.0035 /0.267*size, 0.0085 /0.267*size)
                }
                [ usdText scene "Open"
                    origin
                    activeTextMat
                    { verticalAlignment= Middle, horizontalAlignment= Center
                    , width= 0.25
                    , height= 0.03
                    , pointSize= 70.86614
                    }
                    (langGet exportLang t.open)
                , usdText scene "Close"
                    origin
                    inactiveTextMat
                    { verticalAlignment= Middle, horizontalAlignment= Center
                    , width= 0.25
                    , height= 0.03
                    , pointSize= 70.86614
                    }
                    (langGet exportLang t.close)
                , usdPlace editPath "OpenBG" True (Just "</AssetFactories/Masters/Factories/UIOpenButton>")
                    { orient= verticalOrientation
                    , scale= Just (0.3, 0.24, 0.24)
                    , translate= Just (0.0, 0.0, -0.001)
                    }
                    defaultPhysics
                , usdPlace editPath "CloseBG" True (Just "</AssetFactories/Masters/Factories/UICloseButton>")
                    { orient= verticalOrientation
                    , scale= Just (0.3, 0.24, 0.24)
                    , translate= Just (0.0, 0.0, -0.001)
                    }
                    defaultPhysics
                ]
        
        groupToLabel : LookGroup -> String
        groupToLabel group =
            let
                descr = langGet exportLang group.description
            in
            if showGroupId
                then String.join " "[ group.id, "-", descr ]
                else descr

        lookToLabel : Look -> String
        lookToLabel look =
            let
                descr = langGet exportLang look.description
            in
            if showShopId
                then 
                    case look.shopId of
                        Just shopId -> String.join " " [ shopId, "-", descr ]
                        Nothing -> descr
                else descr

        overviewLines : String -> Float -> Float -> Float -> List LookGroup -> (Float, List String)
        overviewLines name xCenter yCenter yOffset lookGroups =
            if List.length lookGroups == 1
                then (yCenter, [])
                else
                  let
                    buttonsPath= "</Root/Scenes/"++scene++"/Children/ConfigUI/Children/Hideable/Children/Select_"++name++"/Children/Hideable/Children/Overview/Children/"
                    xCols= 3
                    xStart= xCenter - (xStep * (xCols - 1.0) / 2.0)
                    xStep= 0.1
                    yStep= 0.03
                    xGap= 0.025
                    yGap= 0.015
                    gather= List.foldl
                        (\group pos ->
                            let
                                newLines= String.join "\n" <|
                                    [ usdText scene ("Label_"++group.id)
                                        { scale= Just (1,1,1)
                                        , orient= verticalOrientation
                                        , translate= Just (pos.x, pos.y + yOffset, 0.003)
                                        }
                                        labelTextMat
                                        { verticalAlignment= Middle, horizontalAlignment= Center
                                        , width= 0.45
                                        , height= 0.12
                                        , pointSize= 28.0
                                        }
                                        <| groupToLabel group 
                                    , usdPlace buttonsPath ("Pick_"++group.id) True
                                        (Just "</AssetFactories/Masters/Factories/UIGroupPick>")
                                        { orient= verticalOrientation
                                        , scale= Just (0.17, 0.12, 0.1)
                                        , translate= Just (pos.x, pos.y + yOffset, 0.001)
                                        }
                                        defaultPhysics
                                    , usdPlace buttonsPath ("Active_"++group.id) True
                                        (Just "</AssetFactories/Masters/Factories/UIGroupActive>")
                                        { orient= verticalOrientation
                                        , scale= Just (0.17, 0.12, 0.1)
                                        , translate= Just (pos.x, pos.y + yOffset, 0.001)
                                        }
                                        defaultPhysics
                                    ]
                                (nextX, nextY) = 
                                    if pos.x < (xStart + (toFloat (xCols - 1)) * xStep)
                                        then ( pos.x + xStep, pos.y)
                                        else ( xStart, pos.y - yStep)
                                (nextPanelY, nextH) =
                                    if (pos.x >= xStart) && (pos.x < (xStart+xStep))
                                        then (pos.panelY - (yStep/2.0), pos.panelH + yStep)
                                        else (pos.panelY, pos.panelH)
                            in
                            { x= nextX, y= nextY
                            , panelH= nextH
                            , panelY= nextPanelY
                            , lines= newLines :: pos.lines
                            }
                        )
                        { x= xStart
                        , y= yCenter
                        , panelH= 2.0 * yGap + yStep
                        , panelY= yCenter + yStep
                        , lines= []
                        }
                        lookGroups
                  in
                  ( gather.panelH
                  , usdGroup "Overview" Nothing { origin | translate= Just (xCenter, gather.panelY, 0) } gather.lines
                  )

        multiMatrixLines : MultiItem -> List String
        multiMatrixLines m =
            let
                overviewHeight= 0.12
                xStartForms= -0.03
                xStartLooks= -0.18
                xStep= 0.1
                yStep= 0.04
                yForms= 0.035
                yLooks= 0.005
                yGap= 0.005
                yCenter= 0.0
                xText= 0.0
                yText= 0.12
                matrixPath= "</Root/Scenes/"++scene++"/Children/ConfigUI/Children/Hideable/Children/Multi_"++m.id++"/Children/Hideable/Children/Matrix/Children/"
                gatherLooks= List.foldl
                    (\look pos ->
                        let
                            gatherInnerForms= List.foldl
                                (\form formPos ->
                                    let
                                        missing=
                                            case List.filter (\p -> p.form == form.id && p.look == look.id) m.products of
                                                [] -> 
                                                    [ usdText scene ("Missing_"++form.id++"_"++look.id)
                                                        { scale= Just (1, 1, 1)
                                                        , orient= verticalOrientation
                                                        , translate= Just (formPos.x, pos.y, 0.001)
                                                        }
                                                        labelTextMat
                                                        { verticalAlignment= Middle
                                                        , horizontalAlignment= Center
                                                        , width= 0.5
                                                        , height= 0.1
                                                        , pointSize= 22.0
                                                        }
                                                        <| langGet exportLang t.notAvailable
                                                    ]
                                                _ -> []
                                        productLines= List.concatMap
                                            (\p ->
                                                if (p.form == form.id) && (p.look == look.id)
                                                    then List.concat
                                                        [ usdGroup ("Picked_"++p.product) Nothing
                                                            { origin | translate= Just (formPos.x, pos.y, 0.0)
                                                            }
                                                            [ usdPlace
                                                                (matrixPath++"Picked_"++p.product++"/Children/")
                                                                "pickedBack"
                                                                True
                                                                (Just ("</AssetFactories/Masters/Factories/UIPickedButton>"))
                                                                { scale= Just (1, 1, 1)
                                                                , orient= floorOrientation
                                                                , translate= Just (0.0, 0.0, 0.0)
                                                                }
                                                                defaultPhysics
                                                            , usdText scene "pickedText"
                                                                { scale= Just (1, 1, 1)
                                                                , orient= verticalOrientation
                                                                , translate= Just (0.0, 0.0, 0.001)
                                                                }
                                                                labelTextMat
                                                                { verticalAlignment= Middle
                                                                , horizontalAlignment= Center
                                                                , width= 0.5
                                                                , height= 0.1
                                                                , pointSize= 22.0
                                                                }
                                                                p.product
                                                            ]
                                                        , usdGroup ("Pick_"++p.product) Nothing
                                                            { origin | translate= Just (formPos.x, pos.y, 0.0)
                                                            }
                                                            [ usdPlace
                                                                (matrixPath++"Pick_"++p.product++"/Children/")
                                                                "pickBack"
                                                                True
                                                                (Just ("</AssetFactories/Masters/Factories/UIPickButton>"))
                                                                { scale= Just (1, 1, 1)
                                                                , orient= floorOrientation
                                                                , translate= Just (0.0, 0.0, 0.0)
                                                                }
                                                                defaultPhysics
                                                            , usdText scene "pickText"
                                                                { scale= Just (1, 1, 1)
                                                                , orient= verticalOrientation
                                                                , translate= Just (0.0, 0.0, 0.001)
                                                                }
                                                                labelTextMat
                                                                { verticalAlignment= Middle
                                                                , horizontalAlignment= Center
                                                                , width= 0.5
                                                                , height= 0.1
                                                                , pointSize= 22.0
                                                                }
                                                                p.product
                                                            ]
                                                        ]
                                                    else []
                                            )
                                            m.products
                                    in
                                    { x= formPos.x + xStep, lines= missing :: productLines :: formPos.lines }
                                )
                                { x= xStartForms
                                , lines= []
                                }
                                m.forms
                            lookLines=
                                [ usdPlace
                                    matrixPath
                                    ("Preview_"++look.id)
                                    True
                                    (Just ("</AssetFactories/Masters/Factories/UILookButton_" ++ m.id ++ "_" ++ look.id ++">"))
                                    { scale= Just (0.7, 0.7, 0.7)
                                    , orient= floorOrientation
                                    , translate= Just (xStartLooks, pos.y, 0.0)
                                    }
                                    defaultPhysics
                                , usdText scene ("Label_" ++ look.id)
                                    { scale= Just (1, 1, 1)
                                    , orient= verticalOrientation
                                    , translate= Just (xStartLooks + 0.07, pos.y, 0.001)
                                    }
                                    labelTextMat
                                    { verticalAlignment= Middle, horizontalAlignment= Center
                                    , width= 0.5, height= 0.1, pointSize= 20.0
                                    }
                                    <| lookToLabel look
                                ]
                        in
                        { y= pos.y - yStep, lines= lookLines :: (List.concat gatherInnerForms.lines) :: pos.lines }
                    )
                    { y= yLooks
                    , lines= []
                    }
                    m.looks
                gatherForms= List.foldl
                    (\form pos ->
                        let
                            newLines = String.join "\n" <|
                                [ usdPlace
                                    matrixPath
                                    ("Pick_"++form.id)
                                    True
                                    (Just ("</AssetFactories/Masters/Factories/UIFormButton_" ++ m.id ++ "_" ++ form.id ++">"))
                                    { scale= Just (1, 1, 1)
                                    , orient= floorOrientation
                                    , translate= Just (pos.x, pos.y + 0.035, 0.0)
                                    }
                                    defaultPhysics
                                , usdText scene ("Label_" ++ form.id)
                                    { scale= Just (1, 1, 1)
                                    , orient= verticalOrientation
                                    , translate= Just (pos.x, pos.y, 0.001)
                                    }
                                    labelTextMat
                                    { verticalAlignment= Middle, horizontalAlignment= Center
                                    , width= 0.5
                                    , height= 0.1
                                    , pointSize= 20.0
                                    }
                                    <| langGet exportLang form.description
                                ]
                            (nextX, nextY) = ( pos.x + xStep, pos.y)
                            (nextPanelY, nextH) = (pos.panelY, pos.panelH)
                        in
                        { x= nextX, y= nextY, panelH= nextH, panelY= nextPanelY, lines= newLines :: pos.lines }
                    )
                    { x= xStartForms
                    , y= yForms
                    , panelH= overviewHeight
                    , panelY= yCenter + ((overviewHeight + yGap + yGap)/2.0) + 0.05
                    , lines= []
                    }
                    m.forms
            in
            List.concat
                [ gatherForms.lines
                , List.concat <| gatherLooks.lines
                , [ usdText scene "Description"
                    { scale= Just (2, 2, 2)
                    , orient= verticalOrientation
                    , translate= Just (xText, yText, 0.001)
                    }
                    labelTextMat
                    { verticalAlignment= Middle
                    , horizontalAlignment= Center
                    , width= 0.5
                    , height= 0.1
                    , pointSize= 28.0
                    }
                    <| langGet exportLang m.description
                  ]
                ]

        multiPanelLines : MultiItem -> List String
        multiPanelLines m =
            List.map
                (\p ->
                    usdText scene ("Product_"++p.product)
                        { scale= Just (1, 1, 1)
                        , orient= verticalOrientation
                        , translate= Just (0.0, 0.0, 0.001)
                        }
                        labelTextMat
                        { verticalAlignment= Middle
                        , horizontalAlignment= Center
                        , width= 0.5
                        , height= 0.1
                        , pointSize= 22.0
                        }
                        <| String.join "\n\n"
                            [ langGet exportLang p.description
                            , "EAN: "++p.ean ++ "   Bestell-Nr.: "++p.product
                            , langGet exportLang p.url
                            ]
                )
                m.products
            {-
            List.concatMap
                (\p ->
                    let
                        overviewHeight= 0.0
                        xCols= toFloat m.columns
                        xStart= xCenter - (xStep * (xCols - 1.0) / 2.0)
                        yText=  0.08
                        yForms= -0.02
                        yLooks= -0.1
                        totalH= 0.25
                        xStep= 0.08
                        yStep= 0.08
                        xGap= 0.025
                        yGap= 0.01
                        lookPath= "</Root/Scenes/"++scene++"/Children/ConfigUI/Children/Hideable/Children/Multi_"++m.id++"/Children/Hideable/Children/Panel/Children/Product_"++ p.product ++"/Children/"
                        allowLooks=
                            List.filterMap
                                (\product ->
                                    if p.form == product.form 
                                    -- p.product /= product.product && 
                                        then Just product.look
                                        else Nothing
                                )
                                m.products
                        gatherForms= List.foldl
                            (\form pos ->
                                let
                                    newLines = String.join "\n" <|
                                        [ usdPlace
                                            lookPath
                                            ("Pick_"++form.id)
                                            True
                                            (Just ("</AssetFactories/Masters/Factories/UIFormButton_" ++ m.id ++ "_" ++ form.id ++">"))
                                            { scale= Just (1, 1, 1)
                                            , orient= floorOrientation
                                            , translate= Just (pos.x, pos.y + 0.035, 0.0)
                                            }
                                            defaultPhysics
                                        , usdText scene ("Label_" ++ form.id)
                                            { scale= Just (1, 1, 1)
                                            , orient= verticalOrientation
                                            , translate= Just (pos.x, pos.y, 0.001)
                                            }
                                            (if form.id == p.form then activeTextMat else labelTextMat)
                                            { verticalAlignment= Middle, horizontalAlignment= Center
                                            , width= 0.5
                                            , height= 0.1
                                            , pointSize= 20.0
                                            }
                                            <| langGet exportLang form.description
                                        ]
                                    (nextX, nextY) = 
                                        if pos.x < (xStart + (xCols - 1.0) * xStep)
                                            then ( pos.x + xStep, pos.y)
                                            else ( xStart, pos.y - yStep)
                                    (nextPanelY, nextH) =
                                        if (pos.x >= xStart) && (pos.x < (xStart+xStep))
                                            then (pos.panelY - (yStep/2.0), pos.panelH + yStep)
                                            else (pos.panelY, pos.panelH)
                                in
                                { x= nextX, y= nextY, panelH= nextH, panelY= nextPanelY, lines= newLines :: pos.lines }
                            )
                            { x= xStart
                            , y= yForms
                            , panelH= gatherLooks.panelH + yStep
                            , panelY= yCenter + ((overviewHeight + yGap + yGap)/2.0) + 0.05
                            , lines= []
                            }
                            m.forms
                        gatherLooks= List.foldl
                            (\look pos ->
                                let
                                    newLines = String.join "\n" <|
                                        [ usdPlace
                                            lookPath
                                            ("Pick_"++look.id)
                                            True
                                            (Just ("</AssetFactories/Masters/Factories/UILookButton_" ++ m.id ++ "_" ++ look.id ++">"))
                                            { scale= Just (1, 1, 1)
                                            , orient= floorOrientation
                                            , translate= Just (pos.x, pos.y + 0.035, 0.0)
                                            }
                                            defaultPhysics
                                        , usdText scene ("Label_" ++ look.id)
                                            { scale= Just (1, 1, 1)
                                            , orient= verticalOrientation
                                            , translate= Just (pos.x, pos.y, 0.001)
                                            }
                                            (if look.id == p.look then activeTextMat else labelTextMat)
                                            { verticalAlignment= Middle, horizontalAlignment= Center
                                            , width= 0.5
                                            , height= 0.1
                                            , pointSize= 20.0
                                            }
                                            <| lookToLabel look
                                        ]
                                    (nextX, nextY) = 
                                        if pos.x < (xStart + (xCols - 1.0) * xStep)
                                            then ( pos.x + xStep, pos.y)
                                            else ( xStart, pos.y - yStep)
                                    (nextPanelY, nextH) =
                                        if (pos.x >= xStart) && (pos.x < (xStart+xStep))
                                            then (pos.panelY - (yStep/2.0), pos.panelH + yStep)
                                            else (pos.panelY, pos.panelH)
                                in
                                if List.member look.id allowLooks
                                    then { x= nextX, y= nextY, panelH= nextH, panelY= nextPanelY, lines= newLines :: pos.lines }
                                    else pos
                            )
                            { x= xStart
                            , y= yLooks
                            , panelH= 2.0 * yGap + overviewHeight
                            , panelY= yCenter - ((overviewHeight + yGap + yGap)/2.0) + 0.05
                            , lines= []
                            }
                            m.looks
                        backW= (xCols*xStep) + (2*xGap)
                    in
                    usdGroup ("Product_"++p.product) Nothing
                        { origin | translate= Just (xCenter, 0, 0) } <| List.concat
                        [ gatherForms.lines
                        , [ usdText scene "Description"
                                { scale= Just (1, 1, 1)
                                , orient= verticalOrientation
                                , translate= Just (xCenter, yText, 0.001)
                                }
                                labelTextMat
                                { verticalAlignment= Middle
                                , horizontalAlignment= Center
                                , width= 0.5
                                , height= 0.1
                                , pointSize= 22.0
                                }
                                <| String.join "\n\n"
                                    [ langGet exportLang p.description
                                    , "EAN: "++p.ean ++ "   Bestell-Nr.: "++p.product
                                    , langGet exportLang p.url
                                    ]
                          ]
                        , gatherLooks.lines
                        , usdPanel scene "back" backW totalH totalH 10.0
                            { scale= Just (1, 1, 1)
                            , orient= floorOrientation
                            , translate= Just (0, 0, -0.001)
                            }
                        ]
                )
                m.products
            -}

        panelLines : String -> Float -> Float -> Float -> List LookGroup -> List String
        panelLines name xCenter yCenter yOffset lookGroups=
            let
                (overviewHeight, overview) = overviewLines name xCenter yCenter yOffset lookGroups
            in
            --(usdPlace
            --    ("</Root/Scenes/"++scene++"/Children/ConfigUI/Children/Hideable/Children/Select_"++name++"/Children/")
            --    "Rod" True
            --    (Just ("</AssetFactories/Masters/Factories/UIRod35x1>"))
            --    { scale= Just (1, 1, 1)
            --    , orient= verticalOrientation
            --    , translate= Just (xCenter, yCenter + 0.25, 0.0)
            --    }
            --    defaultPhysics) ::
            overview ++ (
             List.concatMap
              (\group ->
                let
                    looksPath= "</Root/Scenes/"++scene++"/Children/ConfigUI/Children/Hideable/Children/Select_"++name++"/Children/Hideable/Children/"++group.id++"/Children/"
                    xCols= 4
                    xStart= xCenter - (xStep * (xCols - 1.0) / 2.0)
                    xStep= 0.075
                    yStep= 0.075
                    xGap= 0.025
                    yGap= 0.01
                    gather= List.foldl
                        (\look pos ->
                            let
                                newLines = String.join "\n" <|
                                    [ usdPlace
                                        looksPath
                                        ("Pick_"++look.id)
                                        True
                                        (Just ("</AssetFactories/Masters/Factories/UILookButton_"++ name ++ "_" ++ group.id ++ "_" ++ look.id ++">"))
                                        { scale= Just (1, 1, 1)
                                        , orient= floorOrientation
                                        , translate= Just (pos.x, pos.y + 0.035, 0.0)
                                        }
                                        defaultPhysics
                                    , usdText scene ("Label_"++look.id)
                                        { scale= Just (1, 1, 1)
                                        , orient= verticalOrientation
                                        , translate= Just (pos.x, pos.y, 0.001)
                                        }
                                        labelTextMat
                                        { verticalAlignment= Middle, horizontalAlignment= Center
                                        , width= 0.35
                                        , height= 0.05
                                        , pointSize= 20.0
                                        }
                                        <| lookToLabel look
                                    ]
                                (nextX, nextY) = 
                                    if pos.x < (xStart + (toFloat (xCols - 1)) * xStep)
                                        then ( pos.x + xStep, pos.y)
                                        else ( xStart, pos.y - yStep)
                                (nextPanelY, nextH) =
                                    if (pos.x >= xStart) && (pos.x < (xStart+xStep))
                                        then (pos.panelY - (yStep/2.0), pos.panelH + yStep)
                                        else (pos.panelY, pos.panelH)
                            in
                            { x= nextX, y= nextY, panelH= nextH, panelY= nextPanelY, lines= newLines :: pos.lines }
                        )
                        { x= xStart
                        , y= yCenter - overviewHeight - yGap - yGap
                        , panelH= 2.0 * yGap + overviewHeight
                        , panelY= yCenter - ((overviewHeight + yGap + yGap)/2.0) + 0.05
                        , lines= []
                        }
                        group.looks
                    backW= (xCols*xStep) + (2*xGap)
                    back= String.join "\n" <| 
                        usdPanel scene "back" backW gather.panelH (gather.panelH - overviewHeight) 10.0
                            { scale= Just (1, 1, 1)
                            , orient= floorOrientation
                            , translate= Just (0, gather.panelY, -0.001)
                            }
                in
                usdGroup ("Panel_"++group.id) Nothing
                    { origin | translate= Just (xCenter, yCenter, 0) } 
                    ( back :: gather.lines )
              )
              lookGroups
            )

        tUi zOff = case old.uiXform.translate of
            Just (x,y,z) -> Just (x,y,z+zOff)
            _ -> old.uiXform.translate
    in
    case i of
        Static _ -> old

        Toggle item ->
            { uiXform= { scale= old.uiXform.scale, orient= old.uiXform.orient, translate= tUi (0.06 * size) }
            , lines= List.append old.lines <| List.concat
                    [ usdGroup ("UI_"++item.id) Nothing old.uiXform <| List.concat
                        [ toggleLines 
                            item.id 
                            "</AssetFactories/Masters/Factories/UIPanel50x6>"
                            (fixDescr <| langGet exportLang item.description)
                        ]
                    ]
            }

        SelectToggle item ->
            { uiXform= { scale= old.uiXform.scale, orient= old.uiXform.orient, translate= tUi (0.12 * size) }
            , lines= List.append old.lines <| List.concat
                    [ usdGroup ("UI_"++item.id) Nothing old.uiXform <| List.concat
                        [ toggleLines
                            item.id
                            "</AssetFactories/Masters/Factories/UISelectToggle50x12>"
                            (fixDescr <| langGet exportLang item.description)
                        , editLines item.id
                        , selectLines item.lookGroups
                        ]
                    , usdGroup ("Select_"++item.id) Nothing item.panelXform <|
                        usdGroup "Hideable" Nothing origin <| 
                            panelLines item.id 0.0 0.0 item.lookGroupsOffset item.lookGroups
                    ]
            }

        Select item ->
            { uiXform= { scale= old.uiXform.scale, orient= old.uiXform.orient, translate= tUi (0.12 * size) }
            , lines= List.append old.lines <|
                List.concat
                    [ usdGroup ("UI_"++item.id) Nothing old.uiXform <| List.concat
                        [ (usdPlace 
                            ("</Root/Scenes/"++scene++"/Children/ConfigUI/Children/Hideable/Children/UI_"++item.id++"/Children/")
                            "Panel"
                            True
                            (Just "</AssetFactories/Masters/Factories/UISelect50x12>")
                            { scale= Just (size, size, size)
                            , orient= verticalOrientation
                            , translate= Just (0, 0.0015/0.267 *size, 0.0115 * size)
                            }
                            defaultPhysics)
                        :: ( usdText scene "Label"
                            { orient= floorOrientation
                            , scale= Just (size, size, size)
                            , translate= Just (-0.14045 *size, 0.011236 *size, -0.0165 *size)
                            }
                            labelTextMat
                            { verticalAlignment= Middle, horizontalAlignment= Left
                            , width= 0.1914
                            , height= 0.03
                            , pointSize= 70.86614
                            }
                            (fixDescr <| langGet exportLang item.description)
                           )
                        :: editLines item.id
                        , selectLines item.lookGroups
                        ]
                    , usdGroup ("Select_"++item.id) Nothing item.panelXform <|
                        usdGroup "Hideable" Nothing origin <| 
                            panelLines item.id 0.0 0.0 item.lookGroupsOffset item.lookGroups
                    ]
            }
        Multi item ->
            { uiXform= { scale= old.uiXform.scale, orient= old.uiXform.orient, translate= tUi (0.12 * size) }
            , lines= List.append old.lines <|
                List.concat
                    [ usdGroup ("Multi_"++item.id) Nothing origin <|
                        usdGroup "Hideable" Nothing origin <| List.concat
                            [ usdGroup "Panel" Nothing item.panelXform <| List.concat
                                [ multiPanelLines item
                                , usdPanel scene "back" item.panelW item.panelH item.panelH 6.0
                                    { scale= Just (1, 1, 1)
                                    , orient= floorOrientation
                                    , translate= Just (0, 0, -0.002)
                                    }
                                ]
                            , usdGroup "Matrix" Nothing item.matriXform <| List.concat
                                [ multiMatrixLines item
                                , usdPanel scene "back" item.matrixW item.matrixH item.matrixH 6.0
                                    { scale= Just (1, 1, 1)
                                    , orient= floorOrientation
                                    , translate= Just (0, 0, -0.002)
                                    }
                                ]
                            ]
                    ]
            -- TODO Multi Matrix Panel and Buttons/Indicators
            }


usdText : String -> String -> Xform -> String -> TextAttr -> String -> String
usdText scene name xForm matRef attr text =
    String.join "\n"
        [ "def Xform \""++ name ++"\" {"  
        , usdXformBlock xForm
        , """def Xform "Generated" {"""
        , """def Preliminary_Text "Text" {"""
        , "string content = \"\"\""++
            (String.replace "\\n" "\n" text)++
            "\"\"\""
        , "float width = " ++ String.fromFloat attr.width
        , "float height = " ++ String.fromFloat attr.height
        , "float pointSize = " ++ String.fromFloat attr.pointSize
        , "float depth = 0.00025"
        , "string[] font = [\"Helvetica\"]"
        , "token horizontalAlignment = \"" ++
            ( case attr.horizontalAlignment of
                Left -> "left"
                Center -> "center"
                Right -> "right"
                -- TODO justified?
            ) ++ "\""
        , "rel material:binding = "++matRef
        , "token verticalAlignment = \"" ++
            ( case attr.verticalAlignment of
                Top -> "top"
                Middle -> "middle"
                Bottom -> "bottom"
            ) ++ "\""
        , "token wrapMode = \"flowing\""
        , "}"
        , "}"
        , """def Xform "Children" {}"""
        , "}"
        ]


addInput : UsdInput -> Look -> Look
addInput usdInput l =
    Look l.id l.description 
        (Material l.material.id (usdInput :: l.material.inputs))
        l.thumbnail
        l.shopId
        1.0 1.0
        "normal"
        False


expandLook : Bool -> Look -> Look
expandLook expand look =
    { look | expandedView= expand }


changeTex : Maybe (String, Int) -> String -> Look -> Look
changeTex mbNew inputId l =
    let
        newTex= Maybe.map (\(t,map) -> defaultTx t map) mbNew
        newInputs= List.map
            (\input ->
                if input.id == inputId then { input | texture= newTex } else input
            )
            l.material.inputs 
    in
    { l | material= Material l.material.id newInputs }


changeValues : String -> String -> Look -> Look
changeValues inputId values l =
    let
        newInputs= List.map
            (\input ->
                if input.id == inputId then { input | values= values } else input
            )
            l.material.inputs 
    in
    { l | material= Material l.material.id newInputs }


changeLookInGroups : String -> String -> String -> (String -> Look -> Look) -> List LookGroup -> List LookGroup
changeLookInGroups grpId lookId newStr fun groups = 
    List.map
        (\group ->
            if group.id == grpId
                then
                    { group | looks= List.map
                        (\look ->
                            if look.id == lookId
                                then fun newStr look
                                else look
                        )
                        group.looks 
                    }
                else
                    group
        ) groups


scaleUV : (Float, Float) -> Mesh -> Mesh
scaleUV (scaleU, scaleV) mesh =
    { mesh | st= List.map (\(u,v) -> ( u * scaleU, v * scaleV ) ) mesh.st }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        changePage : String -> String -> String -> (Page -> Page) -> Model
        changePage infoId panelId pageId change =
            { model | scenes=
                List.map
                    (\sc ->
                        case sc of
                            Info info ->
                                if info.id == infoId 
                                    then
                                        Info
                                            { info 
                                            | panels= List.map
                                                (\panel ->
                                                    if panel.id == panelId
                                                        then
                                                            { panel
                                                            | pages= List.map
                                                                (\page ->
                                                                    if page.id == pageId
                                                                        then change page
                                                                        else page
                                                                )
                                                                panel.pages
                                                            }
                                                        else panel
                                                )
                                                info.panels
                                            }
                                    else sc
                            _ -> sc
                    )
                    model.scenes
            }

        changeInfoPanel : String -> String -> (Panel -> Panel) -> Model
        changeInfoPanel sceneId panelId changePanel =
            { model | scenes=
                List.map
                    (\sc ->
                        case sc of
                            Info info ->
                                if info.id == sceneId 
                                    then
                                        Info
                                            { info 
                                            | panels= List.map
                                                (\panel ->
                                                    if panel.id == panelId
                                                        then changePanel panel
                                                        else panel
                                                )
                                                info.panels
                                            }
                                    else sc
                            _ -> sc
                    )
                    model.scenes
            }

        changeInfoScene : String -> (InfoScene -> InfoScene) -> Model
        changeInfoScene sceneId changeInfo =
            { model | scenes=
                List.map
                    (\sc ->
                        case sc of
                            Info scene ->
                                if scene.id == sceneId 
                                    then
                                        Info <| changeInfo scene
                                    else sc
                            _ -> sc
                    )
                    model.scenes
            }

        changeConfigScene : String -> (ConfigScene -> ConfigScene) -> Model
        changeConfigScene sceneId changeCfg =
            { model | scenes=
                List.map
                    (\sc ->
                        case sc of
                            Config scene ->
                                if scene.id == sceneId 
                                    then
                                        Config <| changeCfg scene
                                    else sc
                            _ -> sc
                    )
                    model.scenes
            }
    in
    case msg of
        NoOp ->
            ( model, Cmd.none )

        NoOpStr _ ->
            ( model, Cmd.none )

        WindowSize width height ->
            ( { model
                | width = toFloat width
                , height = toFloat height
                --, editor = Editor.resize (Helper.Common.windowWidth w) (Helper.Common.windowHeight h) model.editor
              }
            , Cmd.none
            )

        ChangeLanguage str ->
            ( { model | newLang= str }, Cmd.none )

        ToggleLanguage lang ->
            ( { model | languages= 
                Dict.map
                    (\l b ->
                        if l == lang then not b else b
                    )
                    model.languages
              }
            , Cmd.none
            )

        SetExportLanguage ->
            ( { model | exportLang= model.newLang }, Cmd.none )

        AddLanguage ->
            let
                changeWithGroups= 
                    \a -> 
                    { a 
                    | description= langAdd model.newLang "????" a.description
                    , lookGroups = 
                        List.map
                            (\group ->
                                { group 
                                | description= langAdd model.newLang "????" group.description
                                , looks= List.map (\l -> { l | description= langAdd model.newLang "????" l.description }) group.looks
                                }
                            )
                            a.lookGroups
                    }
            in
            ( { model 
              | newLang= ""
              , languages= Dict.insert model.newLang True model.languages
              , scenes=
                List.map
                    (\sc ->
                        case sc of
                            Config scene ->
                                Config <|
                                    (\cs -> 
                                        { cs 
                                        | description= langAdd model.newLang "????" cs.description
                                        , items=
                                            List.map
                                                (\item ->
                                                    case item of
                                                        Static i -> Static i
                                                        Toggle i ->
                                                            Toggle
                                                                { i 
                                                                | description= langAdd model.newLang "????" i.description
                                                                , look= (\l -> { l | description= langAdd model.newLang "????" l.description }) i.look
                                                                }
                                                        Select i -> Select <| changeWithGroups i
                                                        SelectToggle i -> SelectToggle <| changeWithGroups i
                                                        Multi i ->
                                                            Multi
                                                                { i
                                                                | products= List.map
                                                                    (\p ->
                                                                        { p
                                                                        | description= langAdd model.newLang "????" p.description
                                                                        , url= langAdd model.newLang "????" p.url
                                                                        }
                                                                    )
                                                                    i.products
                                                                , forms= List.map
                                                                    (\f ->
                                                                        { f | description= langAdd model.newLang "????" f.description }
                                                                    )
                                                                    i.forms
                                                                , looks= List.map
                                                                    (\l ->
                                                                        { l | description= langAdd model.newLang "????" l.description }
                                                                    )
                                                                    i.looks
                                                                }
                                                )
                                                cs.items
                                        }
                                    )
                                    scene
                            _ -> sc
                    )
                    model.scenes
              }
            , Cmd.none
            )

        ExportScene ->
          case model.scenes of
            [ Info info ] ->
                let
                    scene = info.id
                    path = "</Root/Scenes/"++scene++"/Children/"
                    objects = ""
                    codeLines = ""
                    factories = ""
                    behaviors = []
                    startActions = []
                    gather = { lines= [] }
                    usda = String.join "\n"
                        (
                            [ usdHeader "INFO" defaultAnimMeta ""
                            , usdSceneStart scene info.anchor 
                            , usdMaterial ("/Root/Scenes/"++scene) "UITextBody" uiTextBodyMaterial
                            , usdMaterial ("/Root/Scenes/"++scene) "UITextActive" uiTextActiveMaterial
                            , usdMaterial ("/Root/Scenes/"++scene) "UITextInactive" uiTextInactiveMaterial
                            , usdMaterial ("/Root/Scenes/"++scene) "UIPanelMaterial" uiPanelMaterial
                            , usdMaterial ("/Root/Scenes/"++scene) "UIPanelAccent" <| uiPanelAccent <| toLinearStr company.accentColor
                            , "def Xform \"Children\" {"
                            , objects
                            , codeLines
                            , String.join "\n" <| 
                                usdGroup "ConfigUI" Nothing origin <|
                                    List.concat
                                        [ usdGroup "Hint" Nothing
                                            { origin | translate= Just (0, 0.33, 0.75) }
                                            [ usdPlace (path++"ConfigUI/Children/Hint/Children/") 
                                                "Bubble"
                                                True
                                                (Just ("</AssetFactories/Masters/Factories/UIHintBubble>"))
                                                origin
                                                defaultPhysics
                                            , usdText scene "UIHint" 
                                                { origin | translate= Just (0,0,0.011) }
                                                ("</Root/Scenes/"++scene++"/UITextBody>")
                                                { width= 0.2823834
                                                , height= 0.13238342
                                                , pointSize= 95.0
                                                , verticalAlignment= Middle, horizontalAlignment= Center
                                                }
                                                (langGet model.exportLang info.hint)
                                            ]
                                        , usdGroup "Hideable" Nothing origin <| gather.lines
                                        ]
                            , "}"
                            , "def Scope \"Behaviors\" {" 
                            ]
                        ++( [ usdBehavior scene
                                { id= "PresentHint"
                                , exclusive= False
                                , triggers= [ SceneTransition "enter" ]
                                , actionRoot= Group
                                    { parallel= False, loop= False, performCount= 1
                                    , actions= [] -- TODO
                                        {-
                                        [ ( "LookAtCamera"
                                            , LookAtCamera
                                                { affected= [ ("</Root/Scenes/"++scene++"/Children/ConfigUI/Children/Hint>") ]
                                                , duration= 5.0
                                                , front= (0,0,1)
                                                , upVector= (0,0,0)
                                                }
                                            )
                                        , ( "Hide"
                                          , Visibility
                                                { show= False, duration= 0.33
                                                , affected= [ ("</Root/Scenes/"++scene++"/Children/ConfigUI/Children/Hint>") ] 
                                                }
                                          )
                                        ]
                                        -}
                                    }
                                }
                            , usdBehavior scene
                                { id= "Start"
                                , exclusive= False
                                , triggers= [ SceneTransition "enter" ]
                                , actionRoot= Group 
                                    { parallel= True, loop= False, performCount= 1
                                    , actions= List.concat startActions 
                                    }
                                }
                            ]
                          )
                        ++( [ usdBehavior scene
                                { id= "FaceToCamera"
                                , exclusive= False
                                , triggers= [ SceneTransition "enter" ]
                                , actionRoot=
                                    Group
                                        { parallel= True, loop= False, performCount= 1
                                        , actions= [
                                            ( "LookAtCamera"
                                            , LookAtCamera
                                                { affected=
                                                    List.map
                                                        (\p ->
                                                            "</Root/Scenes/"++scene++"/Children/ConfigUI/Children/Hideable/Children/InfoPanel_"++p.id++"/Children/Hideable>"
                                                        )
                                                        info.panels
                                                , duration= 3000
                                                , front= (0,0,1)
                                                , upVector= (0,0,0)
                                                }
                                            )
                                          ]
                                        }
                                }
                            ]
                          )
                        ++( List.map (usdBehavior scene) <| List.concat behaviors )
                        ++[ "}\n}\n}\n}"
                          , factories
                          , "}"
                          ] 
                        )
                in
                model |> withCmd (Outside.sendInfo (Outside.WriteFile { fileName="Test.usda", content=usda}))

            [ Config sc ] ->
                let
                    scene = sc.id
                    path = "</Root/Scenes/"++scene++"/Children/"
                    objects= String.join "\n" <| List.concatMap
                        (\i ->
                            let
                                usdLookGroups item =
                                  usdGroup ("Object_"++item.id) item.skelAnim item.xForm <| 
                                    List.concatMap
                                        (\group ->
                                            List.map
                                                (\look ->
                                                    usdPlace 
                                                        (path++"Object_"++item.id++"/Children/")
                                                        ("Instance_" ++ group.id ++ "_" ++ look.id)
                                                        True
                                                        (Just ("</AssetFactories/Masters/Factories/PrimitiveShapeAssetFactory_Object_"++item.id++"_"++group.id++"_"++look.id++">"))
                                                        origin
                                                        item.physics
                                                )
                                                group.looks
                                        )
                                        item.lookGroups
                            in
                            case i of
                                Static item ->
                                    List.map
                                        (\(idx, xf) ->
                                            usdPlace path ("Object_"++ item.id ++"_"++ String.fromInt idx) True
                                                (Just ("</AssetFactories/Masters/Factories/PrimitiveShapeAssetFactory_Object_"++ item.id ++ ">"))
                                                xf item.physics
                                        )
                                        <| List.indexedMap Tuple.pair item.xForms
                                Toggle item ->
                                    [ usdPlace path ("Object_"++item.id) True Nothing item.xForm item.physics ]
                                Select item ->
                                    usdLookGroups item
                                SelectToggle item ->
                                    usdLookGroups item
                                Multi item ->
                                    List.map
                                        (\p ->
                                            usdPlace path ("Object_" ++ item.id ++ "_" ++ p.product) True Nothing item.xForm defaultPhysics
                                        )
                                        item.products
                        )
                        sc.items

                    gather = List.foldl 
                        ( uiForItem scene sc.uiScale model.exportLang sc.translations )
                        { uiXform= sc.uiXform
                            --{ scale= Just (1,1,1)
                            --, orient= Just [1,0,0,0]
                            --, translate= Just (0,0,0.5)
                            --}
                        , lines= [] 
                        }
                        sc.items

                    codeLines = String.join "\n" <| 
                        codesForScene sc gather.uiXform model.exportLang

                    matFactoryPrefix = "/AssetFactories/Masters/Materials"
                    factories = usdScope "AssetFactories" "( active = false )"
                            ( String.join "\n"
                                ( [ "def Scope \"Masters\" {"
                                , "def Scope \"Factories\" {"
                                , usdShapeAsset "UIPanel50x6"           "</AssetFactories/Masters/Meshes/UIPanel50x6>"          "</AssetFactories/Masters/Materials/UIPanel>"
                                --, usdShapeAsset "UIToggle50x6"          "</AssetFactories/Masters/Meshes/UIToggle50x6>"         "</AssetFactories/Masters/Materials/UIPanel>"
                                , usdShapeAsset "UISelectToggle50x12"   "</AssetFactories/Masters/Meshes/UISelectToggle50x12>"  "</AssetFactories/Masters/Materials/UIPanel>"
                                , usdShapeAsset "UISelect50x12"         "</AssetFactories/Masters/Meshes/UISelect50x12>"        "</AssetFactories/Masters/Materials/UIPanel>"
                                , usdShapeAsset "UIPanelBG"             "</AssetFactories/Masters/Meshes/UIPanelBG>"            "</AssetFactories/Masters/Materials/UIPanel>"
                                , usdShapeAsset "UIToggleButton"        "</AssetFactories/Masters/Meshes/UIToggleButton>"       "</AssetFactories/Masters/Materials/UIPanel>"
                                , usdShapeAsset "UIToggleActive"        "</AssetFactories/Masters/Meshes/UIToggleState>"        "</AssetFactories/Masters/Materials/UIToggleActive>"
                                , usdShapeAsset "UIToggleInactive"      "</AssetFactories/Masters/Meshes/UIToggleState>"        "</AssetFactories/Masters/Materials/UIToggleInactive>"
                                , usdShapeAsset "UIOpenButton"          "</AssetFactories/Masters/Meshes/UIEditButtonBG>"       "</AssetFactories/Masters/Materials/UIOpenButton>"  
                                , usdShapeAsset "UICloseButton"         "</AssetFactories/Masters/Meshes/UIEditButtonBG>"       "</AssetFactories/Masters/Materials/UICloseButton>"
                                , usdShapeAsset "UIGroupPick"           "</AssetFactories/Masters/Meshes/UIEditButtonBG>"       "</AssetFactories/Masters/Materials/UIGroupPick>"
                                , usdShapeAsset "UIGroupActive"         "</AssetFactories/Masters/Meshes/UIEditButtonBG>"       "</AssetFactories/Masters/Materials/UIGroupActive>"
                                , usdShapeAsset "UIRod35x1"             "</AssetFactories/Masters/Meshes/UIRod35x1>"            "</AssetFactories/Masters/Materials/UIRod>"
                                , usdShapeAsset "UIHintBubble"          "</AssetFactories/Masters/Meshes/UIHintBubble>"         "</AssetFactories/Masters/Materials/UIPanel>"
                                , usdShapeAsset "UIOrder"               "</AssetFactories/Masters/Meshes/UIOrder>"              "</AssetFactories/Masters/Materials/UIPanel>"
                                , usdShapeAsset "UIPickedButton"        "</AssetFactories/Masters/Meshes/UIPickButton>"         "</AssetFactories/Masters/Materials/UIToggleActive>"
                                , usdShapeAsset "UIPickButton"          "</AssetFactories/Masters/Meshes/UIPickButton>"         "</AssetFactories/Masters/Materials/UIToggleInactive>"
                                --, usdShapeAsset "UILookButtonBorder"    "</AssetFactories/Masters/Meshes/UILookButtonBorder>"   "</AssetFactories/Masters/Materials/UIPanel>"
                                ]
                                ++( List.concatMap
                                    (\i ->
                                        let
                                            lookGroups itemId lgs =
                                                List.concatMap
                                                    (\group ->
                                                        List.map
                                                            (\look ->
                                                                let
                                                                    suffix = itemId ++ "_" ++ group.id ++ "_" ++ look.id
                                                                in
                                                                String.join "\n"
                                                                    [ usdShapeAsset
                                                                        ("PrimitiveShapeAssetFactory_Object_" ++ suffix)
                                                                        ("</AssetFactories/Masters/Meshes/PrimitiveShapeAssetFactory_Object_"++ itemId ++ "_" ++ look.meshId ++ "_Mesh>")
                                                                        ("</AssetFactories/Masters/Materials/PrimitiveShapeAssetFactory_Object_"++ suffix ++">")
                                                                    , usdShapeAsset
                                                                        ("UILookButton_" ++ suffix )
                                                                        ("</AssetFactories/Masters/Meshes/PrimitiveShapeAssetFactory_Button_" ++ suffix ++"_Mesh>")
                                                                        ("</AssetFactories/Masters/Materials/LookBtn_" ++ suffix ++">")
                                                                    --, usdShapeAsset
                                                                    --    ("UILookButton_" ++ suffix )
                                                                    --    ("</AssetFactories/Masters/Meshes/PrimitiveShapeAssetFactory_ButtonFront_" ++ suffix ++"_Mesh>")
                                                                    --    ("</AssetFactories/Masters/Materials/Look_" ++ suffix ++">")
                                                                    --, usdShapeAsset
                                                                    --    ("UILookButtonBorder_" ++ suffix )
                                                                    --    ("</AssetFactories/Masters/Meshes/PrimitiveShapeAssetFactory_ButtonBorder_" ++ suffix ++"_Mesh>")
                                                                    --    ("</AssetFactories/Masters/Materials/Look_" ++ suffix ++">")
                                                                    ]
                                                            )
                                                            group.looks
                                                    )
                                                    lgs
                                        in
                                        case i of
                                            Static item ->
                                                [ usdShapeAsset
                                                    ("PrimitiveShapeAssetFactory_Object_" ++ item.id)
                                                    ("</AssetFactories/Masters/Meshes/PrimitiveShapeAssetFactory_Object_"++ item.id ++"_Mesh>")
                                                    ("</AssetFactories/Masters/Materials/PrimitiveShapeAssetFactory_Object_"++ item.id ++ "__" ++ item.look.id ++">")
                                                ]
                                            Toggle item ->
                                                [ usdShapeAsset
                                                    ("PrimitiveShapeAssetFactory_Object_" ++ item.id)
                                                    ("</AssetFactories/Masters/Meshes/PrimitiveShapeAssetFactory_Object_"++ item.id ++"_Mesh>")
                                                    ("</AssetFactories/Masters/Materials/PrimitiveShapeAssetFactory_Object_"++ item.id ++ "__" ++ item.look.id ++">")
                                                ]
                                            Select item -> lookGroups item.id item.lookGroups
                                            SelectToggle item -> lookGroups item.id item.lookGroups
                                            Multi item ->
                                                List.concat
                                                    [ List.map
                                                        (\p ->
                                                            let
                                                                name = item.id ++ "_" ++ p.product
                                                            in
                                                            usdShapeAsset
                                                                ("PrimitiveShapeAssetFactory_Object_" ++ name)
                                                                ("</AssetFactories/Masters/Meshes/PrimitiveShapeAssetFactory_Object_" ++ name ++"_Mesh>")
                                                                ("</AssetFactories/Masters/Materials/PrimitiveShapeAssetFactory_Object_" ++ name ++ ">")
                                                        )
                                                        item.products
                                                    , List.map
                                                        (\look ->
                                                            let
                                                                name = item.id ++ "_" ++ look.id
                                                            in
                                                            usdShapeAsset
                                                                ("UILookButton_" ++ name)
                                                                --("</AssetFactories/Masters/Meshes/PrimitiveShapeAssetFactory_Button_" ++ name ++"_Mesh>")
                                                                ("</AssetFactories/Masters/Meshes/UILookButton>")
                                                                ("</AssetFactories/Masters/Materials/LookBtn_" ++ name ++">")
                                                        )
                                                        item.looks
                                                    , List.map
                                                        (\form ->
                                                            let
                                                                name = item.id ++ "_" ++ form.id
                                                            in
                                                            usdShapeAsset
                                                                ("UIFormButton_" ++ name)
                                                                ("</AssetFactories/Masters/Meshes/UIFormButton>")
                                                                ("</AssetFactories/Masters/Materials/Form_" ++ name ++">")
                                                        )
                                                        item.forms
                                                    ]
                                    )
                                    sc.items
                                )
                                ++[ "}"
                                , "def Scope \"Meshes\" {"
                                , usdMesh "UIPanel50x6"     Nothing uiPanel50x6Mesh
                                --, usdMesh "UIToggle50x6"  Nothing uiToggle50x6Mesh
                                , usdMesh "UISelectToggle50x12" Nothing <| offsetPointsZ { offset= 0.06, min= 0.028, max= 0.031 } uiPanel50x6Mesh
                                , usdMesh "UIOrder"         Nothing <| offsetPointsZ { offset= -0.06, min= 0.028, max= 1.0 } uiSelect50x12Mesh
                                , usdMesh "UISelect50x12"   Nothing uiSelect50x12Mesh
                                , usdMesh "UIEditButtonBG"  Nothing uiBackgroundMesh
                                , usdMesh "UIToggleButton"  Nothing uiToggleButtonMesh
                                , usdMesh "UIToggleState"   Nothing uiToggleStateMesh
                                , usdMesh "UILookButton"    Nothing uiLookButtonMesh
                                , usdMesh "UIFormButton"    Nothing uiFormButtonMesh
                                , usdMesh "UIPickButton"    Nothing uiPickButtonMesh
                                --, usdMesh "UILookButtonFront"  Nothing uiLookButtonFrontMesh
                                --, usdMesh "UILookButtonBorder" Nothing uiLookButtonBorderMesh
                                , usdMesh "UIPanelBG"       Nothing uiPanelBackgroundMesh
                                , usdMesh "UIRod35x1"       Nothing uiRod35x1Mesh
                                , usdMesh "UIHintBubble"    Nothing uiHintMesh
                                ]
                                ++( List.concatMap    -- Object related meshes
                                    (\i ->
                                        case i of
                                            Static item ->
                                                [ usdMesh ("PrimitiveShapeAssetFactory_Object_" ++ item.id ++ "_Mesh") item.skelAnim item.mesh ]
                                            Toggle item ->
                                                [ usdMesh ("PrimitiveShapeAssetFactory_Object_" ++ item.id ++ "_Mesh") item.skelAnim item.mesh ]
                                            Select item ->
                                                List.map
                                                    (\(key,mesh) ->
                                                        usdMesh ("PrimitiveShapeAssetFactory_Object_" ++ item.id ++ "_" ++ key ++ "_Mesh") item.skelAnim mesh
                                                    ) <| Dict.toList item.meshesById
                                            SelectToggle item ->
                                                List.map
                                                    (\(key,mesh) ->
                                                        usdMesh ("PrimitiveShapeAssetFactory_Object_" ++ item.id ++ "_" ++ key ++ "_Mesh") item.skelAnim mesh
                                                    ) <| Dict.toList item.meshesById
                                            Multi item ->
                                                List.map
                                                    (\p ->
                                                        let
                                                            mesh=
                                                                case List.head <| List.filter (\f -> f.id == p.form) item.forms of
                                                                    Nothing -> k3Mesh
                                                                    Just f -> f.cfgMesh
                                                        in
                                                        usdMesh ("PrimitiveShapeAssetFactory_Object_"  ++ item.id ++ "_" ++ p.product ++ "_Mesh") Nothing mesh
                                                    )
                                                    item.products
                                    )
                                    sc.items
                                )
                                ++( List.filterMap  -- individual buttons with custom UVs
                                    (\i ->
                                        let
                                            loopLooks itemId lgs =
                                                String.join "\n"<| List.concatMap
                                                    (\group ->
                                                        List.map
                                                            (\look ->
                                                                --String.join "\n"
                                                                --    [ usdMesh ("PrimitiveShapeAssetFactory_ButtonFront_" ++ itemId ++ "_" ++ group.id ++ "_" ++ look.id ++"_Mesh")
                                                                --        Nothing <| scaleUV (look.btnScaleU, look.btnScaleV) uiLookButtonFrontMesh
                                                                --    , usdMesh ("PrimitiveShapeAssetFactory_ButtonBorder_" ++ itemId ++ "_" ++ group.id ++ "_" ++ look.id ++"_Mesh")
                                                                --        Nothing uiLookButtonBorderMesh
                                                                --    ]
                                                                usdMesh ("PrimitiveShapeAssetFactory_Button_" ++ itemId ++ "_" ++ group.id ++ "_" ++ look.id ++"_Mesh")
                                                                    Nothing <| scaleUV (look.btnScaleU, look.btnScaleV) uiLookButtonMesh
                                                            )
                                                            group.looks
                                                    )
                                                    lgs
                                        in
                                        case i of
                                            Static _            -> Nothing
                                            Toggle _            -> Nothing
                                            Select item         -> Just ( loopLooks item.id item.lookGroups )
                                            SelectToggle item   -> Just ( loopLooks item.id item.lookGroups )
                                            Multi _             -> Nothing
                                    )
                                    sc.items
                                ) ++ [ "}"
                                , "def Scope \"Materials\" {"
                                , usdMaterial matFactoryPrefix "UIPanel"            uiPanelMaterial
                                , usdMaterial matFactoryPrefix "UIToggleActive"     uiToggleActiveMaterial
                                , usdMaterial matFactoryPrefix "UIToggleInactive"   uiToggleInactiveMaterial
                                , usdMaterial matFactoryPrefix "UIOpenButton"       uiButtonOpenMaterial
                                , usdMaterial matFactoryPrefix "UICloseButton"      uiButtonCloseMaterial
                                , usdMaterial matFactoryPrefix "UIGroupPick"        uiGroupPickMaterial
                                , usdMaterial matFactoryPrefix "UIGroupActive"      uiGroupActiveMaterial
                                , usdMaterial matFactoryPrefix "UIRod"              uiRodMaterial
                                ]
                                ++( List.concatMap
                                    (\i ->
                                        let
                                            usdLookGroups itemId lgs =
                                                List.concatMap
                                                    (\group ->
                                                        List.map
                                                            (\look ->
                                                                let
                                                                    suffix = itemId ++ "_" ++ group.id ++ "_" ++ look.id
                                                                    allButOcclusion= List.filter (\f -> f.id /= "occlusion") look.material.inputs
                                                                    btnMaterial=
                                                                        Material look.material.id allButOcclusion
                                                                in
                                                                String.join "\n"
                                                                    [ usdMaterial
                                                                        matFactoryPrefix
                                                                        ("PrimitiveShapeAssetFactory_Object_"++ suffix)
                                                                        look.material
                                                                    , usdMaterial
                                                                        matFactoryPrefix
                                                                        ("LookBtn_" ++ suffix)
                                                                        btnMaterial
                                                                    ]
                                                            )
                                                            group.looks
                                                    )
                                                    lgs
                                        in
                                        case i of
                                            Static item ->
                                                [ usdMaterial
                                                    matFactoryPrefix
                                                    ("PrimitiveShapeAssetFactory_Object_"++ item.id ++ "__" ++ item.look.id)
                                                    item.look.material
                                                ]
                                            Toggle item ->
                                                [ usdMaterial
                                                    matFactoryPrefix
                                                    ("PrimitiveShapeAssetFactory_Object_"++ item.id ++ "__" ++ item.look.id)
                                                    item.look.material
                                                ]
                                            Select item ->
                                                usdLookGroups item.id item.lookGroups
                                            SelectToggle item ->
                                                usdLookGroups item.id item.lookGroups
                                            Multi item -> List.concat
                                                [ List.filterMap
                                                    (\p ->
                                                        case List.head <| List.filter (\l -> l.id == p.look) item.looks of
                                                            Nothing -> Nothing
                                                            Just look -> Just <|
                                                                usdMaterial
                                                                    matFactoryPrefix
                                                                    ("PrimitiveShapeAssetFactory_Object_" ++ item.id ++ "_" ++ p.product)
                                                                    look.material
                                                    )
                                                    item.products
                                                , List.map
                                                    (\look ->
                                                        usdMaterial
                                                            matFactoryPrefix
                                                            ("Look_" ++ item.id ++ "_" ++ look.id)
                                                            look.material
                                                    )
                                                    item.looks
                                                , List.map
                                                    (\form ->
                                                        usdMaterial
                                                            matFactoryPrefix
                                                            ("Form_" ++ item.id ++ "_" ++ form.id)
                                                            <| Material "pic"
                                                                [ UsdInput "diffuseColor" "(1,1,1)"
                                                                    (Just (defaultTx form.thumbnail 0))
                                                                ]
                                                    )
                                                    item.forms
                                                ]
                                    )
                                    sc.items
                                )
                                ++[ "}" ]
                                )
                            )
                    (startActions, behaviors) = 
                      List.unzip <|
                        ( behaviorsForCode scene ) ::
                        List.map
                            (\i ->
                                case i of
                                    Static _          -> ([], [])
                                    Toggle item       -> behaviorsForToggle scene item
                                    SelectToggle item -> behaviorsForSelectToggle scene item
                                    Select item       -> behaviorsForSelect scene item
                                    Multi item        -> behaviorsForMulti scene item
                            )
                            sc.items

                    matPath = "/Root/Scenes/" ++ scene
                    usda = String.join "\n"
                        (
                            [ usdHeader "TEST" sc.animMeta ""
                            , usdSceneStart scene sc.anchor
                            , usdMaterial matPath "UITextBody" uiTextBodyMaterial
                            , usdMaterial matPath "UITextActive" uiTextActiveMaterial
                            , usdMaterial matPath "UITextInactive" uiTextInactiveMaterial
                            , usdMaterial matPath "UIPanelMaterial" uiPanelMaterial
                            , usdMaterial matPath "UIPanelAccent" <| uiPanelAccent <| toLinearStr company.accentColor
                            , "def Xform \"Children\" {"
                            , objects
                            , codeLines
                            , String.join "\n" <| 
                                usdGroup "ConfigUI" Nothing origin <|
                                    List.concat
                                        [ usdGroup "Hint" Nothing
                                            { origin | translate= Just (0, 0.33, 0.75) }
                                            [ usdPlace (path++"ConfigUI/Children/Hint/Children/")
                                                "Bubble"
                                                True
                                                (Just ("</AssetFactories/Masters/Factories/UIHintBubble>"))
                                                origin
                                                defaultPhysics
                                            , usdText scene "UIHint" 
                                                { origin | translate= Just (0,0,0.011) }
                                                ("</Root/Scenes/"++scene++"/UITextBody>")
                                                { width= 0.2823834
                                                , height= 0.13238342
                                                , pointSize= 95.0
                                                , verticalAlignment= Middle, horizontalAlignment= Center
                                                }
                                                (langGet model.exportLang sc.translations.hint)
                                            ]
                                        , usdGroup "Hideable" Nothing origin <| gather.lines
                                        ]
                            , "}"
                            , "def Scope \"Behaviors\" {"
                            ]
                        ++( [ usdBehavior scene
                                { id= "PresentHint"
                                , exclusive= False
                                , triggers= [ SceneTransition "enter" ]
                                , actionRoot= Group
                                    { parallel= False, loop= False, performCount= 1
                                    , actions=
                                        if optimizeForMatrix
                                            then
                                                [ ( "HideAllButMatrix"
                                                , justHide
                                                    [ "</Root/Scenes/"++ scene ++"/Children/ConfigUI/Children/Hint>"
                                                    , "</Root/Scenes/"++ scene ++"/Children/Order>"
                                                    --, "</Root/Scenes/"++ scene ++"/Children/ConfigUI/Children/Hideable/Children/Order>"
                                                    ]
                                                )
                                                ]
                                            else
                                                [ ( "LookAtCamera"
                                                    , LookAtCamera
                                                        { affected= [ ("</Root/Scenes/"++scene++"/Children/ConfigUI/Children/Hint>") ]
                                                        , duration= 5.0
                                                        , front= (0,0,1)
                                                        , upVector= (0,0,0)
                                                        }
                                                    )
                                                , ( "Hide"
                                                , Visibility
                                                        { show= False, duration= 0.33
                                                        , affected= [ ("</Root/Scenes/"++scene++"/Children/ConfigUI/Children/Hint>") ] 
                                                        }
                                                )
                                                ]
                                    }
                                }
                            , usdBehavior scene
                                { id= "Start"
                                , exclusive= False
                                , triggers= [ SceneTransition "enter" ]
                                , actionRoot= Group 
                                    { parallel= True, loop= False, performCount= 1
                                    , actions= List.concat startActions 
                                    }
                                }
                            ]
                          )
                        ++( List.filterMap
                                (\i ->
                                    let
                                        startAnim item =
                                            case item.skelAnim of
                                                Nothing -> Nothing
                                                Just _ ->
                                                    let
                                                        bName= "StartAnim_"++ item.id
                                                        obj = "</Root/Scenes/"++ scene ++"/Children/Object_"++ item.id ++">"
                                                    in
                                                    Just <| usdBehavior scene
                                                        { id= bName
                                                        , exclusive= True
                                                        , triggers= [ TapGesture obj ]
                                                        , actionRoot= Group
                                                            { parallel= True, loop= False, performCount= 1
                                                            , actions= [ ( bName, StartAnimation { affected= [obj] }) ]
                                                            }
                                                        }
                                    in
                                    case i of
                                        SelectToggle item ->    startAnim item
                                        Select item ->          startAnim item
                                        Toggle item ->          startAnim item
                                        Static item ->          startAnim item
                                        Multi item ->           Nothing
                                )
                                sc.items
                          )
                        ++( [ usdBehavior scene
                                { id= "FaceToCamera"
                                , exclusive= False
                                , triggers= [ SceneTransition "enter" ]
                                , actionRoot=
                                    Group
                                        { parallel= True, loop= False, performCount= 1
                                        , actions= [
                                            ( "LookAtCamera"
                                            , LookAtCamera
                                                { affected= List.concat <|
                                                    List.filterMap
                                                        (\i ->
                                                            let
                                                                ref id = 
                                                                    "</Root/Scenes/"++scene++"/Children/ConfigUI/Children/Hideable/Children/Select_"++id++"/Children/Hideable>"
                                                            in
                                                            case i of
                                                                SelectToggle item ->
                                                                    Just [ ref item.id ]
                                                                Select item ->
                                                                    Just [ ref item.id ]
                                                                Multi item ->
                                                                    let
                                                                        uiPath= "</Root/Scenes/"++scene++"/Children/ConfigUI/Children/Hideable/Children/Multi_"++item.id++"/Children/Hideable/Children/"
                                                                    in
                                                                    Just
                                                                        [ uiPath ++ "Panel>"
                                                                        , uiPath ++ "Matrix>"
                                                                        ]
                                                                        
                                                                _ -> Nothing
                                                        )
                                                        sc.items
                                                , duration= 3000
                                                , front= (0,0,1)
                                                , upVector= (0,0,0)
                                                }
                                            )
                                          ]
                                        }
                                }
                            ]
                          )
                        ++( List.map (usdBehavior scene) <| List.concat behaviors )
                        ++[ "}\n}\n}\n}" -- "}\n}\n}\n"
                          , factories
                          , "}"
                          , usdXform ("_Root_Scenes_"++scene++"_Behaviors_Order_Show", sc.codeXform)
                          , usdXform ("_Root_Scenes_"++scene++"_Behaviors_Order_Hide", gather.uiXform)
                          , usdXform ("_Root_Scenes_"++scene++"_Behaviors_Order_Grow",   { origin | scale= Just (1,1, 3) } )
                          , usdXform ("_Root_Scenes_"++scene++"_Behaviors_Order_Shrink", { origin | scale= Just (1,1, 0.3333) } )
                          ] 
                        ++( List.map
                                (\i ->
                                    case i of
                                        Toggle item       -> xFormsForToggle sc.id item.id
                                        SelectToggle item -> xFormsForToggle sc.id item.id
                                        _  -> ""
                                )
                                sc.items
                            ) 
                        )
                in
                model |> withCmd (Outside.sendInfo (Outside.WriteFile { fileName="Test.usda", content=usda}))
            _ ->  ( model, Cmd.none )

        LoadTestDoc ->
            ( { model | scenes= [ Config testScene ] }, Cmd.none )

        LoadChairDoc ->
            ( { model | scenes= [ Config configScene ] }, Cmd.none )

        LoadInfoDoc ->
            ( { model | scenes= [ Info infoScene ] }, Cmd.none )

        NewDoc ->
            ( { model | scenes= [ Config emptyScene ] }, Cmd.none )

        OpenDoc ->
            model |> withCmd (Outside.sendInfo (Outside.AskForFile "Test.cfgxr"))

        SaveElm ->
            case model.scenes of
                [ Config scene ] ->
                    model |> withCmd
                        ( Outside.sendInfo
                            ( Outside.WriteFile 
                                { fileName="src/Intro/xProject.elm"
                                , content= elmForScene scene
                                }
                            )
                        )
                [ Info info ] ->
                    model |> withCmd
                        ( Outside.sendInfo
                            ( Outside.WriteFile 
                                { fileName="src/Intro/xProject.elm"
                                , content= elmForInfo info
                                }
                            )
                        )
                _ -> ( model, Cmd.none )

        SaveScene indent ->
            case model.scenes of
                [ Config scene ] ->
                    model |> withCmd 
                        ( Outside.sendInfo 
                            ( Outside.WriteFile 
                                { fileName="Test.cfgxr"
                                , content= Encode.encode indent <| configSceneEncoder scene
                                }
                            )
                        )
                [ Info info ] ->
                    model |> withCmd 
                        ( Outside.sendInfo 
                            ( Outside.WriteFile 
                                { fileName="Test.cfgxr"
                                , content= Encode.encode indent <| infoSceneEncoder info
                                }
                            )
                        )
                _ -> ( model, Cmd.none )

        --Convert ->
        --    model |> withCmd (Outside.sendInfo (Outside.Convert "Test.usda" ))

        Outside infoForElm ->
            case infoForElm of
                Outside.GotClipboard clipboard ->
                    --pasteToEditorAndClipboard model clipboard
                    ( model, Cmd.none )

                Outside.GotFileList fileList ->
                    --( { model | fileList = fileList }, Cmd.none )
                    ( model, Cmd.none )

                Outside.GotFile doc ->
                    {-
                    Helper.Load.loadDocument doc model
                      |> (\m -> {m | popupStatus = PopupClosed})
                      -- |> (\m -> m |> withCmd (Helper.Server.updateDocument m.fileStorageUrl m.document))
                      -- TODO: fix the above
                      |> withNoCmd
                    -}
                    --( { model | text = doc.content }, Cmd.none )
                    ( case Decode.decodeString configSceneDecoder doc.content of
                        Ok s ->
                            { model | scenes= [ Config s ] }
                        _ -> model
                    , Cmd.none 
                    )

        OutsideInfo msg_ ->
           model
             |> withCmd (Outside.sendInfo msg_)

        LogErr _ ->
            ( model, Cmd.none )

        ChangeInfoScene infoId change v ->
            ( changeInfoScene infoId
                (\info ->
                    change info v
                )
            , Cmd.none
            )
 
        ChangeInfoPanel infoId panelId change str ->
            ( changeInfoPanel infoId panelId
                (\panel ->
                    change panel str
                )
            , Cmd.none
            )

        ChangePanelStartOpen infoId panelId b ->
            (changeInfoPanel infoId panelId
                (\panel ->
                    { panel | startOpen= b }
                )
            , Cmd.none
            )

        ChangePage infoId panelId pageId change str ->
            (changePage infoId panelId pageId
                (\page ->
                    change page str
                )
            , Cmd.none
            )

        ChangeConfigScene sceneId change v ->
            ( changeConfigScene sceneId
                (\scene ->
                    change scene v
                )
            , Cmd.none
            )       

        ChangeSelectToggleItem sceneId itemId change str ->
            ( changeConfigScene sceneId
                (\scene ->
                   { scene | items= List.map
                        (\item ->
                            case item of
                                SelectToggle i -> if i.id /= itemId then item else
                                    SelectToggle <| change i str
                                _ -> item
                        ) 
                        scene.items
                    }
                )
            , Cmd.none
            )

        ChangeSelectItem sceneId itemId change str ->
            ( changeConfigScene sceneId
                (\scene ->
                    { scene | items= List.map
                        (\item ->
                            case item of
                                Select i -> if i.id /= itemId then item else
                                    Select <| change i str
                                _ -> item
                        ) 
                        scene.items
                    }
                )
            , Cmd.none
            )

        ChangeLook sceneId itemIdx grpId lookId f param ->
            ( changeConfigScene sceneId
                (\scene ->
                    { scene | items=
                        List.map
                            (\(idx,item) -> 
                                if idx == itemIdx 
                                    then
                                        case item of
                                            Static i ->
                                                Static 
                                                    { i | look= f param i.look }
                                            Toggle i ->
                                                Toggle
                                                    { i | look= f param i.look }
                                            SelectToggle i ->
                                                SelectToggle
                                                    { i | lookGroups=
                                                        changeLookInGroups grpId lookId param f i.lookGroups
                                                    }
                                            Select i ->
                                                Select
                                                    { i | lookGroups=
                                                        changeLookInGroups grpId lookId param f i.lookGroups
                                                    }
                                            Multi i ->
                                                Multi
                                                    { i | looks= List.map (\look -> f param look) i.looks }
                                    else item
                            )
                            (List.indexedMap Tuple.pair scene.items)
                    }
                )
            , Cmd.none
            )

        ChangePanelTitle sceneId panelIdx lang str ->
            ( changeInfoScene sceneId
                (\scene ->
                    { scene | panels=
                        List.map
                            (\(idx,panel) -> 
                                if idx == panelIdx 
                                    then { panel | title= Dict.insert lang (fixDescr str) panel.title }
                                    else panel
                            )
                            (List.indexedMap Tuple.pair scene.panels)
                    }
                )
            , Cmd.none
            )

        ChangePanelSubTitle sceneId panelIdx lang str ->
            ( changeInfoScene sceneId
                (\scene ->
                    { scene | panels=
                        List.map
                            (\(idx,panel) -> 
                                if idx == panelIdx 
                                    then { panel | subTitle= Dict.insert lang (fixDescr str) panel.subTitle }
                                    else panel
                            )
                            (List.indexedMap Tuple.pair scene.panels)
                    }
                )
            , Cmd.none
            )

        ChangeItemDescr sceneId itemIdx lang str ->
            let
                descr= fixDescr str
            in
            ( changeConfigScene sceneId
                (\scene ->
                    { scene | items=
                        List.map
                            (\(idx,item) -> 
                                if idx == itemIdx 
                                    then
                                        case item of
                                            Toggle i ->       Toggle       { i | description= Dict.insert lang descr i.description }
                                            SelectToggle i -> SelectToggle { i | description= Dict.insert lang descr i.description }
                                            Select i ->       Select       { i | description= Dict.insert lang descr i.description }
                                            _ -> item
                                    else item
                            )
                            (List.indexedMap Tuple.pair scene.items)
                    }
                )
            , Cmd.none
            )

        ChangeMaterialId sceneId itemIdx grpId lookId str ->
            let
                newMat : Material -> String -> Material
                newMat= \mat newId ->
                    { mat | id= sanitize "matID" newId }
            in
            ( changeConfigScene sceneId
                (\scene ->
                    { scene | items=
                        List.map
                            (\(idx,item) -> 
                                if idx == itemIdx 
                                    then
                                        case item of
                                            Static i -> 
                                                Static
                                                    { i | look= 
                                                        (\id l -> { l | material= newMat l.material id })
                                                        str 
                                                        i.look
                                                    }
                                            Toggle i -> 
                                                Toggle
                                                    { i | look= 
                                                        (\id l -> { l | material= newMat l.material id })
                                                        str 
                                                        i.look
                                                    }
                                            SelectToggle i ->
                                                SelectToggle
                                                    { i | lookGroups=
                                                        changeLookInGroups grpId lookId str
                                                            (\id l-> { l | material= newMat l.material id }) 
                                                            i.lookGroups
                                                    }
                                            Select i ->
                                                Select
                                                    { i | lookGroups=
                                                        changeLookInGroups grpId lookId str
                                                            (\id l-> { l | material= newMat l.material id }) 
                                                            i.lookGroups
                                                    }
                                            Multi i ->
                                                Multi
                                                    { i | looks= List.map
                                                        (\look ->
                                                            (\id l -> { l | material= newMat l.material id })
                                                            str
                                                            look
                                                        )
                                                        i.looks
                                                    }
                                    else item
                            )
                            (List.indexedMap Tuple.pair scene.items)
                    }
                )
            , Cmd.none
            )

        _ ->
            case model.scenes of
                [ Config scene ] ->
                    let
                        newScene = case msg of
                            DeleteLook itemIdx grpId lookId ->  
                                { scene | items=
                                    List.map
                                        (\(idx,item) -> 
                                            if idx == itemIdx 
                                                then
                                                    case item of
                                                        SelectToggle i ->
                                                            SelectToggle
                                                                { i | lookGroups= 
                                                                    List.map
                                                                        (\group ->
                                                                            if group.id == grpId
                                                                                then
                                                                                    { group | looks= List.filter (\look -> look.id /= lookId) group.looks }
                                                                                else
                                                                                    group
                                                                        ) 
                                                                        i.lookGroups
                                                                }
                                                        Select i ->
                                                            Select
                                                               { i | lookGroups= 
                                                                    List.map
                                                                        (\group ->
                                                                            if group.id == grpId
                                                                                then
                                                                                    { group | looks= List.filter (\look -> look.id /= lookId) group.looks }
                                                                                else
                                                                                    group
                                                                        ) 
                                                                        i.lookGroups
                                                                }
                                                        Multi i ->
                                                            Multi { i | looks= List.filter (\look -> look.id /= lookId) i.looks }
                                                        _ -> item
                                                else item
                                        )
                                        (List.indexedMap Tuple.pair scene.items)
                                }
                                
                            ChangeRALCode newId ->
                                { scene | ralId= newId }

                            ChangeRALSuffix newSuffix ->
                                { scene | ralSuffix= newSuffix }

                            ChangeGrpName itemIdx grpId str ->
                                let
                                    newId= sanitize "grpID" str
                                in
                                { scene | items=
                                    List.map
                                        (\(idx,item) -> 
                                            if idx == itemIdx 
                                                then
                                                    case item of
                                                        SelectToggle i ->
                                                            SelectToggle
                                                                { i | lookGroups= 
                                                                    List.map
                                                                        (\group->
                                                                            if group.id == grpId
                                                                                then { group | id = newId }
                                                                                else group
                                                                        ) 
                                                                        i.lookGroups
                                                                }
                                                        Select i ->
                                                            Select
                                                                { i | lookGroups= 
                                                                    List.map
                                                                        (\group->
                                                                            if group.id == grpId
                                                                                then { group | id = newId }
                                                                                else group
                                                                        ) 
                                                                        i.lookGroups
                                                                }
                                                        _ -> item
                                                else item
                                        )
                                        (List.indexedMap Tuple.pair scene.items)
                                }

                            ChangeGrpDescr itemIdx grpId lang str ->
                                let
                                    descr= fixDescr str
                                in
                                { scene | items=
                                    List.map
                                        (\(idx,item) -> 
                                            if idx == itemIdx 
                                                then
                                                    case item of
                                                        SelectToggle i ->
                                                            SelectToggle
                                                                { i | lookGroups= 
                                                                    List.map
                                                                        (\group->
                                                                            if group.id == grpId
                                                                                then { group | description= Dict.insert lang descr group.description }
                                                                                else group
                                                                        ) 
                                                                        i.lookGroups
                                                                }
                                                        Select i ->
                                                            Select
                                                                { i | lookGroups= 
                                                                    List.map
                                                                        (\group->
                                                                            if group.id == grpId
                                                                                then { group | description= Dict.insert lang descr group.description }
                                                                                else group
                                                                        ) 
                                                                        i.lookGroups
                                                                }
                                                        _ -> item
                                                else item
                                        )
                                        (List.indexedMap Tuple.pair scene.items)
                                }

                            DeleteGrpById itemIdx grpId ->  
                                { scene | items=
                                    List.map
                                        (\(idx,item) -> 
                                            if idx == itemIdx 
                                                then
                                                    case item of
                                                        SelectToggle i -> SelectToggle  { i | lookGroups= List.filter (\look -> look.id /= grpId) i.lookGroups }
                                                        Select i ->       Select        { i | lookGroups= List.filter (\look -> look.id /= grpId) i.lookGroups }
                                                        _ -> item
                                                else item
                                        )
                                        (List.indexedMap Tuple.pair scene.items)
                                }

                            AddLookToGrp itemIdx grpId ->
                                { scene | items=
                                    List.map
                                        (\(idx,item) -> 
                                            if idx == itemIdx 
                                                then
                                                    case item of
                                                        SelectToggle i ->
                                                            SelectToggle
                                                                { i | lookGroups= 
                                                                    List.map
                                                                        (\group ->
                                                                            if group.id == grpId
                                                                                then { group | looks= newLook :: group.looks }
                                                                                else group
                                                                        ) 
                                                                        i.lookGroups
                                                                }
                                                        Select i ->
                                                            Select
                                                                { i | lookGroups= 
                                                                    List.map
                                                                        (\group ->
                                                                            if group.id == grpId
                                                                                then { group | looks= newLook :: group.looks }
                                                                                else group
                                                                        ) 
                                                                        i.lookGroups
                                                                }
                                                        _ -> item
                                                else item
                                        )
                                        (List.indexedMap Tuple.pair scene.items)
                                }

                            AddRALToGrp itemIdx grpId ralId ralSuffix ->
                                { scene | items=
                                    List.map
                                        (\(idx,item) -> 
                                            if idx == itemIdx 
                                                then
                                                    case item of
                                                        SelectToggle i ->
                                                            SelectToggle
                                                                { i | lookGroups= 
                                                                    List.map
                                                                        (\group ->
                                                                            if group.id == grpId
                                                                                then { group | looks= (ralLook "99" ralId (Just ralId) [] ralSuffix):: group.looks }
                                                                                else group
                                                                        ) 
                                                                        i.lookGroups
                                                                }
                                                        Select i ->
                                                            Select
                                                                { i | lookGroups= 
                                                                    List.map
                                                                        (\group ->
                                                                            if group.id == grpId
                                                                                then { group | looks= newLook :: group.looks }
                                                                                else group
                                                                        ) 
                                                                        i.lookGroups
                                                                }
                                                        Multi i -> Multi { i | looks= newLook :: i.looks }
                                                        _ -> item
                                                else item
                                        )
                                        (List.indexedMap Tuple.pair scene.items)
                                }

                            AddLookGroupToItemIdx itemIdx ->
                                { scene | items=
                                    List.map
                                        (\(idx,item) -> 
                                            if idx == itemIdx 
                                                then
                                                    case item of
                                                        SelectToggle i ->
                                                            SelectToggle 
                                                                { i | lookGroups= 
                                                                    ( LookGroup 
                                                                        "GruppeX" 
                                                                        (Dict.fromList [ ("en", "New material Group"), ("de", "Neue Materialgruppe") ])
                                                                        [ newLook ]
                                                                    ) 
                                                                    :: i.lookGroups
                                                                }
                                                        Select i ->
                                                            Select 
                                                                { i| lookGroups= 
                                                                    ( LookGroup
                                                                        "GruppeX"
                                                                        (Dict.fromList [ ("en", "New material Group"), ("de", "Neue Materialgruppe") ])
                                                                        [ newLook ]
                                                                    ) 
                                                                    :: i.lookGroups
                                                                }
                                                        _ -> item
                                                else item
                                        )
                                        (List.indexedMap Tuple.pair scene.items)
                                }

                            ChangeItemId itemIdx str ->  
                                let
                                    newId = sanitize "itemID" str
                                in
                                { scene | items=
                                    List.map
                                        (\(idx,item) -> 
                                            if idx == itemIdx 
                                                then
                                                    case item of
                                                        Static i ->       Static       { i | id= newId }
                                                        Toggle i ->       Toggle       { i | id= newId }
                                                        SelectToggle i -> SelectToggle { i | id= newId }
                                                        Select i ->       Select       { i | id= newId }
                                                        Multi i ->        Multi        { i | id= newId }
                                                else item
                                        )
                                        (List.indexedMap Tuple.pair scene.items)
                                }

                            DeleteItemByIdx itemIdx ->
                                { scene | items= 
                                    List.filterMap 
                                        (\(idx,item) -> if idx == itemIdx then Nothing else Just item)
                                        (List.indexedMap Tuple.pair scene.items)
                                }

                            _ -> scene

                    in
                    ( { model | scenes= [ Config newScene ] }, Cmd.none )

                [ Info info ] ->
                    let
                        newInfo =
                          case msg of
                            DeletePanelByIdx panelIdx ->
                                { info | panels= 
                                    List.filterMap 
                                        (\(idx,item) -> if idx == panelIdx then Nothing else Just item)
                                        (List.indexedMap Tuple.pair info.panels)
                                }
                            _ -> info
                    in
                    ( { model | scenes= [ Info newInfo ] }, Cmd.none )

                _ -> ( model, Cmd.none )


getDocumentList : Cmd msg
getDocumentList =
    Outside.sendInfo Outside.AskForFileList


expandBtn : String -> Msg -> Element Msg
expandBtn name msg =
    row [ width fill, spacing 20, paddingXY 0 10 ]
        [ Input.button btnLook { onPress= Just msg, label=text "Expand" }
        , text name
        ]

collapseBtn : Msg -> Element Msg
collapseBtn msg =
    Input.button btnLook
        { onPress= Just msg
        , label= text "Collapse"
        }

viewLook : String -> Dict String Bool -> Bool -> Int -> String -> Look -> Element Msg
viewLook scene languages showTexts itemIdx grpId look =
    if not look.expandedView then
        expandBtn look.id (ChangeLook scene itemIdx grpId look.id (\_ -> expandLook True) "")
    else
      column [ width fill, spacing 5, paddingXY 0 10 ]
        [ if showTexts then
            row [ width fill, spacing 5, paddingXY 0 10 ]
                [ Input.text [ width (fillPortion 2) ]
                    { onChange= 
                        ChangeLook scene itemIdx grpId look.id 
                            (\str l -> { l| id= sanitize "lookId" str } )
                    , label= Input.labelAbove [] <| text "ID"
                    , text= look.id
                    , placeholder= Nothing
                    }
                , viewTranslations 
                    (\l ->
                        ChangeLook scene itemIdx grpId look.id 
                            (\descr lo -> { lo | description= Dict.insert l descr lo.description } )
                    ) 
                    "Look Description" 
                    <| reduce languages look.description
                , Input.button (Element.alignRight :: btnLook)
                    { onPress= Just (DeleteLook itemIdx grpId look.id)
                    , label= text "Delete"
                    }
                ]
          else Element.none
        , row [ width fill, spacing 5, paddingXY 0 10 ]
            [ collapseBtn (ChangeLook scene itemIdx grpId look.id (\_ -> expandLook False) "")
            , Input.text [ width fill ]
                { onChange= ChangeMaterialId scene itemIdx grpId look.id
                , text= look.material.id
                , placeholder= Nothing
                , label= Input.labelAbove [] <| text "Material ID"
                }
            , Input.button btnLook
                { onPress= Just <| 
                    ChangeLook scene itemIdx grpId look.id 
                        (\_ -> addInput ( UsdInput "xColor" "(0.5, 0.5, 0.5)" Nothing )) 
                        ""
                , label= text "Add Color"
                }
            , Input.button btnLook
                { onPress= Just <| 
                    ChangeLook scene itemIdx grpId look.id 
                        (\_ -> addInput (UsdInput "opacity" "1.0" Nothing )) 
                        ""
                , label= text "Add Float"
                }
            ]
        , Element.table [ width fill, spacing 5 ]
            { data= look.material.inputs
            , columns=
                [ { header= text "Channel"
                  , width= fillPortion 2
                  , view= \i ->
                        Input.text [ width fill ] <|
                            { onChange= NoOpStr
                            , text= i.id
                            , label= labelHidden "FloatName"
                            , placeholder= Nothing
                            }
                  }
                , { header= text "Default"
                  , width= fillPortion 5
                  , view= \i ->
                              Input.text [ width fill ]
                                { onChange= ChangeLook scene itemIdx grpId look.id (changeValues i.id)
                                , text= i.values
                                , placeholder= Nothing
                                , label= labelHidden "Default"
                                }
                  }
                , { header= text "Texture"
                  , width= fillPortion 6
                  , view= \i -> 
                        viewTexture scene itemIdx grpId look.id i.id i.texture
                  }
                ]
            }
        ]


viewTexture : String -> Int -> String -> String -> String -> Maybe Texture -> Element Msg
viewTexture scene itemIdx grpId lookId inputId mbTex =
    row [ width fill, spacing 10 ] <|
        case mbTex of
            Nothing ->
                [ Input.button btnLook
                    { onPress= Just <| 
                        ChangeLook scene itemIdx grpId lookId 
                            (\_ -> 
                                changeTex 
                                    (Just (scene++"/"++inputId++"_"++String.fromInt itemIdx++"_"++ grpId ++"_" ++lookId ++".png", 0))
                                    inputId
                            )
                            ""
                    , label= text "Use Texture"
                    }
                , Input.button btnLook
                    { onPress= Just <|
                        ChangeLook scene itemIdx grpId lookId
                            (\_ l -> 
                                { l | material= Material l.material.id <| List.filter (\i -> i.id /= inputId) l.material.inputs }
                            )
                            inputId
                    , label= text "Remove Input"
                    }
                ]
            Just tex ->
                [ Input.text [ width <| fillPortion 3 ]
                    { onChange= NoOpStr
                    , label= labelHidden "Path"
                    , placeholder= Nothing
                    , text= tex.file
                    }
                , Input.button (Element.alignRight :: btnLook)
                    { onPress= Just <| 
                        ChangeLook scene itemIdx grpId lookId 
                            (\_ -> changeTex Nothing inputId) 
                            ""
                    , label= text "No Texture"
                    }
                ]



viewGroup : String -> Dict String Bool -> String -> String -> Int -> LookGroup -> Element Msg
viewGroup scene languages ralId ralSuffix itemIdx group =
    column [ width fill, spacing 5, paddingXY 0 10 ]
        [ row [ width fill, spacing 5, paddingXY 0 10 ]
            [ Input.text []
                { onChange= ChangeGrpName itemIdx group.id
                , label= Input.labelAbove [] <| text "Material Group"
                , placeholder= Nothing
                , text= group.id
                }
            , viewTranslations (\l-> ChangeGrpDescr itemIdx group.id l) "Group Description" <| reduce languages group.description
            , Input.button (Element.alignRight :: btnLook)
                { onPress= Just (DeleteGrpById itemIdx group.id)
                , label= text "Delete"
                }
            ]
        , column [ width fill, spacing 5, paddingXY 0 10 ] <|
            List.map (viewLook scene languages True itemIdx group.id) group.looks
        , row [ width fill, spacing 5 ]
            [ Input.button btnLook
                { onPress= Just (AddLookToGrp itemIdx group.id)
                , label= text "Add look"
                }
            , Input.text [ width (px 120)]
                    { onChange= ChangeRALCode
                    , label= Input.labelAbove [] <| text "RAL Code"
                    , placeholder= Nothing
                    , text= ralId
                    }
            , Input.text [ width (px 120)]
                    { onChange= ChangeRALSuffix
                    , label= Input.labelAbove [] <| text "RAL Suffix"
                    , placeholder= Nothing
                    , text= ralSuffix
                    }
            , Input.button btnLook
                { onPress= Just (AddRALToGrp itemIdx group.id ralId ralSuffix)
                , label= text <| "Add K5 RAL "
                }
            ]
        ]


viewTranslations : (String -> String -> Msg) -> String -> LanguageString -> Element Msg
viewTranslations toMsg hiddenStr ls =
    column [ width fill, spacing 5 ] <|
        List.map
            (\(l,d) ->
                row [ width fill, spacing 5 ]
                    [ el [] <| text l
                    , Input.text []
                        { onChange= toMsg l
                        , text= d
                        , placeholder= Nothing
                        , label= Input.labelHidden hiddenStr
                        }
                    ]
            )
            <| Dict.toList ls


--viewXform : String -> String -> Xform -> (Xform -> Msg) -> Element Msg
--viewXform sceneId itemId xf f =
--    let
--        translate = Maybe.withDefault (0.0, 0.0, 0.0) xf.translate
--        orient = Maybe.withDefault [ 1.0, 0.0, 0.0, 0.0 ] xf.orient
--        scale = Maybe.withDefault ( 1.0, 1.0, 1.0 ) xf.scale
--    in
--    --Element.none
--     
--    column [ width fill, spacing 5 ]
--        [ row [ width fill, spacing 5 ]
--            [ text "scale"
--            , Input.text []
--                { placeholder= Nothing
--                , label= Input.labelAbove [] <| text "X"
--                , onChange (f )
--                }
--            ]
--        ]

viewPanel : String -> Dict String Bool -> (Int, Panel) -> Element Msg
viewPanel scene languages (idx, panel) =
    let
        nextIdx= List.length panel.pages
        emptyPage = 
            { id="Page" ++ String.fromInt nextIdx
            , imagePath= ""
            , imageWidth= 1
            , imageHeight= 1 
            , bodyText= Dict.fromList [ ("de","") ]
            , expandedView= True
            }
    in
    column [ width fill, spacing 5, paddingXY 10 10, Border.glow (gray 50) 2.5 ] <|
        [ row [ width fill, spacing 5, paddingXY 0 10 ]
            [ text "InfoPanel"
            , Input.text []
                { onChange= ChangeInfoPanel scene panel.id (\si v -> { si | id= v } )
                , text= panel.id
                , placeholder= Nothing
                , label= Input.labelAbove [] <| text "ID"
                }
            , Input.checkbox []
                { onChange = ChangePanelStartOpen scene panel.id
                , icon = Input.defaultCheckbox
                , checked = panel.startOpen
                , label = Input.labelAbove [] <| text "startOpen"
                }
            , Input.text []
                { onChange= ChangeInfoPanel scene panel.id (\si v -> { si | startIdx= Maybe.withDefault 0 <| String.toInt v } )
                , text= String.fromInt panel.startIdx
                , placeholder= Nothing
                , label= Input.labelAbove [] <| text "startIdx"
                }
            , Input.button (Element.alignRight :: btnLook)
                { onPress= Just (DeletePanelByIdx idx)
                , label= text "Delete"
                }
            ]
        , row [ width fill, spacing 5, paddingXY 0 10 ]
            [ viewTranslations (\l -> ChangePanelTitle scene idx l) "Panel Title" <| reduce languages panel.title
            , viewTranslations (\l -> ChangePanelSubTitle scene idx l) "Panel Subtitle" <| reduce languages panel.subTitle
            ]
        , Input.button btnLook
            { onPress= Just (ChangeInfoPanel scene panel.id (\si v -> { si | pages= emptyPage :: si.pages } ) "")
            , label= text "Add Page"
            }
        , column [ width fill ]
            ( List.map
                (\idxPage ->
                    viewPage scene languages panel.id idxPage
                )
                <| List.indexedMap Tuple.pair panel.pages
            )
        , Input.button btnLook
            { onPress= Just (ChangeInfoPanel scene panel.id (\si v -> { si | pages= si.pages ++ [emptyPage]} ) "")
            , label= text "Add Page"
            }
        ]

viewPage : String -> Dict String Bool -> String -> (Int, Page) -> Element Msg
viewPage scene langauges panelId (idx, page) =
    column [ width fill, spacing 5, paddingXY 10 10 ]
        [ row [ width fill, spacing 5 ]
            [ Input.text []
                { onChange= ChangePage scene panelId page.id (\p v -> { p | id= v } )
                , text= page.id
                , placeholder= Nothing
                , label= Input.labelAbove [] <| text "ID"
                }
            , Input.text []
                { onChange= ChangePage scene panelId page.id (\p v -> { p | imagePath= v } )
                , text= page.imagePath
                , placeholder= Nothing
                , label= Input.labelAbove [] <| text "imagePath"
                }
            , Input.text []
                { onChange= ChangePage scene panelId page.id (\p v -> { p | imageWidth= Maybe.withDefault 1 <| String.toInt v } )
                , text= String.fromInt page.imageWidth
                , placeholder= Nothing
                , label= Input.labelAbove [] <| text "width (px)"
                }
            , Input.text []
                { onChange= ChangePage scene panelId page.id (\p v -> { p | imageHeight= Maybe.withDefault 1 <| String.toInt v } )
                , text= String.fromInt page.imageHeight
                , placeholder= Nothing
                , label= Input.labelAbove [] <| text "height (px)"
                }
            ]
        --, viewTranslations  (\l -> ChangePageText scene panelId idx l) "Body Text" <| reduce languages page.bodyText
        ]

{-
    { id: String
    , imagePath: String
    , imageWidth: Int
    , imageHeight: Int 
    , bodyText: LanguageString
    , expandedView: Bool
    }
-}

viewItem : String -> Dict String Bool -> String -> String -> (Int, Item) -> Element Msg
viewItem scene languages ralId ralSuffix (idx, i) =
    let
        common : String -> String -> Maybe ( Bool, Msg, Msg ) -> LanguageString -> Element Msg
        common type_ id ex description= 
            case ex of
                Just (expanded, msgExpand, msgCollapse) ->
                    if expanded
                        then
                            row [ width fill, spacing 5, paddingXY 0 10 ]
                                [ column [spacing 5, paddingXY 0 10 ]
                                    [ text type_
                                    , collapseBtn msgCollapse
                                    ]
                                , Input.text []
                                    { onChange= ChangeItemId idx
                                    , text= id
                                    , placeholder= Nothing
                                    , label= Input.labelAbove [] <| text "ID"
                                    }
                                , viewTranslations (\l -> ChangeItemDescr scene idx l) "Item Description" <| reduce languages description
                                , Input.button (Element.alignRight :: btnLook)
                                    { onPress= Just (DeleteItemByIdx idx)
                                    , label= text "Delete"
                                    }
                                ]
                        else
                            column [ spacing 5, paddingXY 0 10 ]
                                [ text type_
                                , expandBtn id msgExpand
                                ]
                _ ->
                    row [ width fill, spacing 5, paddingXY 0 10 ]
                        [ text type_
                        , Input.text []
                            { onChange= ChangeItemId idx
                            , text= id
                            , placeholder= Nothing
                            , label= Input.labelAbove [] <| text "ID"
                            }
                        , viewTranslations (\l-> ChangeItemDescr scene idx l) "Item Description" <| reduce languages description
                        , Input.button (Element.alignRight :: btnLook)
                            { onPress= Just (DeleteItemByIdx idx)
                            , label= text "Delete"
                            }
                        ]
    in
    case i of
        Multi item -> -- TODO Multi
            column [ width fill, spacing 5, paddingXY 10 10, Border.glow (gray 50) 2.5 ] <|
                [ row [ width fill, spacing 5, paddingXY 0 10 ]
                    [ text "Multi"
                    , Input.text []
                        { onChange= ChangeItemId idx
                        , text= item.id
                        , placeholder= Nothing
                        , label= Input.labelAbove [] <| text "ID"
                        }
                    , Input.button (Element.alignRight :: btnLook)
                        { onPress= Just (DeleteItemByIdx idx)
                        , label= text "Delete"
                        }
                    ]
                ]

        Static item ->
            column [ width fill, spacing 5, paddingXY 10 10, Border.glow (gray 50) 2.5 ] <|
                [ row [ width fill, spacing 5, paddingXY 0 10 ]
                    [ text "Static"
                    , Input.text []
                        { onChange= ChangeItemId idx
                        , text= item.id
                        , placeholder= Nothing
                        , label= Input.labelAbove [] <| text "ID"
                        }
                    , Input.button (Element.alignRight :: btnLook)
                        { onPress= Just (DeleteItemByIdx idx)
                        , label= text "Delete"
                        }
                    ]
                , viewLook scene languages False idx "" item.look
                ]

        Toggle item ->
            column [ width fill, spacing 5, paddingXY 10 10, Border.glow (gray 50) 2.5 ] <|
                [ common "Toggle" item.id 
                    Nothing
                    item.description
                , viewLook scene languages False idx "" item.look
                ]

        SelectToggle item ->
            column [ width fill, spacing 5, paddingXY 10 10, Border.glow (gray 50) 2.5 ] <|
                [ common "SelectToggle" item.id 
                    ( Just 
                        ( item.expandedView
                        , (ChangeSelectToggleItem scene item.id (\si _ -> { si | expandedView= True  } ) "")
                        , (ChangeSelectToggleItem scene item.id (\si _ -> { si | expandedView= False } ) "")
                        )
                    )
                    item.description
                --, viewXform scene item.id (ChangeSelectToggleItem scene item.id (\si _ -> si) ) item.panelXform
                , showIf item.expandedView <|
                        column [ width fill ] <|
                            ( Input.button btnLook
                                { onPress= Just (AddLookGroupToItemIdx idx)
                                , label= text "Add group"
                                }) ::
                            List.map (viewGroup scene languages ralId ralSuffix idx) item.lookGroups
                ]

        Select item ->
            column [ width fill, spacing 5, paddingXY 10 10, Border.glow (gray 50) 2.5 ] <|
                [ common "Select" item.id
                    ( Just 
                        ( item.expandedView
                        , (ChangeSelectItem scene item.id (\si _ -> { si | expandedView= True  } ) "")
                        , (ChangeSelectItem scene item.id (\si _ -> { si | expandedView= False } ) "")
                        )
                    )
                    item.description
                --, viewXform scene item.id (ChangeSelectItem scene item.id (\si _ -> si) ) item.panelXform
                , showIf item.expandedView <|
                        column [ width fill ] <|
                            ( Input.button btnLook
                                { onPress= Just (AddLookGroupToItemIdx idx)
                                , label= text "Add group"
                                }) ::
                            List.map (viewGroup scene languages ralId ralSuffix idx) item.lookGroups
                ]


view : Model -> Html Msg
view model =
    Element.layoutWith { options = [ Element.focusStyle myFocusStyle ] }
        [ Background.color <| gray 155
        , Font.color <| gray 55
        , Font.size 14
        ]
        ( case model.scenes of
            [ MainMenu ] ->
                column 
                    [ width fill, height fill, spacing 5, paddingXY 10 10 ]
                    [ row [ width fill, spacing 5, paddingXY 0 10 ]
                        [ Input.button btnLook
                            { label= text "Test"
                            , onPress= Just LoadTestDoc
                            }
                        , Input.button btnLook
                            { label= text "Config"
                            , onPress= Just LoadChairDoc
                            }
                        , Input.button btnLook
                            { label= text "Info"
                            , onPress= Just LoadInfoDoc
                            }
                        , Input.button btnLook
                            { label= text "New"
                            , onPress= Just NewDoc
                            }
                        , Input.button btnLook
                            { label= text "Open"
                            , onPress= Just OpenDoc
                            }
                        ]
                    ]
            [ Info info ] ->
                column
                    [ width fill, height fill, spacing 5, paddingXY 10 10, scrollbarY ]
                    [ column [ width fill, spacing 25 ]
                        [ row [ width fill, spacing 10 ]
                            [ Input.button btnLook
                                { label= text "Export"
                                , onPress= Just ExportScene
                                }
                            , Input.button btnLook
                                { label= text "Save compact"
                                , onPress= Just (SaveScene 0)
                                }
                            , Input.button btnLook
                                { label= text "Save"
                                , onPress= Just (SaveScene 4)
                                }
                            , Input.button btnLook
                                { label= text "Save Elm"
                                , onPress= Just SaveElm
                                }
                            , row [ spacing 5, alignRight ] 
                                [ wrappedRow [ width fill, spacing 5 ] <| List.map
                                        (\(l,b)->
                                            Input.button 
                                                ( ( if l == model.exportLang then Border.color <| gray 0
                                                        else if b
                                                            then Border.color <| gray 200
                                                            else Border.color <| gray 240
                                                  ) :: (Border.width 2):: btnLook
                                                )
                                                { onPress= Just (ToggleLanguage l)
                                                , label= text l
                                                }
                                        )
                                        (Dict.toList model.languages)
                                , Input.text [ width (px 45)]
                                    { onChange= ChangeLanguage
                                    , label= Input.labelHidden "new language to add"
                                    , placeholder= Nothing
                                    , text= model.newLang
                                    }
                                , Input.button btnLook
                                    { onPress= Just AddLanguage
                                    , label= text "Add Lang"
                                    }
                                , Input.button btnLook
                                    { onPress= Just SetExportLanguage
                                    , label= text "Set Export" 
                                    }
                                ]
                            ]
                        ]
                    , if info.expandedView 
                        then
                            wrappedRow [ width fill, spacing 5 ] 
                                [ collapseBtn (ChangeInfoScene info.id (\sc _-> { sc | expandedView= False }) "")
                                , column [ width fill, spacing 5 ] 
                                    []
                                ]
                        else expandBtn info.id (ChangeInfoScene info.id (\sc _-> { sc | expandedView= True }) "")
                    , if info.expandedView then
                        column [ width fill, height fill, spacing 5, paddingXY 0 10 ] <|
                            List.map
                                (viewPanel info.id model.languages)
                                <| List.indexedMap Tuple.pair info.panels
                        else Element.none
                    ]
            [ Config scene ] ->
                column
                    [ width fill, height fill, spacing 5, paddingXY 10 10, scrollbarY ]
                    [ column [ width fill, spacing 25 ]
                        [ row [ width fill, spacing 10 ]
                            [ Input.button btnLook
                                { label= text "Export"
                                , onPress= Just ExportScene
                                }
                            , Input.button btnLook
                                { label= text "Save compact"
                                , onPress= Just (SaveScene 0)
                                }
                            , Input.button btnLook
                                { label= text "Save"
                                , onPress= Just (SaveScene 4)
                                }
                            , Input.button btnLook
                                { label= text "Save Elm"
                                , onPress= Just SaveElm
                                }
                            , row [ spacing 5, alignRight ] 
                                [ wrappedRow [ width fill, spacing 5 ] <| List.map
                                        (\(l,b)->
                                            Input.button 
                                                ( ( if l == model.exportLang then Border.color <| gray 0
                                                        else if b
                                                            then Border.color <| gray 200
                                                            else Border.color <| gray 240
                                                  ) :: (Border.width 2):: btnLook
                                                )
                                                { onPress= Just (ToggleLanguage l)
                                                , label= text l
                                                }
                                        )
                                        (Dict.toList model.languages)
                                , Input.text [ width (px 45)]
                                    { onChange= ChangeLanguage
                                    , label= Input.labelHidden "new language to add"
                                    , placeholder= Nothing
                                    , text= model.newLang
                                    }
                                , Input.button btnLook
                                    { onPress= Just AddLanguage
                                    , label= text "Add Lang"
                                    }
                                , Input.button btnLook
                                    { onPress= Just SetExportLanguage
                                    , label= text "Set Export" 
                                    }
                                ]
                            ]
                        ]
                    , if scene.expandedView 
                        then
                            wrappedRow [ width fill, spacing 5 ] 
                                [ collapseBtn (ChangeConfigScene scene.id (\sc _-> { sc | expandedView= False }) "")
                                , column [ width fill, spacing 5 ] 
                                    [ Input.text [ width fill ]
                                        { onChange=
                                            ChangeConfigScene
                                                scene.id 
                                                (\sc newId -> 
                                                    { sc | id= sanitize "sceneID" newId }
                                                )
                                        , label= Input.labelAbove [] <| text "ID"
                                        , text= scene.id
                                        , placeholder= Nothing
                                        }
                                    , case scene.thumbnail of
                                        Nothing ->
                                            Input.button btnLook 
                                                { onPress= Just <| ChangeConfigScene scene.id (\sc str -> { sc | thumbnail= Just str }) (scene.id++"/pic.jpeg")
                                                , label= text "Add Pic"
                                                }
                                        Just pic ->
                                            row [ width fill, spacing 5 ]
                                                [ Input.text [ width fill ] 
                                                    { onChange= ChangeConfigScene scene.id (\sc str -> { sc | thumbnail= Just str })
                                                    , label= Input.labelHidden ""
                                                    , placeholder= Nothing
                                                    , text= pic
                                                    }
                                                , Input.button btnLook
                                                    { onPress= Just (ChangeConfigScene scene.id (\sc _-> { sc | thumbnail= Nothing }) "")
                                                    , label= text "Remove Pic"
                                                    }
                                                ]
                                    ]
                                , viewTranslations 
                                    (\l ->
                                        ChangeConfigScene scene.id 
                                            (\sc descr -> { sc | description= Dict.insert l descr sc.description } )
                                    ) 
                                    "Look Description" 
                                    <| reduce model.languages scene.description
                            , text "Add"
                            , column [ spacing 10 ]
                                [ Input.button btnLook
                                    { onPress= Just <| ChangeConfigScene scene.id
                                        (\cs _ ->
                                            { cs | items = 
                                                ( Toggle
                                                    { id="itemID"
                                                    , description= Dict.fromList
                                                        [ ( "en", "New optional item")
                                                        , ( "de", "Neues Zusatz-Element")
                                                        ]
                                                    , expandedView= False
                                                    , startActive= False
                                                    , shopIds= Nothing
                                                    , children= []
                                                    , xForm= origin
                                                    , physics= defaultPhysics
                                                    , mesh= cubeMesh
                                                    , skelAnim= Nothing
                                                    , look= newLook
                                                    } ) :: cs.items
                                            }
                                        ) ""
                                    , label= text "Toggle"
                                    }
                                , Input.button btnLook
                                    { onPress= Just <| ChangeConfigScene scene.id
                                        (\cs _ ->
                                            { cs | items = 
                                                ( Select
                                                    { id= "itemID"
                                                    , description= Dict.fromList
                                                        [ ( "en", "New fixed item with varying looks")
                                                        , ( "de", "Neues Element mit Farbvarianten")
                                                        ]
                                                    , rod= West
                                                    , expandedView= False
                                                    , xForm= origin
                                                    , panelXform= origin
                                                    , physics= defaultPhysics
                                                    , meshesById= Dict.fromList [ ("normal", cubeMesh) ]
                                                    , skelAnim= Nothing
                                                    , startGrpLook= ("","")
                                                    , lookGroups= []
                                                    , lookGroupsOffset= 0.0
                                                    , startOpen= False
                                                    } ) :: cs.items
                                            }
                                        ) ""
                                    , label= text "Select"
                                    }
                                , Input.button btnLook
                                    { onPress= Just <| ChangeConfigScene scene.id
                                        (\cs _ ->
                                            { cs | items = 
                                                ( SelectToggle
                                                    { id= "itemID"
                                                    , description= Dict.fromList
                                                        [ ( "en", "New optional item")
                                                        , ( "de", "Neues Zusatz-Element mit Farbvarianten")
                                                        ]
                                                    , rod= West
                                                    , expandedView= False
                                                    , xForm= origin
                                                    , panelXform= origin
                                                    , physics= defaultPhysics
                                                    , meshesById= Dict.fromList [ ("normal", cubeMesh) ]
                                                    , skelAnim= Nothing
                                                    , startGrpLook= ("", "")
                                                    , lookGroups= []
                                                    , lookGroupsOffset= 0.0
                                                    , startActive= False
                                                    , shopIdForNone= Nothing
                                                    , children= []
                                                    , startOpen= False
                                                    } ) :: cs.items
                                            }
                                        ) ""
                                    , label= text "SelectToggle"
                                    }
                                ]
                            ]
                        else expandBtn scene.id (ChangeConfigScene scene.id (\sc _-> { sc | expandedView= True }) "")
                    , if scene.expandedView then
                        column [ width fill, height fill, spacing 5, paddingXY 0 10 ] <|
                            List.map (viewItem scene.id model.languages scene.ralId scene.ralSuffix) <| List.indexedMap Tuple.pair scene.items
                        else Element.none
                    ]
            _ -> text "Only a single ConfigScene for now"
        ) 


-- VIEW HELPERS

btnLook = [ padding 5, Border.rounded 5, Border.solid, Background.color (gray 200) ]

iconLook = [ width (px 30), height (px 30), Border.rounded 15, Border.solid, Background.color (gray 155), Font.color (gray 200) ]


myFocusStyle =
    { borderColor = Nothing
    , backgroundColor = Nothing
    , shadow = Nothing
    }


showIf : Bool -> Element Msg -> Element Msg
showIf flag el =
    if flag
        then el
        else Element.none


setHtmlId : String -> Html.Attribute msg
setHtmlId id =
    Attribute.attribute "id" id


setElementId : String -> Element.Attribute msg
setElementId id =
    Element.htmlAttribute (setHtmlId id)


pxFloat : Float -> Element.Length
pxFloat p =
    px (round p)


gray g =
    rgb255 g g g


-- De-/Encoders

configSceneDecoder : Decoder ConfigScene
configSceneDecoder =
    Decode.succeed ConfigScene
        |> required "id" string
        |> required "description" langDecoder
        |> required "shopUrl" langDecoder
        |> required "thumbnail" (maybe string)
        |> required "anchor" anchorDecoder
        |> required "translations" translationDecoder
        |> required "items" (list itemDecoder)
        |> required "uiScale" float
        |> required "uiXform" xformDecoder
        |> required "codeXform" xformDecoder
        |> required "urlPrefix" string
        |> required "urlAR" string
        |> required "urlPreview" string
        |> hardcoded ""
        |> hardcoded ""
        |> required "animMeta" animMetaDecoder
        |> hardcoded False


configSceneEncoder : ConfigScene -> Encode.Value 
configSceneEncoder scene =
    Encode.object
        [ ( "id", Encode.string scene.id )
        , ( "description", langEncoder scene.description )
        , ( "shopUrl", langEncoder scene.shopUrl )
        , ( "thumbnail", (Json.Encode.Extra.maybe Encode.string scene.thumbnail) )
        , ( "anchor", anchorEncoder scene.anchor )
        , ( "translations", translationEncoder scene.translations )
        , ( "items", Encode.list itemEncoder scene.items )
        , ( "uiScale", Encode.float scene.uiScale )
        , ( "uiXform", xformEncoder scene.uiXform )
        , ( "codeXform", xformEncoder scene.codeXform )
        , ( "urlPrefix", Encode.string scene.urlPrefix )
        , ( "urlAR", Encode.string scene.urlAR )
        , ( "urlPreview", Encode.string scene.urlPreview )
        , ( "animMeta", animMetaEncoder scene.animMeta)
        ]


animMetaDecoder : Decoder AnimationMeta
animMetaDecoder =
    Decode.succeed AnimationMeta
        |> required "timeCodesPerSecond" float
        |> required "startTimeCode" float
        |> required "endTimeCode" float
        |> required "autoPlay" bool


animMetaEncoder : AnimationMeta -> Encode.Value
animMetaEncoder meta =
    Encode.object
        [ ( "timeCodesPerSecond", Encode.float meta.timeCodesPerSecond )
        , ( "startTimeCode", Encode.float meta.startTimeCode )
        , ( "endTimeCode", Encode.float meta.endTimeCode )
        , ( "autoPlay", Encode.bool meta.autoPlay )
        ]

infoSceneDecoder : Decoder InfoScene
infoSceneDecoder =
    Decode.succeed InfoScene
        |> required "id" string
        |> required "description" langDecoder
        |> required "panels" (list panelDecoder)
        |> required "uiScale" float
        |> required "uiXform" xformDecoder
        |> required "hint" langDecoder
        |> required "anchor" anchorDecoder
        |> hardcoded False


infoSceneEncoder : InfoScene -> Encode.Value
infoSceneEncoder scene =
    Encode.object
        [ ( "id", Encode.string scene.id )
        , ( "description", langEncoder scene.description )
        , ( "panels", Encode.list panelEncoder scene.panels )
        , ( "uiScale", Encode.float scene.uiScale )
        , ( "uiXform", xformEncoder scene.uiXform )
        , ( "hint", langEncoder scene.hint )
        , ( "anchor", anchorEncoder scene.anchor )
        ]


pageDecoder : Decoder Page
pageDecoder =
    Decode.succeed Page
        |> required "id" string
        |> required "imagePath" string
        |> required "imageWidth" int
        |> required "imageHeight" int
        |> required "bodyText" langDecoder
        |> hardcoded False


pageEncoder : Page -> Encode.Value
pageEncoder page =
    Encode.object
        [ ( "id", Encode.string page.id )
        , ( "imagePath", Encode.string page.imagePath )
        , ( "imageWidth", Encode.int page.imageWidth )
        , ( "imageHeight", Encode.int page.imageHeight )
        , ( "bodyText", langEncoder page.bodyText )
        ]


panelDecoder : Decoder Panel
panelDecoder =
    Decode.succeed Panel
        |> required "id" string
        |> required "btnXform" xformDecoder
        |> required "uiXform" xformDecoder
        |> required "startOpen" bool
        |> required "physics" physicsDecoder
        |> required "title" langDecoder
        |> required "subTitle" langDecoder
        |> required "pages" (list pageDecoder)
        |> required "startIdx" int
        |> hardcoded False


panelEncoder : Panel -> Encode.Value
panelEncoder panel =
    Encode.object
        [ ( "id", Encode.string panel.id )
        , ( "btnXform", xformEncoder panel.btnXform )
        , ( "uiXform", xformEncoder panel.uiXform )
        , ( "startOpen", Encode.bool panel.startOpen )
        , ( "physics", physicsEncoder panel.physics )
        , ( "title", langEncoder panel.title )
        , ( "subTitle", langEncoder panel.subTitle )
        , ( "pages", Encode.list pageEncoder panel.pages )
        , ( "startIdx", Encode.int panel.startIdx )
        ]


rodToStr : RodOrient -> String
rodToStr rod =
    case rod of
        North       -> "North"
        NorthWest   -> "NorthWest"
        West        -> "West"
        SouthWest   -> "SouthWest"
        South       -> "South"
        SouthEast   -> "SouthEast"
        East        -> "East"
        NorthEast   -> "NorthEast"


strToRod : String -> RodOrient
strToRod str =
    case str of
        "North"      -> North
        "NorthWest"  -> NorthWest
        "West"       -> West
        "SouthWest"  -> SouthWest
        "South"      -> South
        "SouthEast"  -> SouthEast
        "East"       -> East
        "NorthEast"  -> NorthEast
        _ -> North


rodDecoder : Decoder RodOrient
rodDecoder =
    Decode.succeed strToRod
        |> required "rod" string


anchorDecoder : Decoder Anchor
anchorDecoder =
    let
        mapToImage type_ filePath width =
            case type_ of
                "Horizontal" -> Horizontal
                "Vertical"   -> Vertical
                _ ->  Image (AnchorImage filePath width)
    in
    Decode.succeed mapToImage
        |> required "anchorType" string
        |> optional "filePath" string ""
        |> optional "physicalWidth" float 0.0


anchorEncoder : Anchor -> Encode.Value
anchorEncoder anchor =
    Encode.object <|
    case anchor of
        Image ai ->
            [ ( "anchorType", Encode.string "Image")
            , ( "filePath", Encode.string ai.filePath )
            , ( "physicalWidth", Encode.float ai.physicalWidth )
            ]
        Horizontal  -> [ ( "anchorType", Encode.string "Horizontal") ]
        Vertical    -> [ ( "anchorType", Encode.string "Vertical") ]


translationDecoder : Decoder Translation
translationDecoder =
    Decode.succeed Translation
        |> required "with"      langDecoder
        |> required "without"   langDecoder
        |> required "open"      langDecoder
        |> required "close"     langDecoder
        |> required "hint"      langDecoder
        |> required "order"     langDecoder
        |> required "openSummary" langDecoder
        |> required "closeSummary" langDecoder
        |> required "explain0" langDecoder
        |> required "explain1" langDecoder
        |> required "explain2" langDecoder
        |> required "explain3" langDecoder
        |> required "notAvailable" langDecoder


translationEncoder : Translation -> Encode.Value
translationEncoder trans =
    Encode.object
        [ ( "with",     langEncoder trans.with )
        , ( "without",  langEncoder trans.without )
        , ( "open",     langEncoder trans.open )
        , ( "hint",     langEncoder trans.hint )
        , ( "order",    langEncoder trans.order )
        , ( "openSummary", langEncoder trans.openSummary )
        , ( "closeSummary", langEncoder trans.closeSummary )
        , ( "explain0", langEncoder trans.explain0 )
        , ( "explain1", langEncoder trans.explain1 )
        , ( "explain2", langEncoder trans.explain2 )
        , ( "explain3", langEncoder trans.explain3 )
        , ( "notAvailable", langEncoder trans.notAvailable )
        ]

itemDecoder : Decoder Item
itemDecoder =
    Decode.oneOf 
        [ Decode.map Select selectDecoder
        , Decode.map SelectToggle selectToggleDecoder
        , Decode.map Toggle toggleDecoder
        , Decode.map Static staticDecoder
        , Decode.map Multi multiDecoder
        ]


productEncoder : Product -> Encode.Value
productEncoder i =
    Encode.object
        [ ( "product",      Encode.string i.product )
        , ( "ean",          Encode.string i.product )
        , ( "url",          langEncoder i.url )
        , ( "description",  langEncoder i.description )
        , ( "form",         Encode.string i.form )
        , ( "look",         Encode.string i.look )
        ]

productDecoder : Decoder Product
productDecoder =
    Decode.succeed Product
        |> required "description"   langDecoder
        |> required "url"           langDecoder
        |> required "product"       string
        |> required "ean"           string
        |> required "form"          string
        |> required "look"          string

formEncoder : Form -> Encode.Value
formEncoder i =
    Encode.object
        [ ( "id",           Encode.string i.id )
        , ( "thumbnail",    Encode.string i.thumbnail )
        , ( "cfgMesh",      meshEncoder i.cfgMesh )
        , ( "extraInputs",  Encode.list usdInputEncoder i.extraInputs )
        , ( "animatable",   Encode.bool i.animatable )
        , ( "description",  langEncoder i.description )
        ]

formDecoder : Decoder Form
formDecoder =
    Decode.succeed Form
        |> required "id"            string
        |> required "thumbnail"     string
        |> required "cfgMesh"       meshDecoder
        |> required "extraInputs"   (list usdInputDecoder)
        |> required "animatable"    bool
        |> required "description"   langDecoder

itemEncoder : Item -> Encode.Value
itemEncoder item =
    case item of
        Static i -> staticEncoder i
        Toggle i -> Encode.object
            [ ( "id",           Encode.string i.id )
            , ( "description",  langEncoder i.description )
            , ( "xForm",        xformEncoder i.xForm )
            , ( "mesh",         meshEncoder i.mesh )
            , ( "skelAnim",     Json.Encode.Extra.maybe skelAnimEncoder i.skelAnim )
            , ( "physics",      physicsEncoder i.physics )
            , ( "look",         lookEncoder i.look )
            , ( "startActive",  Encode.bool i.startActive )
            , ( "children",     Encode.list staticEncoder i.children )
            ]
        SelectToggle i -> Encode.object
            [ ( "id",           Encode.string i.id )
            , ( "description",  langEncoder i.description )
            , ( "xForm",        xformEncoder i.xForm )
            , ( "meshesById",   meshDictEncoder i.meshesById )
            , ( "skelAnim",     Json.Encode.Extra.maybe skelAnimEncoder i.skelAnim )
            , ( "physics",      physicsEncoder i.physics )
            , ( "startActive",  Encode.bool i.startActive )
            , ( "startOpen",    Encode.bool i.startOpen )
            , ( "lookGroups",   Encode.list lookGroupEncoder i.lookGroups )
            , ( "lookGroupsOffset", Encode.float i.lookGroupsOffset )
            , ( "startGrpLook", Encode.list Encode.string [ Tuple.first i.startGrpLook, Tuple.second i.startGrpLook ] )
            , ( "children",     Encode.list staticEncoder i.children )
            , ( "rod",          Encode.string <| rodToStr i.rod )
            ]
        Select i -> Encode.object
            [ ( "id",           Encode.string i.id )
            , ( "description",  langEncoder i.description )
            , ( "xForm",        xformEncoder i.xForm )
            , ( "meshesById",   meshDictEncoder i.meshesById )
            , ( "skelAnim",     Json.Encode.Extra.maybe skelAnimEncoder i.skelAnim )
            , ( "physics",      physicsEncoder i.physics )
            , ( "startOpen",    Encode.bool i.startOpen )
            , ( "lookGroups",   Encode.list lookGroupEncoder i.lookGroups )
            , ( "lookGroupsOffset", Encode.float i.lookGroupsOffset )
            , ( "startGrpLook", Encode.list Encode.string [ Tuple.first i.startGrpLook, Tuple.second i.startGrpLook ] )
            , ( "rod",          Encode.string <| rodToStr i.rod )
            ]
        Multi i -> Encode.object
            [ ( "id",           Encode.string i.id )
            , ( "description",  langEncoder i.description )
            , ( "physics",      physicsEncoder i.physics )
            , ( "xForm",        xformEncoder i.xForm )
            , ( "panelXform",   xformEncoder i.panelXform )
            , ( "matriXform",   xformEncoder i.matriXform )
            , ( "startProduct", Encode.string i.startProduct )
            , ( "products",     Encode.list productEncoder i.products )
            , ( "looks",        Encode.list lookEncoder i.looks )
            , ( "forms",        Encode.list formEncoder i.forms )
            , ( "columns",      Encode.int i.columns )
            , ( "panelW",       Encode.float i.panelW )
            , ( "panelH",       Encode.float i.panelH )
            , ( "matrixW",      Encode.float i.matrixW )
            , ( "matrixH",      Encode.float i.matrixH )
            ]


multiDecoder : Decoder MultiItem
multiDecoder =
    Decode.succeed MultiItem
        |> required "id" string
        |> required "description" langDecoder
        |> required "physics" physicsDecoder
        |> required "xForm" xformDecoder
        |> required "panelXform" xformDecoder
        |> required "matriXform" xformDecoder
        |> required "startProduct" string
        |> required "products" (list productDecoder)
        |> required "looks" (list lookDecoder)
        |> required "forms" (list formDecoder)
        |> required "columns" int
        |> required "panelW" float
        |> required "panelH" float
        |> required "matrixW" float
        |> required "matrixH" float
        |> hardcoded False

staticEncoder : StaticItem -> Encode.Value
staticEncoder i =
    Encode.object
        [ ( "id",       Encode.string i.id )
        , ( "xForms",   Encode.list xformEncoder i.xForms )
        , ( "mesh",     meshEncoder i.mesh )
        , ( "skelAnim", Json.Encode.Extra.maybe skelAnimEncoder i.skelAnim )
        , ( "physics",  physicsEncoder i.physics )
        , ( "look",     lookEncoder i.look )
        ]

staticDecoder : Decoder StaticItem
staticDecoder =
    Decode.succeed StaticItem
        |> required "id" string
        |> required "xForms" (list xformDecoder)
        |> required "mesh" meshDecoder
        |> optional "skelAnim" (maybe skelAnimDecoder) Nothing
        |> required "physics" physicsDecoder
        |> required "look" lookDecoder
        |> hardcoded False


selectDecoder : Decoder SelectItem
selectDecoder =
    Decode.succeed SelectItem
        |> required "id" string
        |> required "description" langDecoder
        |> required "xForm" xformDecoder
        |> required "panelXform" xformDecoder
        |> required "meshesById" meshDictDecoder
        |> optional "skelAnim" (maybe skelAnimDecoder) Nothing
        |> required "physics" physicsDecoder
        |> required "lookGroups" (list lookGroupDecoder)
        |> required "lookGroupsOffset" float
        |> required "startGrpLook" pairDecoder
        |> required "startOpen" bool
        |> required "rod" rodDecoder
        |> hardcoded False


selectToggleDecoder : Decoder SelectToggleItem
selectToggleDecoder =
    Decode.succeed SelectToggleItem
        |> required "id" string
        |> required "description" langDecoder
        |> required "xForm" xformDecoder 
        |> required "panelXform" xformDecoder
        |> required "meshesById" meshDictDecoder
        |> optional "skelAnim" (maybe skelAnimDecoder) Nothing
        |> required "physics" physicsDecoder
        |> required "lookGroups" (list lookGroupDecoder)
        |> required "lookGroupsOffset" float
        |> required "startGrpLook" pairDecoder
        |> required "startOpen" bool
        |> required "startActive" bool
        |> required "shopIdForNone" (maybe string)
        |> required "children" (list staticDecoder)
        |> required "rod" rodDecoder
        |> hardcoded False


toggleDecoder : Decoder ToggleItem
toggleDecoder =
    Decode.succeed ToggleItem
        |> required "id" string
        |> required "description" langDecoder
        |> required "xForm" xformDecoder 
        |> required "mesh" meshDecoder
        |> optional "skelAnim" (maybe skelAnimDecoder) Nothing
        |> required "physics" physicsDecoder
        |> required "look" lookDecoder
        |> required "startActive" bool
        |> required "shopIds" (maybe pairDecoder)
        |> required "children" (list staticDecoder)
        |> hardcoded False


lookGroupDecoder : Decoder LookGroup
lookGroupDecoder =
    Decode.succeed LookGroup
        |> required "id" string
        |> required "description" langDecoder
        |> required "looks" (list lookDecoder)


lookGroupEncoder : LookGroup -> Encode.Value
lookGroupEncoder g =
    Encode.object
        [ ( "id",           Encode.string g.id )
        , ( "description",  langEncoder g.description )
        , ( "looks",        Encode.list lookEncoder g.looks )
        ]


lookDecoder : Decoder Look
lookDecoder =
    Decode.succeed Look
        |> required "id" string
        |> required "description" langDecoder
        |> required "material" materialDecoder
        |> required "thumbnail" (maybe string)
        |> required "shopId" (maybe string)
        |> required "btnScaleU" float
        |> required "btnScaleV" float
        |> required "meshId" string
        |> hardcoded False


lookEncoder : Look -> Encode.Value
lookEncoder look =
    Encode.object
        [ ( "id",           Encode.string look.id )
        , ( "description",  langEncoder look.description )
        , ( "material",     materialEncoder look.material )
        , ( "thumbnail",    Json.Encode.Extra.maybe Encode.string look.thumbnail )
        , ( "shopId",       Json.Encode.Extra.maybe Encode.string look.shopId )
        , ( "btnScaleU",    Encode.float look.btnScaleU )
        , ( "btnScaleV",    Encode.float look.btnScaleV )
        , ( "meshId",       Encode.string look.meshId )
        ]


textureEncoder : Texture -> Encode.Value
textureEncoder tx =
    Encode.object
        [ ( "file",         Encode.string tx.file )
        , ( "pixelBias",    Json.Encode.Extra.maybe (Encode.list Encode.float) tx.pixelBias )
        , ( "pixelScale",   Json.Encode.Extra.maybe (Encode.list Encode.float) tx.pixelScale )
        , ( "useSecondUV",  Encode.bool tx.useSecondUV )
        , ( "scale",        Json.Encode.Extra.maybe (Encode.list Encode.float) tx.scale )
        , ( "rotation",     Json.Encode.Extra.maybe (Encode.list Encode.float) tx.rotation )
        , ( "translation",  Json.Encode.Extra.maybe (Encode.list Encode.float) tx.translation )
        ]

textureDecoder : Decoder Texture
textureDecoder =
    Decode.succeed Texture
        |> required "file" string
        |> required "pixelBias" (maybe (list float))
        |> required "pixelScale" (maybe (list float))
        |> required "useSecondUV" bool
        |> required "scale" (maybe (list float))
        |> required "rotation" (maybe (list float))
        |> required "translation" (maybe (list float))

usdInputDecoder : Decoder UsdInput
usdInputDecoder =
    Decode.succeed UsdInput
        |> required "id" string
        |> required "values" string
        |> required "texture" (maybe textureDecoder)


usdInputEncoder : UsdInput -> Encode.Value
usdInputEncoder i =
    Encode.object
        [ ( "id",       Encode.string i.id )
        , ( "values",   Encode.string i.values )
        , ( "texture",  Json.Encode.Extra.maybe textureEncoder i.texture )
        ]


physicsDecoder : Decoder Physics
physicsDecoder =
    Decode.succeed Physics
        |> required "collisionShape" shapeDecoder
        |> required "xForm" xformDecoder
        |> required "extend" (list xyzDecoder)
        |> required "frictionDynamic" float
        |> required "frictionStatic" float
        |> required "restitution" float


physicsEncoder : Physics -> Encode.Value
physicsEncoder p =
    Encode.object
        [ ( "collisionShape",   shapeEncoder p.colliderShape )
        , ( "xForm",            xformEncoder p.xForm )
        , ( "extend",           Encode.list xyzEncoder p.extent )
        , ( "frictionDynamic",  Encode.float p.frictionDynamic )
        , ( "frictionStatic",   Encode.float p.frictionStatic )
        , ( "restitution",      Encode.float p.restitution )
        ]


shapeEncoder : ColliderShape -> Encode.Value
shapeEncoder shape =
    Encode.object <|
        case shape of
            Cube s ->
                [ ( "type", Encode.string "Cube" ) 
                , ( "size", Encode.float s )
                ]
            Capsule c ->
                [ ( "type",     Encode.string "Capsule" )
                , ( "axis",     Encode.string c.axis )
                , ( "height",   Encode.float c.height )
                , ( "radius",   Encode.float c.radius )
                ]


shapeDecoder : Decoder ColliderShape
shapeDecoder =
    let
      toShape ax h r =
        Capsule { axis= ax, height= h, radius= r }  
    in
    field "type" string
        |> Decode.andThen
            (\t ->
                case t of
                    "Cube" ->
                      Decode.succeed Cube 
                        |> required "size" float
                    "Capsule" ->
                      Decode.succeed toShape
                        |> required "axis" string
                        |> required "height" float
                        |> required "radius" float
                    _ ->
                        Decode.fail "unknown shape type"
            )
        

materialDecoder : Decoder Material
materialDecoder =
    Decode.succeed Material
        |> required "id" string
        |> required "inputs" (list usdInputDecoder)


materialEncoder : Material -> Encode.Value
materialEncoder m =
    Encode.object
        [ ( "id", Encode.string m.id )
        , ( "inputs", Encode.list usdInputEncoder m.inputs ) 
        ]


xformDecoder : Decoder Xform
xformDecoder =
    Decode.succeed Xform
        |> required "scale" (maybe xyzDecoder)
        |> required "orient" (maybe (list float))
        |> required "translate" (maybe xyzDecoder)


xformEncoder : Xform -> Encode.Value
xformEncoder x =
    Encode.object
        [ ( "scale",     Json.Encode.Extra.maybe xyzEncoder x.scale )
        , ( "orient",    Json.Encode.Extra.maybe (Encode.list Encode.float) x.orient )
        , ( "translate", Json.Encode.Extra.maybe xyzEncoder x.scale )
        ]

interpolationEncoder : Interpolation -> Encode.Value
interpolationEncoder i =
    Encode.string <|
      case i of
        Constant -> "constant"
        Uniform -> "uniform"
        Varying -> "varying"
        FaceVarying -> "faceVarying"
        Vertex -> "vertex"

interpolationDecoder : Decoder Interpolation
interpolationDecoder =
    string |> Decode.andThen
        (\s -> Decode.succeed <|
            case s of
                "constant" -> Constant
                "uniform" -> Uniform
                "varying" -> Varying
                "faceVarying" -> FaceVarying
                "vertex" -> Vertex
                _ -> Vertex
        )


subDEncoder : SubdivisionScheme -> Encode.Value
subDEncoder s =
    Encode.string <|
        case s of
        None         -> "none"
        CatmullClark -> "catmullClark"
        Loop         -> "loop"
        Bilinear     -> "bilinear"


subDDecoder : Decoder SubdivisionScheme
subDDecoder =
    string |> Decode.andThen
        (\s -> Decode.succeed <|
            case s of
                "none"         -> None
                "catmullClark" -> CatmullClark
                "loop"         -> Loop
                "bilinear"     -> Bilinear
                _ -> None
        )


timedXyzDecoder : Decoder ( Float, List (Float, Float, Float ) )
timedXyzDecoder =
    Decode.succeed Tuple.pair
        |> required "time" float
        |> required "values" (list xyzDecoder)


timedXyzEncoder : ( Float, List (Float, Float, Float) ) -> Encode.Value
timedXyzEncoder ( t, values ) =
    Encode.object
        [ ( "time", Encode.float t )
        , ( "values", Encode.list xyzEncoder values )
        ]


timedQuadDecoder : Decoder ( Float, List (List Float) )
timedQuadDecoder =
    Decode.succeed Tuple.pair
        |> required "time" float
        |> required "values" (list quadDecoder)


timedQuadEncoder : ( Float, List (List Float) ) -> Encode.Value
timedQuadEncoder ( t, values ) =
    Encode.object
        [ ( "time", Encode.float t )
        , ( "values", Encode.list (Encode.list Encode.float) values)
        ]

skelAnimDecoder : Decoder SkelAnim
skelAnimDecoder =
    Decode.succeed SkelAnim
        |> required "joints" (list string)
        |> required "bindTransforms" (list (list (list float)))
        |> required "restTransforms" (list (list (list float)))
        |> required "rotations" (list timedQuadDecoder)
        |> required "translations" (list timedXyzDecoder)
        |> required "scales" (list timedXyzDecoder)
        |> required "jointIndices" (list int)
        |> required "jointWeights" (list float)
        |> required "elementSize" int
        |> required "geomBindTransform" (list (list float))


skelAnimEncoder : SkelAnim -> Encode.Value
skelAnimEncoder s =
    Encode.object
        [ ( "joints", Encode.list Encode.string s.joints )
        , ( "bindTransforms", Encode.list (Encode.list (Encode.list Encode.float)) s.bindTransforms )
        , ( "restTransforms", Encode.list (Encode.list (Encode.list Encode.float)) s.restTransforms )
        , ( "rotations", Encode.list timedQuadEncoder s.rotations )
        , ( "translations", Encode.list timedXyzEncoder s.translations )
        , ( "scales", Encode.list timedXyzEncoder s.scales )
        , ( "jointIndices", Encode.list Encode.int s.jointIndices )
        , ( "jointWeights", Encode.list Encode.float s.jointWeights )
        , ( "elementSize", Encode.int s.elementSize )
        , ( "geomBindTransform", Encode.list (Encode.list Encode.float) s.geomBindTransform )
        ]


meshDecoder : Decoder Mesh
meshDecoder =
    Decode.succeed Mesh
        |> required "faceVertexCounts" (list int)
        |> required "faceVertexIndices" (list int)
        |> required "points" (list xyzDecoder)
        |> optional "normals" (list xyzDecoder) []
        |> optional "normalIndices" (maybe (list int)) Nothing
        |> optional "normalsInterpolation" interpolationDecoder Vertex
        |> optional "st" (list uvDecoder) []
        |> optional "stIndices" (maybe (list int)) Nothing
        |> optional "stInterpolation" interpolationDecoder Vertex
        |> optional "st1" (list uvDecoder) []
        |> optional "st1Indices" (maybe (list int)) Nothing
        |> optional "st1Interpolation" interpolationDecoder Vertex
        |> required "subdivisionScheme" subDDecoder
        |> required "noExtend" bool
        |> required "doubleSided" bool


meshEncoder : Mesh -> Encode.Value
meshEncoder m =
    Encode.object
        [ ( "faceVertexCounts", Encode.list Encode.int m.faceVertexCounts )
        , ( "faceVertexIndices",Encode.list Encode.int m.faceVertexIndices )
        , ( "points",           Encode.list xyzEncoder m.points )
        , ( "normals",          Encode.list xyzEncoder m.normals )
        , ( "normalIndices",    Json.Encode.Extra.maybe (Encode.list Encode.int) m.normalIndices )
        , ( "normalsInterpolation", interpolationEncoder m.normalsInterpolation )
        , ( "st",               Encode.list uvEncoder m.st )
        , ( "stIndices",        Json.Encode.Extra.maybe (Encode.list Encode.int) m.stIndices )
        , ( "stInterpolation",  interpolationEncoder m.stInterpolation )
        , ( "st1",              Encode.list uvEncoder m.st1 )
        , ( "st1Indices",       Json.Encode.Extra.maybe (Encode.list Encode.int) m.st1Indices )
        , ( "st1Interpolation", interpolationEncoder m.st1Interpolation )
        , ( "subdivisionScheme",subDEncoder m.subdivisionScheme )
        , ( "noExtend",         Encode.bool m.noExtend )
        , ( "doubleSided",      Encode.bool m.doubleSided )
        ]


pairDecoder : Decoder (String, String)
pairDecoder =
    list string
        |> Decode.andThen
            (\tupleList -> 
                case tupleList of
                   [u,v] -> Decode.succeed (u,v)
                   _ -> Decode.succeed ("","")
            )

uvDecoder : Decoder (Float, Float)
uvDecoder =
    list float
        |> Decode.andThen
            (\tupleList -> 
                case tupleList of
                   [u,v] -> Decode.succeed (u,v)
                   _ -> Decode.succeed (0,0)
            )

uvEncoder : (Float, Float) -> Encode.Value
uvEncoder (u,v) =
    Encode.list Encode.float [u,v]


xyzDecoder : Decoder (Float, Float, Float)
xyzDecoder =
    list float
        |> Decode.andThen
            (\tupleList -> 
                case tupleList of
                    [a,b,c] -> Decode.succeed (a,b,c)
                    _ -> Decode.succeed (0,0,0)
            )


xyzEncoder : (Float, Float, Float) -> Encode.Value
xyzEncoder (x,y,z) =
    Encode.list Encode.float [x,y,z]


quadDecoder : Decoder (List Float)
quadDecoder =
    list float
        |> Decode.andThen
            (\values ->
                case values of
                    [ _, _, _, _ ] -> Decode.succeed values
                    _ -> Decode.fail "needs 4 values"
            )


langDecoder : Decoder LanguageString
langDecoder =
    keyValuePairs string |> Decode.map Dict.fromList


langEncoder : LanguageString -> Encode.Value
langEncoder ls =
    Encode.dict identity Encode.string ls


meshDictDecoder : Decoder (Dict String Mesh)
meshDictDecoder =
    keyValuePairs meshDecoder |> Decode.map Dict.fromList


meshDictEncoder : Dict String Mesh -> Encode.Value
meshDictEncoder dm =
    Encode.dict identity meshEncoder dm


{- --------------------------------- -}

showGroupId = False
showShopId = False

useShopIdAsCode= False
shopIdLength= 13.0

codeCharWidth= 0.05692727 / 0.932 -- for Menlo 

optimizeForMatrix= False -- TODO Matrix


{-

Compile Main application:
elm make src/Main.elm --output=public/Main.js

Compile Intro page for product Swivel:
elm make src/Intro/Intro.elm --output=public-intro/Intro.js

Build Tauri App:
yarn tauri dev

Convert Export to USDZ:
usdcat -o Test.usdc Test.usda
usdzip Test.usdz Test.usdc ao/* opacity/runner.jpg

Standard Intro-Test:
https://www.kreativekk.de/Swivel.html?CABAREBEYN

-}


{-
    # TODO
    - Je Item die url-Code-Breiten für Gruppe/Look konfigurierbar machen
    - Den vertikalen Anfang der Gruppenbuttons korrigieren (RAL_Projekt ist um 2 Zeilen versetzt)
-}