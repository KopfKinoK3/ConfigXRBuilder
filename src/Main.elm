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
        , centerX
        , centerY
        , column
        , el
        , fill
        , height
        , paddingXY, padding
        , px
        , rgb255
        , row
        , spacing
        , text
        , width, wrappedRow
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import File exposing (File)
import Html exposing (Attribute, Html)
import Html.Attributes as Attribute
import Json.Decode as Decode exposing (Decoder, int, string, float, dict, list, bool, oneOf, andThen, keyValuePairs, field, maybe)
import Json.Decode.Pipeline exposing (required, hardcoded, resolve, custom, optional)
import Json.Encode as Encode
import Json.Encode.Extra
import Task exposing (Task)
import Time

import Outside
import Element exposing (fillPortion)
import Element exposing (scrollbarY)
import Char exposing (isDigit, isAlphaNum, toCode)
import Element.Input exposing (labelRight)
import Element.Input exposing (labelHidden)
import Dict exposing (Dict)
import String exposing (indices)
import Html exposing (s)

import TestScene exposing (testScene, cubeMesh)

import ConfigXR.Types exposing 
    ( Anchor(..)
    , AnchorImage
    , Mesh
    , Xform
    , Material
    , ColliderShape(..)
    , Physics
    , HorAlign(..)
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
    , Item(..)
    , Translation
    , ConfigScene
    , newLook
    , defaultPhysics
    , defaultTranslations
    , origin
    , verticalOrientation
    , floorOrientation
    )

import ConfigXR.Helper exposing (simpleLooks, sanitize, fixDescr)
import ConfigXR.UIMeshes exposing (..)
import ConfigXR.Project exposing (configScene)

type Msg
    = NoOp
    | NoOpStr String
    | WindowSize Int Int
    | LogErr String
    | OutsideInfo Outside.InfoForOutside
    | Outside Outside.InfoForElm
    | AddLanguage
    | ChangeLanguage String
    | ToggleLanguage String
    | ExportScene
    | LoadTestDoc
    | LoadChairDoc
    | SaveScene Int
    | NewDoc
    | OpenDoc
    | DeleteLook Int String String
    | ChangeGrpName Int String String
    | ChangeGrpDescr Int String String String
    | DeleteGrpById Int String 
    | AddLookToGrp Int String 
    | AddLookGroupToItemIdx Int 
    | ChangeItemId Int String 
    | DeleteItemByIdx Int

    | ChangeMaterialId String Int String String String
    | ChangeItemDescr String Int String String
    | ChangeLook String Int String String (String -> Look -> Look) String
    | ChangeConfigScene String (ConfigScene -> String -> ConfigScene) String
    | ChangeSelectItem String String (SelectItem -> String -> SelectItem) String
    | ChangeSelectToggleItem String String (SelectToggleItem -> String -> SelectToggleItem) String


reduce : Dict String Bool -> LanguageString -> LanguageString
reduce languages ls =
    Dict.filter
        (\l _ ->
            Maybe.withDefault True <| Dict.get l languages
        ) 
        ls


exportLang= "de"
fallBackLang= "en"
langGet : String -> LanguageString -> String
langGet l ls =
    case Dict.get l ls of
        Nothing ->
            case String.split "-" l of
                master :: _ ->
                    case Dict.get master ls of
                        Nothing -> 
                            (Dict.get fallBackLang ls 
                                |> Maybe.withDefault "?????")
                        Just str -> str
                _ ->
                    (Dict.get fallBackLang ls 
                        |> Maybe.withDefault "?????")
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

type alias Model =
    { width : Float
    , height : Float
    , scenes : List SceneModel
    , newLang : String
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

usdPanel : String -> String -> Float -> Float -> Float -> Xform -> List String
usdPanel scene name w h factor xForm =
    let
        scale= String.fromFloat (1.0/factor)
        scaleLine= "double3 xformOp:scale = (" ++ scale ++", "++ scale ++", "++ scale ++")"
    in
    usdGroup name xForm
        (
            [ 
                "def Xform \"Border\" {"
            , "rel material:binding = </Root/Scenes/"++scene++"/UIPanelAccent>"
            , "quatf xformOp:orient = (1, 0, 0, 0)"
            , scaleLine
            , "double3 xformOp:translate = (0, 0.0016, 0)"
            , "uniform token[] xformOpOrder = [\"xformOp:translate\", \"xformOp:orient\", \"xformOp:scale\"]"
            , usdMesh "BorderMesh" <| resizeRoundedBox (w*factor) (h*factor) uiPanel100Border
            , "}"
            , "def Xform \"Face\" {"
            , "rel material:binding = </Root/Scenes/"++scene++"/UIPanelMaterial>"
            , "quatf xformOp:orient = (1, 0, 0, 0)"
            , scaleLine
            , "double3 xformOp:translate = (0, 0.0000, 0)"
            , "uniform token[] xformOpOrder = [\"xformOp:translate\", \"xformOp:orient\", \"xformOp:scale\"]"
            , usdMesh "FaceMesh" <| resizeRoundedBox (w*factor) (h*factor) uiPanel100Face
            , "}"
            , 
              "def Xform \"Ground\" {"
            , "rel material:binding = </Root/Scenes/"++scene++"/UIPanelMaterial>"
            , "quatf xformOp:orient = (1, 0, 0, 0)"
            , scaleLine
            , "double3 xformOp:translate = (0, 0.0032, 0)"
            , "uniform token[] xformOpOrder = [\"xformOp:translate\", \"xformOp:orient\", \"xformOp:scale\"]"
            , usdMesh "GroundMesh" <| resizeRoundedBox (w*factor) (h*factor) uiPanel100Ground
            , "}"
            ]
        )



usdMaterial : String -> String -> Material -> String
usdMaterial prefix name mat =
    String.join "\n"
      ( [ "def Material \"" ++ name ++ "\" {"
        , "token outputs:surface.connect = <" ++ prefix ++ "/" ++ name ++ "/PBRShader.outputs:surface>"
        , "def Shader \"texCoordReader\""
        , "{\nuniform token info:id = \"UsdPrimvarReader_float2\""
        , "token inputs:varname = \"st\""
        , "float2 outputs:result\n}"
        , "def Shader \"PBRShader\" {\nuniform token info:id = \"UsdPreviewSurface\""
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
        , "}"
        ]
      ++ 
        ( List.concat <| List.filterMap
             (\input ->
                Maybe.map
                    (\tex ->
                        [ "def Shader \"" ++ input.id ++"_texture\""
                        , "{"
                        , "uniform token info:id = \"UsdUVTexture\""
                        , "float4 inputs:bias = (0, 0, 0, 0)"
                        , "asset inputs:file = @" ++ tex ++"@"
                        , "float4 inputs:scale = (1, 1, 1, 1)"
                        , "float2 inputs:st.connect = <" ++ prefix ++ "/"++ mat.id ++"/texCoordReader.outputs:result>"
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
    , description= Dict.fromList [ ("en", "Sample Scene description"), ( "de", "Beispiel Szenenbeschreibung") ]
    , thumbnail= Nothing
    , uiScale= 1.0
    , uiXform= { scale= Just (1, 1, 1), orient = Just [1, 0, 0, 0], translate= Just (0,0,0.15) }
    , anchor= Horizontal
    , translations= defaultTranslations
    , items= []
    , expandedView= False
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    (   { width= flags.width
        , height= flags.height
        , scenes= [ MainMenu ]
        , newLang= exportLang
        , languages= Dict.fromList [ (exportLang, True) ]
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


usdHeader : String -> String -> String
usdHeader id masters =
    String.concat [ """#usda 1.0
(
autoPlay = false
customLayerData = {
string creator = "mobi.config-xr.CXRBuilder Version 0.1 beta"
string identifier = """, "\"", id,  "\"", """
}
defaultPrim = "Root"
metersPerUnit = 1
timeCodesPerSecond = 60
upAxis = "Y"
)
"""++ masters ++"""
def Xform "Root"
{
def Scope "Scenes" (
kind = "sceneLibrary"
)
{
""" ]


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


usd3dList : List (Float, Float, Float) -> String
usd3dList l =
    "[ " ++ ( String.join ", " 
        ( List.map 
            (\(x,y, z) -> "( " ++ String.fromFloat x ++ ", " ++ String.fromFloat y ++ ", " ++ String.fromFloat z ++ " )" )
            l
        ) 
    ) ++ " ]"


usdIntList : List Int -> String
usdIntList indices =
    "[ " ++ (String.join ", " (List.map String.fromInt indices)) ++ " ]"



usdShapeAsset : String -> String -> String -> String
usdShapeAsset name meshRef matRef =
    "def \"" ++ name ++ """\" { def "Mesh0" {
rel material:binding = </AssetFactories/Masters/Factories/""" ++ name ++ """/Mesh0/Material>
def "Mesh0" (\ninstanceable = true\nprepend references = """ ++ meshRef ++"""\n) {}
def "Material" (\nprepend references = """ ++ matRef ++ """\n) {}\n}\n}"""


usdPlace : String -> String -> Maybe String -> Xform -> Physics -> String
usdPlace path name overwriteRef xForm physics =
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


usdMesh : String -> Mesh -> String
usdMesh name mesh =
    String.concat
        [ "def Mesh \""++ name ++"\" {\n"
        , """int[] faceVertexCounts = """, usdIntList mesh.faceVertexCounts, """
int[] faceVertexIndices = """, usdIntList mesh.faceVertexIndices, """
point3f[] points = """, usd3dList mesh.points, """
normal3f[] primvars:normals = """, usd3dList mesh.normals, """ (
interpolation = "varying"
)
"""     , case mesh.normalIndices of
            Nothing -> ""
            Just i -> "int[] primvars:normals:indices = " ++ usdIntList i ++ "\n"
        , """texCoord2f[] primvars:st = """, usd2dList mesh.st, """ (
interpolation = "faceVarying"
)
"""     , case mesh.stIndices of
            Nothing -> ""
            Just i -> "int[] primvars:st:indices = " ++ usdIntList i ++ "\n"
        , """uniform token subdivisionScheme = "none"
}
"""
        ]


usdOver : String -> Mesh -> String
usdOver name mesh =
    String.concat
        [ "over Mesh \""++ name ++"\" {\n"
        , """int[] faceVertexCounts = """, usdIntList mesh.faceVertexCounts, """
int[] faceVertexIndices = """, usdIntList mesh.faceVertexIndices, """
point3f[] points = """, usd3dList mesh.points, """
normal3f[] primvars:normals = """, usd3dList mesh.normals, """ (
interpolation = "varying"
)
"""     , case mesh.normalIndices of
            Nothing -> ""
            Just i -> "int[] primvars:normals:indices = " ++ usdIntList i ++ "\n"
        , """texCoord2f[] primvars:st = """, usd2dList mesh.st, """ (
interpolation = "faceVarying"
)
"""     , case mesh.stIndices of
            Nothing -> ""
            Just i -> "int[] primvars:st:indices = " ++ usdIntList i ++ "\n"
        , """uniform token subdivisionScheme = "none"
}
"""
        ]


usdFlattened : (String, Mesh) -> String
usdFlattened (id, mesh) =
    usdOver (String.replace ">" "" (String.replace "</" "" id)) mesh


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
        ( "def Xform \"" ++ name ++" {\n"
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

type UsdTrigger
    = TapGesture String
    | SceneTransition String


type UsdAction
    = Visibility { show: Bool, duration: Float, affected: (List String) }
    | Group { parallel: Bool, loop: Bool, performCount: Int, actions: List (String, UsdAction) }
    | Transform { absolute: Bool, duration: Float, xformTarget: String, affected: String }
    | LookAtCamera { duration: Float, front: (Float, Float, Float), upVector: (Float, Float, Float), affected: List String }                           


type alias UsdBehavior =
    { id: String
    , actionRoot: UsdAction
    , triggers: List UsdTrigger
    , exclusive: Bool
    }


usdAction : String -> String -> UsdAction -> String
usdAction prefix name action =
    String.join "\n" <|
      case action of
        Visibility v ->
            [ "def Preliminary_Action \""++ name ++"\" {"
            , "rel affectedObjects = ["++ String.join ", " v.affected ++"]"
            , "double duration = " ++ String.fromFloat v.duration
            , "token easeType = \"inout\""
            , "token info:id = \"Visibility\""
            , "token motionType = \"none\""
            , "double moveDistance = 0"
            , "token style = \"basic\""
            , "token type = \"" ++ (if v.show then "show" else "hide") ++ "\""
            , "}"
            ]
        Group g ->
            (   [ "def Preliminary_Action \""++ name ++"\" {"
                , "rel actions = [ "++ String.join ", " (List.map (\(n,_) -> "<"++prefix++name++"/"++n++">") g.actions) ++" ]"
                , "token info:id = \"Group\""
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
            , "rel affectedObjects = "++ t.affected
            , "double duration = "++ String.fromFloat t.duration
            , "token easeType = \"none\""
            , "token info:id = \"Transform\""
            , "token type = \"" ++ (if t.absolute then "absolute" else "relative" ) ++ "\""
            , "rel xformTarget = "++ t.xformTarget
            , "}"
            ]
        LookAtCamera t ->
            [ "def Preliminary_Action \""++ name ++"\" {"
            , "rel affectedObjects = [ "++ (String.join ", " t.affected) ++" ]"
            , "double duration = "++ String.fromFloat t.duration
            , "double3 front = (0, 0, 1)"
            , "token info:id = \"LookAtCamera\""
            , "double3 upVector = (0, 0, 0)"
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


behaviorsForSelectToggle : String -> SelectToggleItem -> (List (String, UsdAction), List UsdBehavior)
behaviorsForSelectToggle scene item =
    let
        name = item.id
        objPath     = "</Root/Scenes/"++scene++"/Children/Object_"++name++"/Children/"
        editBtnPath = uiPath++"EditButton/Children/"
        uiPath      = "</Root/Scenes/"++scene++"/Children/ConfigUI/Children/Hideable/Children/UI_"++name++"/Children/"
        panelPath   = "</Root/Scenes/"++scene++"/Children/ConfigUI/Children/Hideable/Children/Select_"++name++"/Children/Hideable/Children/"
    in
    ( [ ( "TogglePos_"++name
        , Transform 
            { absolute= False
            , duration= 0
            , xformTarget= 
                if item.startActive
                    then "</_Root_Scenes_"++scene++"_Behaviors_"++ name ++"_TogglePos_ON>"
                    else "</_Root_Scenes_"++scene++"_Behaviors_"++ name ++"_TogglePos_OFF>"
            , affected= uiPath++"ToggleGroup>"
            }
        )
      , if item.startActive
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
      , ( "HideLooksAndLabels_"++name -- Hide most objects, indicators and labels
        , Group { parallel= True, loop= False, performCount= 1, actions= 
            List.map 
            (\group -> 
                ( "Group_"++group.id
                , Group { parallel= True, loop= False, performCount= 1, actions=
                    List.filterMap
                        (\look -> 
                            if (group.id, look.id) /= item.startGrpLook
                                then Just 
                                    ( "Look_"++look.id
                                    , justHide 
                                        [ (objPath++"Instance_" ++ group.id ++ "_" ++ look.id ++ ">")
                                        , (uiPath++"SelectLabels/Children/Look_" ++ group.id ++ "_" ++ look.id ++ ">")
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
    , [ { id= "SwitchON_"++name
        , exclusive= True
        , triggers=
            [ TapGesture (uiPath++"ToggleGroup/Children/ToggleOFF>")
            , TapGesture (uiPath++"OFF>")
            , TapGesture (uiPath++"StateOFF>")
            , TapGesture (uiPath++"ONText>")
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
            , TapGesture (uiPath++"OFFText>")
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
            , TapGesture (uiPath++"OFFText>")
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
                                [ ( "ShowLook"
                                , justShow
                                        [ (objPath ++ "Instance_" ++ outerGroup.id ++ "_" ++ look.id ++ ">")
                                        , (uiPath++"SelectLabels/Children/Look_" ++ outerGroup.id ++ "_" ++ look.id ++ ">")
                                        --, (panelPath ++ outerGroup.id ++ "/Children/Indicate_"++ look.id ++">") -- TODO create
                                        ]
                                )
                                , ( "HideOthers"
                                  , Group { parallel= True, loop= False, performCount= 1, actions= 
                                        List.map 
                                            (\group -> 
                                                ( "Group_"++group.id
                                                , Group { parallel= True, loop= False, performCount= 1, actions=
                                                    List.filterMap
                                                        (\innerLook -> 
                                                            if (group.id, innerLook.id) /= (outerGroup.id, look.id)
                                                                then Just <| ( "Look_"++innerLook.id, justHide 
                                                                    [ (objPath++"Instance_" ++ group.id ++ "_" ++ innerLook.id ++ ">")
                                                                    , (uiPath++"SelectLabels/Children/Look_" ++ group.id ++ "_" ++ innerLook.id ++ ">")
                                                                    ] )
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
                                        ( "Hide_"++innerGroup.id
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


behaviorsForSelect : String -> SelectItem -> (List (String, UsdAction), List UsdBehavior)
behaviorsForSelect scene item =
    let
        name = item.id
        objPath     = "</Root/Scenes/"++scene++"/Children/Object_"++name++"/Children/"
        editBtnPath = uiPath++"EditButton/Children/"
        uiPath      = "</Root/Scenes/"++scene++"/Children/ConfigUI/Children/Hideable/Children/UI_"++name++"/Children/"
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
                                then Just 
                                    ( "Look_"++look.id
                                    , justHide 
                                        [ (objPath++"Instance_" ++ group.id ++ "_" ++ look.id ++ ">")
                                        , (uiPath++"SelectLabels/Children/Look_" ++ group.id ++ "_" ++ look.id ++ ">")
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
                                    ]
                                )
                                , ( "HideOthers"
                                  , Group { parallel= False, loop= False, performCount= 1, actions=
                                        List.concatMap
                                            (\group ->
                                                List.filterMap
                                                    (\innerLook ->
                                                        if (group.id, innerLook.id) /= (outerGroup.id, look.id)
                                                            then Just <| ( "Look_"++innerLook.id, justHide 
                                                                [ (objPath++"Instance_" ++ group.id ++ "_" ++ innerLook.id ++ ">")
                                                                , (uiPath++"SelectLabels/Children/Look_" ++ group.id ++ "_" ++ innerLook.id ++ ">")
                                                                ] )
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
                                    ( "Hide_"++innerGroup.id
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
        uiPath      = "</Root/Scenes/"++scene++"/Children/ConfigUI/Children/Hideable/Children/UI_"++name++"/Children/"
    in
    ( [ ( "TogglePos_"++name
        , Transform 
            { absolute= False
            , duration= 0
            , xformTarget= 
                if item.startActive
                    then "</_Root_Scenes_"++scene++"_Behaviors_"++ name ++"_TogglePos_ON>"
                    else "</_Root_Scenes_"++scene++"_Behaviors_"++ name ++"_TogglePos_OFF>"
            , affected= uiPath++"ToggleGroup>"
            }
        )
      , ( "Hide_"++name
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
      ]
    , [ { id= "SwitchON_"++name
        , exclusive= True
        , triggers=
            [ TapGesture (uiPath++"ToggleGroup/Children/ToggleOFF>")
            , TapGesture (uiPath++"OFF>")
            , TapGesture (uiPath++"StateOFF>")
            , TapGesture (uiPath++"ONText>")
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
                    ] }
              )
            , ( "Hide"
              , Visibility { show= False, duration= 0, affected=
                    [ uiPath++"ToggleGroup/Children/ToggleOFF>"
                    , uiPath++"OFF>"
                    , uiPath++"StateOFF>"
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
            , TapGesture (uiPath++"OFFText>")
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
                    ] }
              )
            , ( "Hide"
              , Visibility { show= False, duration= 0, affected=
                    [ uiPath++"ToggleGroup/Children/ToggleON>"
                    , uiPath++"ON>"
                    , uiPath++"StateON>"
                    , "</Root/Scenes/"++scene++"/Children/Object_" ++ name ++">"
                    ] }
              )
            ] }
        }
      ]
    )


usdGroup : String -> Xform -> List String -> List String
usdGroup name xForm children =
    (  [ "def Xform \""++ name ++"\" {"  
       , usdXformBlock xForm
       , """def Xform "Generated" {}"""
       , """def Xform "Children" {"""
      ]
    ++ children
    ++ [ "}", "}" ]
    )


type alias GatherUsd =
    { uiXform: Xform
    , lines: List String 
    }

uiForItem : String -> Float -> Translation -> Item -> GatherUsd -> GatherUsd
uiForItem scene size t i old =
    let
        labelTextMat    = "</Root/Scenes/"++scene++"/UITextBody>"
        activeTextMat   = "</Root/Scenes/"++scene++"/UITextActive>"
        inactiveTextMat = "</Root/Scenes/"++scene++"/UITextInactive>"

        toggleLines : String -> String -> String -> List String
        toggleLines name panelAsset description =
            let
                uiPath      = "</Root/Scenes/"++scene++"/Children/ConfigUI/Children/Hideable/Children/UI_"++name++"/Children/"
                togglePath  = uiPath++"ToggleGroup/Children/"
            in
                [ usdPlace uiPath "StateON" (Just "</AssetFactories/Masters/Factories/UIToggleActive>")
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
                , usdPlace uiPath "StateOFF" (Just "</AssetFactories/Masters/Factories/UIToggleInactive>")
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
                , usdPlace uiPath "ON" (Just panelAsset)
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
                , usdPlace uiPath "OFF" (Just panelAsset)
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
                    { horizontalAlignment= Left
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
                    , horizontalAlignment= Left
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
                    , horizontalAlignment= Right
                    }
                    (langGet exportLang t.without)
                ,  String.join "\n" <| 
                    usdGroup "ToggleGroup"
                        { scale= Just (size, size, size)
                        , orient= verticalOrientation
                        , translate= Just (0.0325/0.267 *size, 0.0085 *size, -0.0051/0.267 *size)
                        --, translate= Just (0.0325/0.267 *size, -0.0085/0.267 *size, -0.0051/0.267 *size)
                        }
                        [ usdPlace togglePath "ToggleON" (Just "</AssetFactories/Masters/Factories/UIToggleButton>")
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
                        , usdPlace togglePath "ToggleOFF" (Just "</AssetFactories/Masters/Factories/UIToggleButton>")
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
            usdGroup ("SelectLabels") 
                { orient= floorOrientation
                , scale= Just (size, size, size)
                , translate= Just (0.0167/0.267 *size, 0.011236 *size, 0.0085/0.267 *size)
                }
                <| List.concatMap
                    (\group ->
                        List.map
                            (\look ->
                                let
                                    gd = fixDescr <| langGet exportLang group.description
                                    ld = fixDescr <| langGet exportLang look.description
                                    descr= if gd == "_" then ld else String.join "\n" [ gd, ld ]
                                in
                                usdText scene ( "Look_"++ group.id ++ "_" ++ look.id )
                                    { scale= Nothing
                                    , orient= Nothing
                                    , translate= Just (0,0,0)
                                    }
                                    labelTextMat
                                    { horizontalAlignment= Left
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
            usdGroup "EditButton"
                { orient= floorOrientation
                , scale= Just (size, size, size)
                --, translate= Just (0.0324 /0.267*size, 0.0035 /0.267*size, 0.0085 /0.267*size)
                , translate= Just (0.0324 /0.267*size, 0.0035 /0.267*size, 0.0085 /0.267*size)
                }
                [ usdText scene "Open"
                    origin
                    activeTextMat
                    { horizontalAlignment= Center
                    , width= 0.2
                    , height= 0.03
                    , pointSize= 70.86614
                    }
                    (langGet exportLang t.open)
                , usdText scene "Close"
                    origin
                    inactiveTextMat
                    { horizontalAlignment= Center
                    , width= 0.2
                    , height= 0.03
                    , pointSize= 70.86614
                    }
                    (langGet exportLang t.close)
                , usdPlace editPath "OpenBG" (Just "</AssetFactories/Masters/Factories/UIOpenButton>")
                    { orient= verticalOrientation
                    , scale= Just (0.267, 0.24, 0.24)
                    , translate= Just (0.0, 0.0, -0.001)
                    }
                    defaultPhysics
                , usdPlace editPath "CloseBG" (Just "</AssetFactories/Masters/Factories/UICloseButton>")
                    { orient= verticalOrientation
                    , scale= Just (0.267, 0.24, 0.24)
                    , translate= Just (0.0, 0.0, -0.001)
                    }
                    defaultPhysics
                ]

        overviewLines : String -> Float -> Float -> List LookGroup -> (Float, List String)
        overviewLines name xCenter yCenter lookGroups =
            if List.length lookGroups == 1
                then (yCenter, [])
                else
                  let
                    buttonsPath= "</Root/Scenes/"++scene++"/Children/ConfigUI/Children/Hideable/Children/Select_"++name++"/Children/Hideable/Children/Overview/Children/"
                    xCols= 3
                    xStart= xCenter - (xStep * (xCols-1.0) / 2.0)
                    xStep= 0.1
                    yStep= 0.025
                    xGap= 0.025
                    yGap= 0.015
                    gather= List.foldl
                        (\group pos ->
                            let
                                newLines= String.join "\n" <|
                                    [ usdText scene ("Label_"++group.id)
                                        { scale= Just (1,1,1)
                                        , orient= verticalOrientation
                                        , translate= Just (pos.x, pos.y, 0.003)
                                        }
                                        labelTextMat
                                        { horizontalAlignment= Center
                                        , width= 0.45
                                        , height= 0.12
                                        , pointSize= 28.0
                                        }
                                        (langGet exportLang group.description)
                                    , usdPlace buttonsPath ("Pick_"++group.id)
                                        (Just "</AssetFactories/Masters/Factories/UIGroupPick>")
                                        { orient= verticalOrientation
                                        , scale= Just (0.17, 0.1, 0.1)
                                        , translate= Just (pos.x, pos.y, 0.001)
                                        }
                                        defaultPhysics
                                    , usdPlace buttonsPath ("Active_"++group.id)
                                        (Just "</AssetFactories/Masters/Factories/UIGroupActive>")
                                        { orient= verticalOrientation
                                        , scale= Just (0.17, 0.1, 0.1)
                                        , translate= Just (pos.x, pos.y, 0.001)
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
                  , usdGroup ("Overview") { origin | translate= Just (xCenter, gather.panelY, 0) } gather.lines
                  )

        panelLines : String -> Float -> Float -> List LookGroup -> List String
        panelLines name xCenter yCenter lookGroups=
            let
                (overviewHeight, overview) = overviewLines name xCenter yCenter lookGroups
            in
            --(usdPlace
            --    ("</Root/Scenes/"++scene++"/Children/ConfigUI/Children/Hideable/Children/Select_"++name++"/Children/")
            --    "Rod"
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
                    xStart= xCenter - (xStep * (xCols-1.0) / 2.0)
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
                                        { horizontalAlignment= Center
                                        , width= 0.35
                                        , height= 0.05
                                        , pointSize= 20.0
                                        }
                                        <| langGet exportLang look.description
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
                        usdPanel scene "back" backW gather.panelH 10.0
                            { scale= Just (1, 1, 1)
                            , orient= floorOrientation
                            , translate= Just (0, gather.panelY, -0.001)
                            }
                in
                usdGroup ("Panel_"++group.id) 
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
                    [ usdGroup ("UI_"++item.id) old.uiXform <| List.concat
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
                    [ usdGroup ("UI_"++item.id) old.uiXform <| List.concat
                        [ toggleLines
                            item.id
                            "</AssetFactories/Masters/Factories/UISelectToggle50x12>"
                            (fixDescr <| langGet exportLang item.description)
                        , editLines item.id
                        , selectLines item.lookGroups
                        ]
                    , usdGroup ("Select_"++item.id) item.panelXform <|
                        usdGroup "Hideable" origin <| 
                            panelLines item.id 0.0 0.0 item.lookGroups
                    ]
            }

        Select item ->
            { uiXform= { scale= old.uiXform.scale, orient= old.uiXform.orient, translate= tUi (0.12 * size) }
            , lines= List.append old.lines <|
                List.concat
                    [ usdGroup ("UI_"++item.id) old.uiXform <| List.concat
                        [ (usdPlace 
                            ("</Root/Scenes/"++scene++"/Children/ConfigUI/Children/Hideable/Children/UI_"++item.id++"/Children/")
                            "Panel" 
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
                            { horizontalAlignment= Left
                            , width= 0.1914
                            , height= 0.03
                            , pointSize= 70.86614
                            }
                            (fixDescr <| langGet exportLang item.description)
                           )
                        :: editLines item.id
                        , selectLines item.lookGroups
                        ]
                    , usdGroup ("Select_"++item.id) item.panelXform <|
                        usdGroup "Hideable" origin <| 
                            panelLines item.id 0.0 0.0 item.lookGroups
                    ]
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
        , """string[] font = ["Helvetica"]"""
        , "token horizontalAlignment = \"" ++
            ( case attr.horizontalAlignment of
                Left -> "left"
                Center -> "center"
                Right -> "right"
                -- TODO justified?
            ) ++ "\""
        , "rel material:binding = "++matRef
        , "token verticalAlignment = \"middle\""
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
        1.0 1.0
        False


expandLook : Bool -> Look -> Look
expandLook expand look =
    { look | expandedView= expand }


changeTex : Maybe String -> String -> Look -> Look
changeTex mbNew inputId l =
    let
        newInputs= List.map
            (\input ->
                if input.id == inputId then { input | texture= mbNew } else input
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
                                                        Toggle i -> Toggle
                                                                { i 
                                                                | description= langAdd model.newLang "????" i.description
                                                                , look= (\l -> { l | description= langAdd model.newLang "????" l.description }) i.look
                                                                }
                                                        Select i -> Select <| changeWithGroups i
                                                        SelectToggle i -> SelectToggle <| changeWithGroups i
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
            [ Config sc ] ->
                let
                    scene = sc.id
                    path = "</Root/Scenes/"++scene++"/Children/"
                    objects= String.join "\n" <| List.concatMap
                        (\i ->
                            let
                                usdLookGroups item =
                                  usdGroup ("Object_"++item.id) item.xForm <| 
                                    List.concatMap
                                        (\group ->
                                            List.map 
                                                (\look -> 
                                                    usdPlace 
                                                        (path++"Object_"++item.id++"/Children/") 
                                                        ("Instance_" ++ group.id ++ "_" ++ look.id) 
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
                                    [ usdPlace path ("Object_"++item.id) Nothing item.xForm item.physics ]
                                Toggle item -> 
                                    [ usdPlace path ("Object_"++item.id) Nothing item.xForm item.physics ]
                                Select item -> 
                                    usdLookGroups item
                                SelectToggle item -> 
                                    usdLookGroups item
                        )
                        sc.items

                    gather = List.foldl 
                        ( uiForItem scene sc.uiScale sc.translations )
                        { uiXform= sc.uiXform
                        , lines= [] 
                        }
                        sc.items

                    matFactoryPrefix = "/AssetFactories/Masters/Materials"
                    factories = usdScope "AssetFactories" "( active = false )"
                            ( String.join "\n"
                                ( [ "def Scope \"Masters\" {"
                                , "def Scope \"Factories\" {"
                                , usdShapeAsset "UIPanel50x6"           "</AssetFactories/Masters/Meshes/UIPanel50x6>"          "</AssetFactories/Masters/Materials/UIPanel>"
                                , usdShapeAsset "UIToggle50x6"          "</AssetFactories/Masters/Meshes/UIToggle50x6>"         "</AssetFactories/Masters/Materials/UIPanel>"
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
                                                                in
                                                                String.join "\n"
                                                                    [ usdShapeAsset
                                                                        ("PrimitiveShapeAssetFactory_Object_" ++ suffix)
                                                                        ("</AssetFactories/Masters/Meshes/PrimitiveShapeAssetFactory_Object_"++ itemId ++"_Mesh>")
                                                                        ("</AssetFactories/Masters/Materials/PrimitiveShapeAssetFactory_Object_"++ suffix ++">")
                                                                    , usdShapeAsset 
                                                                        ("UILookButton_" ++ suffix )
                                                                        ("</AssetFactories/Masters/Meshes/PrimitiveShapeAssetFactory_Button_" ++ suffix ++"_Mesh>")
                                                                        ("</AssetFactories/Masters/Materials/Look_" ++ suffix ++">")
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
                                            Select item -> usdLookGroups item.id item.lookGroups
                                            SelectToggle item -> usdLookGroups item.id item.lookGroups
                                    )
                                    sc.items
                                )
                                ++[ "}"
                                , "def Scope \"Meshes\" {"
                                , usdMesh "UIPanel50x6"    uiPanel50x6Mesh
                                , usdMesh "UIToggle50x6"   uiToggle50x6Mesh
                                , usdMesh "UISelectToggle50x12" <| offsetPointsZ { offset= 0.06, min= 0.028, max= 0.031 } uiPanel50x6Mesh
                                , usdMesh "UISelect50x12"  uiSelect50x12Mesh
                                , usdMesh "UIEditButtonBG" uiBackgroundMesh
                                , usdMesh "UIToggleButton" uiToggleButtonMesh
                                , usdMesh "UIToggleState"  uiToggleStateMesh
                                , usdMesh "UILookButton"   uiLookButtonMesh
                                , usdMesh "UIPanelBG"      uiPanelBackgroundMesh
                                , usdMesh "UIRod35x1"      uiRod35x1Mesh
                                ]
                                ++( List.map    -- Object related meshes
                                    (\i -> case i of
                                        Static item       -> usdMesh ("PrimitiveShapeAssetFactory_Object_" ++ item.id ++ "_Mesh") item.mesh
                                        Toggle item       -> usdMesh ("PrimitiveShapeAssetFactory_Object_" ++ item.id ++ "_Mesh") item.mesh
                                        Select item       -> usdMesh ("PrimitiveShapeAssetFactory_Object_" ++ item.id ++ "_Mesh") item.mesh
                                        SelectToggle item -> usdMesh ("PrimitiveShapeAssetFactory_Object_" ++ item.id ++ "_Mesh") item.mesh
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
                                                                usdMesh ("PrimitiveShapeAssetFactory_Button_" ++ itemId ++ "_" ++ group.id ++ "_" ++ look.id ++"_Mesh") 
                                                                    <| scaleUV (look.btnScaleU, look.btnScaleV) uiLookButtonMesh 
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
                                    )
                                    sc.items
                                )
                                ++[ "}"
                                , "def Scope \"Materials\" {"
                                , usdMaterial matFactoryPrefix "UIPanel" uiPanelMaterial
                                , usdMaterial matFactoryPrefix "UIToggleActive" uiToggleActiveMaterial
                                , usdMaterial matFactoryPrefix "UIToggleInactive" uiToggleInactiveMaterial
                                , usdMaterial matFactoryPrefix "UIOpenButton" uiButtonOpenMaterial
                                , usdMaterial matFactoryPrefix "UICloseButton" uiButtonCloseMaterial
                                , usdMaterial matFactoryPrefix "UIGroupPick" uiGroupPickMaterial
                                , usdMaterial matFactoryPrefix "UIGroupActive" uiGroupActiveMaterial
                                , usdMaterial matFactoryPrefix "UIRod" uiRodMaterial
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
                                                                in
                                                                String.join "\n"
                                                                    [ usdMaterial
                                                                        matFactoryPrefix
                                                                        ("PrimitiveShapeAssetFactory_Object_"++ suffix)
                                                                        look.material
                                                                    , usdMaterial
                                                                        matFactoryPrefix
                                                                        ("Look_" ++ suffix)
                                                                        look.material
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
                                    )
                                    sc.items
                                )
                                ++[ "}" ]
                                )
                            )
                    (startActions, behaviors) = 
                      List.unzip <|
                        List.map
                            (\i ->
                                case i of
                                    Static _          -> ([], [])
                                    Toggle item       -> behaviorsForToggle scene item
                                    SelectToggle item -> behaviorsForSelectToggle scene item
                                    Select item       -> behaviorsForSelect scene item
                            )
                            sc.items
                    usda = String.join "\n"
                        (
                            [ usdHeader "TEST" ""
                            , usdSceneStart scene sc.anchor 
                            , usdMaterial ("/Root/Scenes/"++scene) "UITextBody" uiTextBodyMaterial
                            , usdMaterial ("/Root/Scenes/"++scene) "UITextActive" uiTextActiveMaterial
                            , usdMaterial ("/Root/Scenes/"++scene) "UITextInactive" uiTextInactiveMaterial
                            , usdMaterial ("/Root/Scenes/"++scene) "UIPanelMaterial" uiPanelMaterial
                            , usdMaterial ("/Root/Scenes/"++scene) "UIPanelAccent" uiPanelAccent
                            , "def Xform \"Children\" {"
                            , objects
                            , String.join "\n" <| 
                                usdGroup "ConfigUI" sc.uiXform <| 
                                    usdGroup "Hideable" origin <| 
                                        gather.lines
                            , "}"
                            , "def Scope \"Behaviors\" {" 
                            ]
                        ++( [ usdBehavior scene
                                { id= "Start"
                                , exclusive= False
                                , triggers= [ SceneTransition "enter" ]
                                , actionRoot= 
                                    Group 
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
                                                    List.filterMap
                                                        (\i ->
                                                            let
                                                                ref id = 
                                                                    "</Root/Scenes/"++scene++"/Children/ConfigUI/Children/Hideable/Children/Select_"++id++"/Children/Hideable>"
                                                            in
                                                            case i of
                                                                SelectToggle item ->
                                                                    Just (ref item.id)
                                                                Select item ->
                                                                    Just (ref item.id)
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
                        ++[ "}\n}\n}\n}"
                            , factories
                            , "}"
                            ] 
                        ++( List.map
                                (\i ->
                                    case i of
                                        Static _          -> ""
                                        Toggle item       -> xFormsForToggle sc.id item.id
                                        SelectToggle item -> xFormsForToggle sc.id item.id
                                        Select _          -> ""
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

        NewDoc ->
            ( { model | scenes= [ Config emptyScene ] }, Cmd.none )

        OpenDoc ->
            model |> withCmd (Outside.sendInfo (Outside.AskForFile "Test.cfgxr"))

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
                                    else item
                            )
                            (List.indexedMap Tuple.pair scene.items)
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
                                                        _ -> item
                                                else item
                                        )
                                        (List.indexedMap Tuple.pair scene.items)
                                }

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


viewTexture : String -> Int -> String -> String -> String -> Maybe String -> Element Msg
viewTexture scene itemIdx grpId lookId inputId mbTex =
    row [ width fill, spacing 10 ] <|
        case mbTex of
            Nothing ->
                [ Input.button btnLook
                    { onPress= Just <| 
                        ChangeLook scene itemIdx grpId lookId 
                            (\_ -> 
                                changeTex 
                                    (Just (scene++"/"++inputId++"_"++String.fromInt itemIdx++"_"++ grpId ++"_" ++lookId ++".png"))
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
                    , text= tex
                    }
                , Input.button (Element.alignRight :: btnLook)
                    { onPress= Just <| 
                        ChangeLook scene itemIdx grpId lookId 
                            (\_ -> changeTex Nothing inputId) 
                            ""
                    , label= text "No Texture"
                    }
                ]



viewGroup : String -> Dict String Bool -> Int -> LookGroup -> Element Msg
viewGroup scene languages itemIdx group =
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
        , Input.button btnLook
            { onPress= Just (AddLookToGrp itemIdx group.id)
            , label= text "Add look"
            }
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

viewItem : String -> Dict String Bool -> (Int, Item) -> Element Msg
viewItem scene languages (idx, i) =
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
                                , viewTranslations (\l-> ChangeItemDescr scene idx l) "Item Description" <| reduce languages description
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
                                List.map (viewGroup scene languages idx) item.lookGroups
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
                                List.map (viewGroup scene languages idx) item.lookGroups
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
                            { label= text "Bionics"
                            , onPress= Just LoadChairDoc
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
                            , row [ spacing 5, alignRight ] 
                                [ wrappedRow [ width fill, spacing 5 ] <| List.map
                                        (\(l,b)->
                                            Input.button 
                                                ( ( if l == exportLang then Border.color <| gray 0
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
                                    , label = text "Add Lang"
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
                                                    , xForm= origin
                                                    , physics= defaultPhysics
                                                    , mesh= cubeMesh
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
                                                    , mesh= cubeMesh
                                                    , startGrpLook= ("","")
                                                    , lookGroups= []
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
                                                    , mesh= cubeMesh
                                                    , startGrpLook= ("", "")
                                                    , lookGroups= []
                                                    , startActive= False
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
                            List.map (viewItem scene.id model.languages) <| List.indexedMap Tuple.pair scene.items
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
        |> required "thumbnail" (maybe string)
        |> required "anchor" anchorDecoder
        |> required "translations" translationDecoder
        |> required "items" (list itemDecoder)
        |> required "uiScale" float
        |> required "uiXform" xformDecoder
        |> hardcoded False


configSceneEncoder : ConfigScene -> Encode.Value 
configSceneEncoder scene =
    Encode.object
        [ ( "id", Encode.string scene.id )
        , ( "description", langEncoder scene.description )
        , ( "thumbnail", (Json.Encode.Extra.maybe Encode.string scene.thumbnail) )
        , ( "anchor", anchorEncoder scene.anchor )
        , ( "translations", translationEncoder scene.translations )
        , ( "items", Encode.list itemEncoder scene.items )
        , ( "uiScale", Encode.float scene.uiScale )
        , ( "uiXform", xformEncoder scene.uiXform )
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


translationEncoder : Translation -> Encode.Value
translationEncoder trans =
    Encode.object
        [ ( "with",     langEncoder trans.with )
        , ( "without",  langEncoder trans.without )
        , ( "open",     langEncoder trans.open )
        , ( "close",    langEncoder trans.close )
        ]


itemDecoder : Decoder Item
itemDecoder =
    Decode.oneOf 
        [ Decode.map Select selectDecoder
        , Decode.map SelectToggle selectToggleDecoder
        , Decode.map Toggle toggleDecoder
        , Decode.map Static staticDecoder
        ]


itemEncoder : Item -> Encode.Value
itemEncoder item =
    Encode.object <|
        case item of
            Static i ->
                [ ( "id",           Encode.string i.id )
                , ( "xForm",        xformEncoder i.xForm )
                , ( "mesh",         meshEncoder i.mesh )
                , ( "physics",      physicsEncoder i.physics )
                , ( "look",         lookEncoder i.look )
               ]
            Toggle i ->
                [ ( "id",           Encode.string i.id )
                , ( "description",  langEncoder i.description )
                , ( "xForm",        xformEncoder i.xForm )
                , ( "mesh",         meshEncoder i.mesh )
                , ( "physics",      physicsEncoder i.physics )
                , ( "look",         lookEncoder i.look )
                , ( "startActive",  Encode.bool i.startActive )
                ]
            SelectToggle i ->
                [ ( "id",           Encode.string i.id )
                , ( "description",  langEncoder i.description )
                , ( "xForm",        xformEncoder i.xForm )
                , ( "mesh",         meshEncoder i.mesh )
                , ( "physics",      physicsEncoder i.physics )
                , ( "startActive",  Encode.bool i.startActive )
                , ( "startOpen",    Encode.bool i.startOpen )
                , ( "lookGroups",   Encode.list lookGroupEncoder i.lookGroups )
                , ( "startGrpLook", Encode.list Encode.string [ Tuple.first i.startGrpLook, Tuple.second i.startGrpLook ] )
                , ( "rod",          Encode.string <| rodToStr i.rod )
                ]
            Select i ->
                [ ( "id",           Encode.string i.id )
                , ( "description",  langEncoder i.description )
                , ( "xForm",        xformEncoder i.xForm )
                , ( "mesh",         meshEncoder i.mesh )
                , ( "physics",      physicsEncoder i.physics )
                , ( "startOpen",    Encode.bool i.startOpen )
                , ( "lookGroups",   Encode.list lookGroupEncoder i.lookGroups )
                , ( "startGrpLook", Encode.list Encode.string [ Tuple.first i.startGrpLook, Tuple.second i.startGrpLook ] )
                , ( "rod",          Encode.string <| rodToStr i.rod )
                ]


staticDecoder : Decoder StaticItem
staticDecoder =
    Decode.succeed StaticItem
        |> required "id" string
        |> required "xForm" xformDecoder
        |> required "mesh" meshDecoder
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
        |> required "mesh" meshDecoder
        |> required "physics" physicsDecoder
        |> required "lookGroups" (list lookGroupDecoder)
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
        |> required "mesh" meshDecoder
        |> required "physics" physicsDecoder
        |> required "lookGroups" (list lookGroupDecoder)
        |> required "startGrpLook" pairDecoder
        |> required "startOpen" bool
        |> required "startActive" bool
        |> required "rod" rodDecoder
        |> hardcoded False


toggleDecoder : Decoder ToggleItem
toggleDecoder =
    Decode.succeed ToggleItem
        |> required "id" string
        |> required "description" langDecoder
        |> required "xForm" xformDecoder 
        |> required "mesh" meshDecoder
        |> required "physics" physicsDecoder
        |> required "look" lookDecoder
        |> required "startActive" bool
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
        |> required "btnScaleU" float
        |> required "btnScaleV" float
        |> hardcoded False


lookEncoder : Look -> Encode.Value
lookEncoder look =
    Encode.object
        [ ( "id",           Encode.string look.id )
        , ( "description",  langEncoder look.description )
        , ( "material",     materialEncoder look.material )
        , ( "thumbnail",    Json.Encode.Extra.maybe Encode.string look.thumbnail )
        , ( "btnScaleU",    Encode.float look.btnScaleU )
        , ( "btnScaleV",    Encode.float look.btnScaleV )
        ]


usdInputDecoder : Decoder UsdInput
usdInputDecoder =
    Decode.succeed UsdInput
        |> required "id" string
        |> required "values" string
        |> required "texture" (maybe string)


usdInputEncoder : UsdInput -> Encode.Value
usdInputEncoder i =
    Encode.object
        [ ( "id",       Encode.string i.id )
        , ( "values",   Encode.string i.values )
        , ( "texture",  Json.Encode.Extra.maybe Encode.string i.texture )
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

meshDecoder : Decoder Mesh
meshDecoder =
    Decode.succeed Mesh
        |> required "faceVertexCounts" (list int)
        |> required "faceVertexIndices" (list int)
        |> required "points" (list xyzDecoder)
        |> optional "normals" (list xyzDecoder) []
        |> optional "normalIndices" (maybe (list int)) Nothing
        |> optional "st" (list uvDecoder) []
        |> optional "stIndices" (maybe (list int)) Nothing


meshEncoder : Mesh -> Encode.Value
meshEncoder m =
    Encode.object
        [ ( "faceVertexCounts", Encode.list Encode.int m.faceVertexCounts )
        , ( "faceVertexIndices",Encode.list Encode.int m.faceVertexIndices )
        , ( "points",           Encode.list xyzEncoder m.points )
        , ( "normals",          Encode.list xyzEncoder m.normals )
        , ( "normalIndices",    Json.Encode.Extra.maybe (Encode.list Encode.int) m.normalIndices )
        , ( "st",               Encode.list uvEncoder m.st )
        , ( "stIndices",        Json.Encode.Extra.maybe (Encode.list Encode.int) m.stIndices)
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


langDecoder : Decoder LanguageString
langDecoder =
    keyValuePairs string |> Decode.map Dict.fromList


langEncoder : LanguageString -> Encode.Value
langEncoder ls =
    Encode.dict identity Encode.string ls

{- --------------------------------- -}



{-
elm make src/Main.elm --output=public/Main.js
yarn tauri dev
-}