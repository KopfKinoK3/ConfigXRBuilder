module ConfigXR.Types exposing
    ( Anchor(..)
    , AnchorImage
    , AnimationMeta
    , SubdivisionScheme(..)
    , Interpolation(..)
    , Texture, defaultTx, scaledTx
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
    , Item(..)
    , Translation
    , Product
    , Form

    , ConfigScene
    , InfoScene
    , Panel
    , Page
    , UsdTrigger(..)
    , UsdAction (..)
    , UsdBehavior

    , newLook
    , defaultMesh
    , defaultPhysics
    , defaultTranslations
    , defaultAnimMeta
    , origin
    , verticalOrientation, verticalOrientationBlender
    , floorOrientation
    )

import Dict exposing (Dict)
import Element.Region exposing (description)


type alias AnchorImage =
    { filePath : String
    , physicalWidth : Float
    }


type Anchor 
    = Horizontal
    | Vertical
    | Image AnchorImage


type Interpolation
    = Uniform       -- One element for each face of the mesh; elements are typically not interpolated but are inherited by other faces derived from a given face (via subdivision, tessellation, etc.).
    | Varying       -- One element for each point of the mesh; interpolation of point data is always linear.
    | FaceVarying   -- One element for each of the face-vertices that define the mesh topology; interpolation of face-vertex data may be smooth or linear, according to the subdivisionScheme and faceVaryingLinearInterpolation attributes.
    | Vertex        -- One element for each point of the mesh; interpolation of point data is applied according to the subdivisionScheme attribute.
    | Constant      -- One element for the entire mesh; no interpolation.


type SubdivisionScheme
    = None          -- No subdivision, i.e. a simple polygonal mesh; interpolation of point data is linear
    | CatmullClark  -- The default, Catmull-Clark subdivision; preferred for quad-dominant meshes (generalizes B-splines); interpolation of point data is smooth (non-linear)
    | Loop          -- Loop subdivision; preferred for purely triangular meshes; interpolation of point data is smooth (non-linear)
    | Bilinear      -- Subdivision reduces all faces to quads (topologically similar to "catmullClark"); interpolation of point data is bilinear

type alias SkelAnim =
    { joints : List String
    , bindTransforms : List (List (List Float))
    , restTransforms : List (List (List Float))
    , rotations : List (Float, List (List Float))
    , translations : List (Float, List (Float, Float, Float))
    , scales : List (Float, List (Float, Float, Float))
    
    , jointIndices : List Int
    , jointWeights : List Float
    , elementSize : Int
    , geomBindTransform : List (List Float)
    }

type alias AnimationMeta =
    { timeCodesPerSecond : Float
    , startTimeCode : Float
    , endTimeCode : Float
    , autoPlay : Bool
    }

type alias Mesh =
    { faceVertexCounts : List Int
    , faceVertexIndices : List Int
    , points : List ( Float, Float, Float)
    , normals : List ( Float, Float, Float)
    , normalIndices : Maybe (List Int)
    , normalsInterpolation : Interpolation
    , st : List (Float, Float)
    , stIndices : Maybe (List Int)
    , stInterpolation : Interpolation
    , st1 : List (Float, Float)
    , st1Indices : Maybe (List Int)
    , st1Interpolation : Interpolation
    , subdivisionScheme : SubdivisionScheme
    , noExtend : Bool
    , doubleSided : Bool
    }


type alias Xform =
    { scale : Maybe (Float, Float, Float)
    , orient : Maybe (List Float)
    , translate : Maybe (Float, Float, Float)
    }


type alias Material =
    { id: String
    , inputs: List UsdInput
    }


type ColliderShape
    = Cube Float
    | Capsule { axis: String, height: Float, radius: Float }


type alias Physics =
    { colliderShape: ColliderShape
    , xForm: Xform
    , extent: List (Float, Float, Float)
    , frictionDynamic: Float
    , frictionStatic: Float
    , restitution: Float
    }


type HorAlign = Left | Center | Right


type VerAlign = Top | Middle | Bottom


type alias TextAttr =
    { width: Float
    , height: Float
    , pointSize: Float
    , horizontalAlignment: HorAlign
    , verticalAlignment: VerAlign
    }


type alias Texture =
    { file: String
    , pixelBias: Maybe (List Float)
    , pixelScale: Maybe (List Float)
    , useSecondUV: Bool
    , scale: Maybe (List Float)
    , rotation: Maybe Float
    , translation: Maybe (List Float)
    }

defaultTx : String -> Int -> Texture
defaultTx path mapNr =
    Texture path Nothing Nothing (mapNr == 1) Nothing Nothing Nothing

scaledTx : String -> Int -> Float -> Texture
scaledTx path mapNr scale =
    Texture path Nothing Nothing (mapNr == 1) (Just [scale, scale]) Nothing Nothing


type alias UsdInput =
    { id: String
    , values: String
    , texture: Maybe Texture
    }


type alias Look =
    { id: String
    , description: LanguageString
    , material: Material
    , thumbnail: Maybe String
    , shopId: Maybe String
    , btnScaleU: Float
    , btnScaleV: Float
    , meshId: String
    , expandedView: Bool
    }


type alias LookGroup =
    { id: String
    , description: LanguageString
    , looks: List Look
    }


type RodOrient
    = North
    | NorthEast
    | East
    | SouthEast
    | South
    | SouthWest
    | West
    | NorthWest


type alias ToggleItem =
    { id: String
    , description: LanguageString
    , xForm: Xform
    , mesh: Mesh
    , skelAnim : Maybe SkelAnim
    , physics: Physics
    , look: Look
    , startActive: Bool
    , shopIds: Maybe (String, String)
    , children: List StaticItem
    , expandedView: Bool
    }


type alias SelectItem =
    { id: String
    , description: LanguageString
    , xForm: Xform
    , panelXform: Xform
    , meshesById: Dict String Mesh
    , skelAnim : Maybe SkelAnim
    , physics: Physics
    , lookGroups: List LookGroup
    , lookGroupsOffset: Float
    , startGrpLook: ( String, String )
    , startOpen: Bool
    , rod: RodOrient
    , expandedView: Bool
    }


type alias SelectToggleItem =
    { id: String
    , description: LanguageString
    , xForm: Xform
    , panelXform: Xform
    , meshesById: Dict String Mesh
    , skelAnim : Maybe SkelAnim
    , physics: Physics
    , lookGroups: List LookGroup
    , lookGroupsOffset: Float
    , startGrpLook: ( String, String )
    , startOpen: Bool
    , startActive: Bool
    , shopIdForNone: Maybe String
    , children: List StaticItem
    , rod: RodOrient
    , expandedView: Bool
    }


type alias StaticItem =
    { id: String
    , xForms: List Xform
    , mesh: Mesh
    , skelAnim : Maybe SkelAnim
    , physics: Physics
    , look: Look
    , expandedView: Bool
    }


type alias Product =
    { description: LanguageString
    , url: LanguageString
    , product: String
    , ean: String
    , form: String
    , look: String
    }

type alias Form =
    { id: String
    , thumbnail: String
    , cfgMesh: Mesh
    , extraInputs: List UsdInput
    , animatable: Bool
    , description: LanguageString
    }

type alias MultiItem =
    { id: String
    , description: LanguageString
    , physics: Physics
    , xForm: Xform
    , panelXform: Xform
    , matriXform: Xform
    , startProduct: String
    , products: List Product
    , looks: List Look
    , forms: List Form
    , columns: Int
    , panelW: Float
    , panelH: Float
    , matrixW: Float
    , matrixH: Float
    , expandedView: Bool
    }

type Item
    = Toggle ToggleItem
    | Select SelectItem
    | SelectToggle SelectToggleItem
    | Static StaticItem
    | Multi MultiItem
    -- TODO switch      | Switch (List StaticItem)
    -- TODO switch      | Switch (List StaticItem)
    -- TODO follow      | Follow String SelectItem
    -- TODO follow      | FollowToggle String SelectToggleItem
    -- TODO variants    | VariantToggle (Dict (String, ToggleItem))
    -- TODO variants    | VariantSelect (Dict (String, SelectItem))
    -- TODO variants    | VariantSelectToggle (Dict (String, SelectToggleItem))
    -- TODO variants    | VariantStatic (Dict (String, StaticItem))


type alias Page =
    { id: String
    , imagePath: String
    , imageWidth: Int
    , imageHeight: Int 
    , bodyText: LanguageString
    , expandedView: Bool
    }


type alias Panel =
    { id: String
    , btnXform: Xform
    , uiXform: Xform
    , startOpen: Bool
    , physics: Physics
    , title: LanguageString
    , subTitle: LanguageString
    , pages: List Page
    , startIdx: Int
    , expandedView: Bool
    }


type alias InfoScene =
    { id : String
    , description : LanguageString
    , panels : List Panel
    , uiScale : Float
    , uiXform : Xform
    , hint : LanguageString
    , anchor : Anchor
    , expandedView : Bool
    }


type alias LanguageString= 
    Dict String String


type alias Translation =
    { with : LanguageString
    , without : LanguageString
    , open : LanguageString
    , close : LanguageString
    , hint : LanguageString
    , order : LanguageString
    , openSummary : LanguageString
    , closeSummary : LanguageString
    , explain0 : LanguageString
    , explain1 : LanguageString
    , explain2 : LanguageString
    , explain3 : LanguageString
    , notAvailable : LanguageString
    }


type alias ConfigScene =
    { id : String
    , description : LanguageString
    , shopUrl : LanguageString
    , thumbnail : Maybe String
    , anchor : Anchor
    , translations : Translation
    , items : List Item
    , uiScale : Float
    , uiXform : Xform
    , codeXform : Xform
    , urlPrefix : String
    , urlAR : String
    , urlPreview : String
    , ralId : String
    , ralSuffix : String
    , animMeta : AnimationMeta
    -- TODO variants    , variants : Maybe (List (String, LanguageString))
    , expandedView : Bool
    }


type UsdTrigger
    = TapGesture String
    | SceneTransition String
    | Notification String


type UsdAction
    = Visibility { show: Bool, duration: Float, affected: (List String) }
    | Group { parallel: Bool, loop: Bool, performCount: Int, actions: List (String, UsdAction) }
    | Transform { absolute: Bool, duration: Float, xformTarget: String, affected: String }
    | LookAtCamera { duration: Float, front: (Float, Float, Float), upVector: (Float, Float, Float), affected: List String }
    | StartAnimation { affected: List String }
    | Notify { affected: List String, identifier: String }


type alias UsdBehavior =
    { id: String
    , actionRoot: UsdAction
    , triggers: List UsdTrigger
    , exclusive: Bool
    }


-----

defaultMesh=
    { faceVertexCounts= []
    , faceVertexIndices= []
    , points= []
    , normals= []
    , normalIndices= Nothing
    , normalsInterpolation= Vertex
    , st= []
    , stIndices= Nothing
    , stInterpolation= Vertex
    , st1= []
    , st1Indices= Nothing
    , st1Interpolation= Vertex
    , subdivisionScheme= None
    , noExtend= False
    , doubleSided= False
    }


defaultPhysics : Physics
defaultPhysics =
    { colliderShape= Cube 1
    , xForm= origin
    , extent= [(-0.05, -0.05, -0.05), (0.05, 0.05, 0.05)]
    , frictionDynamic= 0.58
    , frictionStatic= 0.58
    , restitution= 0.48
    }


defaultMaterial : Material
defaultMaterial =
    { id= "matXX"
    , inputs=
        [ UsdInput "diffuseColor" "(0, 0, 0.56452835)" Nothing
        --, UsdInput "emissiveColor" "(0, 0, 0)" Nothing
        , UsdInput "metallic" "1" Nothing
        , UsdInput "roughness" "0.28" Nothing
        ]
    }


defaultAnimMeta : AnimationMeta
defaultAnimMeta =
    { timeCodesPerSecond = 60
    , startTimeCode = 0
    , endTimeCode = 0
    , autoPlay = False
    }


newLook : Look
newLook =
    { id="lookXX"
    , description= Dict.fromList [ ("en", "New material/colour"), ("de", "Weiteres Material/Farbe") ]
    , material= defaultMaterial
    , thumbnail= Nothing
    , shopId= Nothing
    , btnScaleU= 1.0
    , btnScaleV= 1.0
    , meshId= "normal"
    , expandedView= False
    }


floorOrientation            = Just [ 0.7071068, -0.7071068, 0, 0 ]
verticalOrientation         = Just [ 1, 0, 0, 0 ]
verticalOrientationBlender  = Just [ 0, 0, 0.7071068, -0.7071068 ]


origin : Xform
origin =
    { scale= Just (1,1,1)
    , orient= verticalOrientation
    , translate= Just (0,0,0)
    }


defaultTranslations=
    { with=     Dict.fromList [ ("en", "with"),  ("de", "mit")  , ("pl", "Tak") ]
    , without=  Dict.fromList [ ("en", "w/out"), ("de", "ohne") , ("pl", "Nie") ]
    , open=
        Dict.fromList
            [ ("en", "Change")
            , ("de", "Anpassen")
            , ("pl", "Przypasować")
            --, ("pl", "Przypasowac")
            ]
    , close=
        Dict.fromList
            [ ("en", "Finish")
            , ("de", "Schliessen")
            , ("pl", "Gotów")
            --, ("pl", "Gotow")
            ]
    , hint=
        Dict.fromList
            [ ("en", "Change options below")
            , ("de", "Wählen Sie hier die Ausstattung")
            , ("pl", "Zmień opcje poniżej")
            --, ("pl", "Zmien opcje ponizej")
            ]
    , order=
        Dict.fromList 
            [ ("en", "Learn more about your customizations and how to order here:")
            , ("de", "Erfahren Sie mehr zu Ihren Anpassungen und bestellen Sie unter:")
            , ("pl", "Zrób zdjęcie i użyj go, aby odwiedzić ten link:")
            --, ("pl", "Zrob zdjecie i uzyj go, aby odwiedzic ten link:")
            ]
    , openSummary=
        Dict.fromList
            [ ("en", "order this setup")
            , ("de", "Im Web bestellen")
            , ("pl", "Ucz się więcej")
            --, ("pl", "Ucz sie wiecej")
            ]
    , closeSummary=
        Dict.fromList
            [ ("en", "return")
            , ("de", "zurück")
            , ("pl", "wstecz")
            ]
    , explain0=
        Dict.fromList
            [ ("en", "As of yet you cannot simply tap on the link above. Until this is supported, please use one of the following methods to place an order:")
            , ("de", "Bisher können Sie diesen Link leider nicht direkt antippen. Bis es so weit ist, nutzen Sie bitte einen der folgenden Wege zum Bestellen:")
            , ("pl", "Na razie nie możesz dotknąć powyższego linku. Dopóki nie jest to obsługiwane, użyj jednej z następujących metod, aby złożyć zamówienie:")
            ]
    , explain1=
        Dict.fromList
            [ ("en", "Enter the URL in a browser of your choice.")
            , ("de", "Geben Sie die URL in einen Browser Ihrer Wahl ein.")
            , ("pl", "Wprowadź adres URL w wybranej przeglądarce.")
            ]
    , explain2=
        Dict.fromList
            [ ("en", "Visit the URL „<URL>“\nand enter the config code in your custom URL following the „?“.")
            , ("de", "Besuchen Sie „<URL>“ und geben dort den Code ein, der in der URL nach dem „?“ steht.")
            , ("pl", "Odwiedź „<URL>” i wprowadź znaki w niestandardowym adresie URL po „?”.")
            ]
    , explain3=
        Dict.fromList
            [ ("en", "(iOS 15 or newer) Open a screenshot of this scene in your „Photos“-App.\nTap the button „Live Text“ in the bottom right corner.\nNow you can tap the link and place your order.")
            , ("de", "(Ab iOS 15) Folgen Sie dem Link, indem Sie einen Schnappschuss dieser Szene in Ihrer „Fotos“-App öffnen.\nTippen Sie rechts unten auf den Button „Live-Text“.\nDamit wird der Link antippbar.")
            , ("pl", "(iOS 15 lub nowszy) Otwórz zrzut ekranu tej sceny w aplikacji „Zdjęcia”. Naciśnij przycisk „Live Text” w prawym dolnym rogu.\nTeraz możesz kliknąć link i złożyć zamówienie.")
            --, ("pl", "Lub po prostu zrob jej migawke i otworz ja w aplikacji do zdjec, ktora rozpoznaje tekst i umozliwia klikniwcie linku.\nZ systemu iOS15 za pomoca nowego przycisku tekstu na zywo w prawym dolnym rogu.")
            ]
    , notAvailable=
        Dict.fromList
            [ ("de", "nicht verfügbar")
            , ("en", "not available")
            ]
    }
