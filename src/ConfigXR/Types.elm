module ConfigXR.Types exposing
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

import Dict exposing (Dict)

type alias AnchorImage =
    { filePath : String
    , physicalWidth : Float
    }


type Anchor 
    = Horizontal
    | Vertical
    | Image AnchorImage


type alias Mesh =
    { faceVertexCounts : List Int
    , faceVertexIndices : List Int
    , points : List ( Float, Float, Float)
    , normals : List ( Float, Float, Float)
    , normalIndices : Maybe (List Int)
    , st : List (Float, Float)
    , stIndices : Maybe (List Int)
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

type alias TextAttr =
    { width: Float
    , height: Float
    , pointSize: Float
    , horizontalAlignment: HorAlign
    }


type alias UsdInput =
    { id: String
    , values: String
    , texture: Maybe String
    }


type alias Look =
    { id: String
    , description: LanguageString
    , material: Material
    , thumbnail: Maybe String
    , btnScaleU: Float
    , btnScaleV: Float
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
    , physics: Physics
    , look: Look
    , startActive: Bool
    , expandedView: Bool
    }


type alias SelectItem =
    { id: String
    , description: LanguageString
    , xForm: Xform
    , panelXform: Xform
    , mesh: Mesh
    , physics: Physics
    , lookGroups: List LookGroup
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
    , mesh: Mesh
    , physics: Physics
    , lookGroups: List LookGroup
    , startGrpLook: ( String, String )
    , startOpen: Bool
    , startActive: Bool
    , rod: RodOrient
    , expandedView: Bool
    }


type alias StaticItem =
    { id: String
    , xForm: Xform
    , mesh: Mesh
    , physics: Physics
    , look: Look
    , expandedView: Bool
    }


type Item
    = Toggle ToggleItem
    | Select SelectItem
    | SelectToggle SelectToggleItem
    | Static StaticItem


type alias LanguageString= 
    Dict String String


type alias Translation =
    { with : LanguageString
    , without : LanguageString
    , open : LanguageString
    , close : LanguageString
    }


type alias ConfigScene =
    { id : String
    , description : LanguageString
    , thumbnail : Maybe String
    , anchor : Anchor
    , translations : Translation
    , items : List Item
    , uiScale : Float
    , uiXform : Xform
    , expandedView : Bool
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
        , UsdInput "emissiveColor" "(0, 0, 0)" Nothing
        , UsdInput "metallic" "1" Nothing
        , UsdInput "roughness" "0.28" Nothing
        ]
    }


newLook : Look
newLook =
    { id="lookXX"
    , description= Dict.fromList [ ("en", "New material/colour"), ("de", "Weiteres Material/Farbe") ]
    , material= defaultMaterial
    , thumbnail= Nothing
    , btnScaleU= 1.0
    , btnScaleV= 1.0
    , expandedView= False
    }


floorOrientation = Just [ 0.7071083, -0.7071052, 0, 0 ]
verticalOrientation = Just [ 1, 0, 0, 0]


origin : Xform
origin =
    { scale= Just (1,1,1)
    , orient= verticalOrientation
    , translate= Just (0,0,0)
    }


defaultTranslations=
    { with=     Dict.fromList [ ("en", "with"),      ("de", "mit")      ]
    , without=  Dict.fromList [ ("en", "w/out"),     ("de", "ohne")     ]
    , open=     Dict.fromList [ ("en", "Change"),    ("de", "Anpassen") ]
    , close=    Dict.fromList [ ("en", "Finish"),    ("de", "Fertig")   ]
    }


