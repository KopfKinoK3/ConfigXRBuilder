module TestScene exposing (testScene, cubeMesh)

import Dict exposing (Dict)
import ConfigXR.Types exposing (..)
import ConfigXR.Helper exposing (..)

testScene : ConfigScene
testScene =
    { id= "Scene"
    , description= Dict.fromList [ ("en", "Sample Scene description"), ( "de", "Beispiel Szenenbeschreibung") ]
    , thumbnail= Nothing
    , expandedView= False
    , uiScale= 1.0
    , uiXform= { scale= Just (1, 1, 1), orient = Just [1, 0, 0, 0], translate= Just (0,0,0.15) }
    , anchor= Horizontal
    , translations= defaultTranslations
    , items=
        [ Toggle 
            { id= "Cube1"
            , description= Dict.fromList [ ("de", "Wuerfel 1"), ("en", "Cube 1") ]
            , expandedView= False
            , xForm=
                { scale= Just (1,1,1)
                , orient= verticalOrientation
                , translate= Just (0,0.05,0)
                }
            , mesh=cubeMesh
            , physics=
                { colliderShape= Cube 1
                , xForm=
                    { scale= Just (0.1, 0.1, 0.1)
                    , orient= verticalOrientation
                    , translate= Just ( 0, 0, 0)
                    }
                , extent= [(-0.05, -0.05, -0.05), (0.05, 0.05, 0.05)]
                , frictionDynamic= 0.58
                , frictionStatic = 0.58
                , restitution = 0.48
                } 
            , startActive= False
            , look=   
                { id="look1"
                , description=Dict.fromList [ ("de", "Beschreibung"), ("en", "Description") ]
                , expandedView= False
                , thumbnail= Nothing
                , btnScaleU= 1.0
                , btnScaleV= 1.0
                , material=
                    { id= "mat1"
                    , inputs=
                        [ UsdInput "diffuseColor"   "(0.56452835, 0.56452835, 0.56452835)" Nothing
                        , UsdInput "emissiveColor"  "(0, 0, 0)" Nothing
                        , UsdInput "metallic"       "0.5" Nothing
                        , UsdInput "roughness"      "0.28" Nothing
                        ]
                    }
                }
            }
        , SelectToggle 
            { id= "Cube2"
            , description= Dict.fromList [ ("de", "Wuerfel 2"), ("en", "Cube 2") ]
            , rod= West
            , expandedView= False
            , xForm=
                { scale= Just (1,1,1)
                , orient= verticalOrientation
                , translate= Just (-0.15,0.05,0)
                }
            , panelXform=
                { scale= Just (1, 1, 1)
                , orient= verticalOrientation
                , translate= Just (-0.15, 0.15, 0.25)
                }
            , mesh= cubeMesh
            , physics=
                { colliderShape= Cube 1
                , xForm=
                    { scale= Just (0.1, 0.1, 0.1)
                    , orient= verticalOrientation
                    , translate= Just ( 0, 0, 0)
                    }
                , extent= [(-0.05, -0.05, -0.05), (0.05, 0.05, 0.05)]
                , frictionDynamic= 0.58
                , frictionStatic = 0.58
                , restitution = 0.48
                } 
            , startActive= False
            , startOpen= False
            , startGrpLook= ("Grp1", "lookA1")
            , lookGroups= 
              [ { id="Grp1"
                , description= Dict.fromList [ ("de", "Gruppe A"), ("en", "Group A") ]
                , looks=
                  [ { id="lookA1"
                    , expandedView= False
                    , description= Dict.fromList [ ("de", "Beschreibung 1"), ("en", "Description 1") ]
                    , thumbnail= Nothing
                    , btnScaleU= 1.0
                    , btnScaleV= 1.0
                    , material=
                        { id= "matA1"
                        , inputs=
                            [ UsdInput "diffuseColor" "(0.56452835, 0, 0)" Nothing
                            , UsdInput "emissiveColor" "(0, 0, 0)" Nothing
                            , UsdInput "metallic" "1" Nothing
                            , UsdInput "roughness" "0.28" Nothing
                            ]
                        }
                    }
                  , { id="lookA2"
                    , description=Dict.fromList [ ("de", "Beschreibung 2"), ("en", "Description 2") ]
                    , expandedView= False
                    , thumbnail= Nothing
                    , btnScaleU= 1.0
                    , btnScaleV= 1.0
                    , material=
                        { id= "matA2"
                        , inputs=
                            [ UsdInput "diffuseColor" "(0, 0.56452835, 0)" Nothing
                            , UsdInput "emissiveColor" "(0, 0, 0)" Nothing
                            , UsdInput "metallic" "1" Nothing
                            , UsdInput "roughness" "0.28" Nothing
                            ]
                        }
                    }
                  , { id="lookA3"
                    , description=Dict.fromList [ ("de", "Beschreibung 3"), ("en", "Description 3") ]
                    , expandedView= False
                    , thumbnail= Nothing
                    , btnScaleU= 1.0
                    , btnScaleV= 1.0
                    , material=
                        { id= "matA3"
                        , inputs=
                            [ UsdInput "diffuseColor" "(0, 0, 0.56452835)" Nothing
                            , UsdInput "emissiveColor" "(0, 0, 0)" Nothing
                            , UsdInput "metallic" "1" Nothing
                            , UsdInput "roughness" "0.28" Nothing
                            ]
                        }
                    }
                  , { id="lookA4"
                    , description=Dict.fromList [ ("de", "Beschreibung 4"), ("en", "Description 4") ]
                    , expandedView= False
                    , thumbnail= Nothing
                    , btnScaleU= 1.0
                    , btnScaleV= 1.0
                    , material=
                        { id= "matA4"
                        , inputs=
                            [ UsdInput "diffuseColor" "(0, 0.75, 0.75)" Nothing
                            , UsdInput "emissiveColor" "(0, 0, 0)" Nothing
                            , UsdInput "metallic" "0" Nothing
                            , UsdInput "roughness" "1" Nothing
                            ]
                        }
                    }
                  ]
                }
              ]
            }
        , SelectToggle 
            { id= "Cube3"
            , description= Dict.fromList [ ("de", "Wuerfel 3"), ("en", "Cube 3") ]
            , rod= East
            , expandedView= False
            , xForm=
                { scale= Just (1,1,1)
                , orient= verticalOrientation
                , translate= Just (-1,0.05,0)
                }
            , panelXform=
                { scale= Just (1, 1, 1)
                , orient= verticalOrientation
                , translate= Just (-1, 0.15, 0.25)
                }
            , mesh= cubeMesh
            , physics=
                { colliderShape= Cube 1
                , xForm=
                    { scale= Just (0.1, 0.1, 0.1)
                    , orient= verticalOrientation
                    , translate= Just ( 0, 0, 0)
                    }
                , extent= [(-0.05, -0.05, -0.05), (0.05, 0.05, 0.05)]
                , frictionDynamic= 0.58
                , frictionStatic = 0.58
                , restitution = 0.48
                } 
            , startActive= False
            , startOpen= False
            , startGrpLook= ("Grp1", "lookA1")
            , lookGroups= 
              [ { id="Grp1"
                , description= Dict.fromList [ ("de", "Gruppe A"), ("en", "Group A") ]
                , looks=
                  [ { id="lookA1"
                    , expandedView= False
                    , description= Dict.fromList [ ("de", "Beschreibung 1"), ("en", "Description 1") ]
                    , thumbnail= Nothing
                    , btnScaleU= 1.0
                    , btnScaleV= 1.0
                    , material=
                        { id= "matA1"
                        , inputs=
                            [ UsdInput "diffuseColor" "(0.56452835, 0, 0)" Nothing
                            , UsdInput "emissiveColor" "(0, 0, 0)" Nothing
                            , UsdInput "metallic" "1" Nothing
                            , UsdInput "roughness" "0.28" Nothing
                            ]
                        }
                    }
                  , { id="lookA2"
                    , description=Dict.fromList [ ("de", "Beschreibung 2"), ("en", "Description 2") ]
                    , expandedView= False
                    , thumbnail= Nothing
                    , btnScaleU= 1.0
                    , btnScaleV= 1.0
                    , material=
                        { id= "matA2"
                        , inputs=
                            [ UsdInput "diffuseColor" "(0, 0.56452835, 0)" Nothing
                            , UsdInput "emissiveColor" "(0, 0, 0)" Nothing
                            , UsdInput "metallic" "1" Nothing
                            , UsdInput "roughness" "0.28" Nothing
                            ]
                        }
                    }
                  , { id="lookA3"
                    , description=Dict.fromList [ ("de", "Beschreibung 3"), ("en", "Description 3") ]
                    , expandedView= False
                    , thumbnail= Nothing
                    , btnScaleU= 1.0
                    , btnScaleV= 1.0
                    , material=
                        { id= "matA3"
                        , inputs=
                            [ UsdInput "diffuseColor" "(0, 0, 0.56452835)" Nothing
                            , UsdInput "emissiveColor" "(0, 0, 0)" Nothing
                            , UsdInput "metallic" "1" Nothing
                            , UsdInput "roughness" "0.28" Nothing
                            ]
                        }
                    }
                  ]
                }
              ]
            }
        ]
    }

cubeMesh : Mesh
cubeMesh =
    { faceVertexCounts= [4, 4, 4, 4, 4, 4]
    , faceVertexIndices= [0, 2, 3, 1, 4, 6, 7, 5, 2, 7, 6, 3, 0, 1, 4, 5, 0, 5, 7, 2, 1, 3, 6, 4]
    , points=
        [(0.05, -0.05, 0.05), (-0.05, -0.05, 0.05), (0.05, 0.05, 0.05), (-0.05, 0.05, 0.05), (-0.05, -0.05, -0.05), (0.05, -0.05, -0.05), (-0.05, 0.05, -0.05), (0.05, 0.05, -0.05)]
    , normals=
        [ (0, 0, 1), (0, -1, -0), (0, 1, 0), (0, 0, 1), (0, 0, -1), (0, -1, -0), (0, 1, 0), (0, 0, -1), (1, -0, 0), (0, -1, -0), (-1, 0, -0), (0, 0, 1), (1, -0, 0), (0, 0, 1), (-1, 0, -0)
        , (0, 1, 0), (-1, 0, -0), (0, -1, -0), (1, -0, 0), (0, 0, -1), (-1, 0, -0), (0, 0, -1), (1, -0, 0), (0, 1, 0)
        ]
    , normalIndices= Just [0, 13, 3, 11, 4, 21, 7, 19, 2, 23, 6, 15, 9, 1, 17, 5, 8, 18, 22, 12, 10, 14, 20, 16]
    , st=
        [ (0.57, 0.502), (0.57, 0.55), (0.57, 0.55), (0.43, 0.498), (0.57, 0.502), (0.43, 0.45), (0.43, 0.45), (0.43, 0.498), (0.45, 0.502), (0.43, 0.55), (0.55, 0.502), (0.43, 0.502)
        , (0.45, 0.498), (0.57, 0.498), (0.55, 0.498), (0.43, 0.55), (0.45, 0.502), (0.57, 0.45), (0.55, 0.502), (0.43, 0.502), (0.45, 0.498), (0.57, 0.498), (0.55, 0.498), (0.57, 0.45)
        ]
    , stIndices= Just [0, 13, 3, 11, 4, 21, 7, 19, 2, 23, 6, 15, 9, 1, 17, 5, 8, 18, 22, 12, 10, 14, 20, 16]
    }
