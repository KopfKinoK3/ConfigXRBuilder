module ConfigXR.RALProject exposing (configScene)

import Dict exposing (Dict)
import ConfigXR.Types exposing (..)
import ConfigXR.Helper exposing (..)

import TestScene exposing (cubeMesh)

occ : List UsdInput
occ =
  []
  --[ UsdInput "occlusion" "0.0" (Just "ao/RAL_Mesh.jpg") ]

shiny= [ UsdInput "roughness" "0.5" Nothing ] -- glänzend
metallic = [ UsdInput "roughness" "0.4" Nothing ]

{-
incubado GmbH
Werner-von-Siemens-Str. 3-7
25479 Ellerau
Deutschland

Tel.: 040 / 882 159 77
E-Mail: hi@incubado.de

machen auch:
https://de.spray.bike/collections/alle-farben   https://de.spray.bike/pages/farben
-}

configScene : ConfigScene
configScene=
    { id= "cosmoslacRAL"
    , urlPrefix= "https://cosmoslac.de/products/ral?variant=" 
    , urlPreview= "https://www.kreativekommunikationskonzepte.de/wp-content/uploads/ar/RAL_preview.png"
    , urlAR= "https://www.kreativekommunikationskonzepte.de/wp-content/uploads/ar/RAL_LANGUAGE.reality"
    , description= Dict.fromList 
        [ ("de", "RAL Farbwahl")
        , ("en", "RAL Colour Picker")
        ]
    , shopUrl= Dict.fromList
        [ ("de", "https://cosmoslac.de/products/ral?variant=")
        ]
    , thumbnail= Nothing
    , expandedView= True
    , uiScale= 1.0
    , uiXform= { scale= Just (1, 1, 1), orient= Just [1, 0, 0, 0], translate= Just (0,0,1) }
    , codeXform= { scale= Just (1, 1, 1), orient= Just [0.8737643, 0.48634967, 0, 0], translate= Just (0,0.1,0) }
    , anchor= Horizontal
    , translations= defaultTranslations
    , ralId= ""
    , ralSuffix= ""
    , animMeta= defaultAnimMeta
    , items=[]
{-
        [ Select
            { id= "Painting"
            , description= Dict.fromList
                [ ("en", "RAL Spray painting")
                , ("de", "RAL Sprühdose")
                ]
            , rod= West
            , expandedView= True
            , mesh= cubeMesh
            , physics= defaultPhysics
            , xForm=
                { scale= Just (1, 1, 1)
                , orient= Just [0.7071068, -0.7071067, 0, 0]
                , translate= Just (-0.8, 0.3571, 0)
                }
            , panelXform=
                { scale= Just (1, 1, 1)
                , orient= verticalOrientation
                , translate= Just (0, 0.8, 0.35)
                }
            , startOpen= True
            , lookGroupsOffset= 0.05

            , startGrpLook= ("B", "BA")
            , lookGroups=
                [ { id= "A"
                  , description= Dict.fromList [ ("en", "Yellow"), ("de", "Gelb") ]
                  , looks=
                      [ ralLook "AA" "1007" (Just "41041258971316") shiny ""
                      , ralLook "AB" "1014" (Just "41041259004084") shiny ""
                      , ralLook "AC" "1015" (Just "41041259561140") shiny ""
                      , ralLook "AD" "1018" (Just "41041259593908") shiny ""
                      , ralLook "AE" "1023" (Just "41041259692212") shiny ""
                      , ralLook "AF" "1028" (Just "41041259430068") shiny ""
                      ]
                  }
                , { id= "B"
                  , description= Dict.fromList [ ("de", "Orange") ]
                  , looks=
                      [ ralLook "BA" "2003" (Just "41041259724980") shiny ""
                      , ralLook "BB" "2010" (Just "41041259036852") shiny ""
                      ]
                  }
                , { id= "C"
                  , description= Dict.fromList [ ("de", "Rot"), ("en", "Red") ]
                  , looks=
                      [ ralLook "CA" "3000" (Just "41041259495604") shiny ""
                      , ralLook "CB" "3003" (Just "41041259364532") shiny ""
                      , ralLook "CC" "3004" (Just "41041259069620") shiny ""
                      , ralLook "CD" "3009" (Just "41041260118196") shiny ""
                      , ralLook "CE" "3015" (Just "41041259823284") shiny ""
                      , ralLook "CF" "3020" (Just "41041259954356") shiny ""
                      ]
                  }
                , { id= "D"
                  , description= Dict.fromList [ ("de", "Violett"), ("en", "Lilac") ]
                  , looks=
                      [ ralLook "DA" "4003" (Just "41041259528372") shiny ""
                      , ralLook "DB" "4005" (Just "41041259462836") shiny ""
                      ]
                  }
                , { id= "E"
                  , description= Dict.fromList [ ("de", "Blau"), ("en", "Blue") ]
                  , looks=
                      [ ralLook "EA" "5010" (Just "41041260249268") shiny ""
                      , ralLook "EB" "5012" (Just "41041259888820") shiny ""
                      , ralLook "EC" "5017" (Just "41041259987124") shiny ""
                      ]
                  }
                , { id= "F"
                  , description= Dict.fromList [ ("de", "Grün"), ("en", "Gree") ]
                  , looks=
                      [ ralLook "FA" "6001" (Just "41041259102388") shiny ""
                      , ralLook "FB" "6002" (Just "41041260019892") shiny ""
                      , ralLook "FC" "6005" (Just "41041259135156") shiny ""
                      , ralLook "FD" "6009" (Just "41041259167924") shiny ""
                      , ralLook "FE" "6018" (Just "41041260052660") shiny ""
                      ]
                  }
                , { id= "G"
                  , description= Dict.fromList [ ("de", "Grau"), ("en", "Gray") ]
                  , looks=
                      [ ralLook "GA" "7011" (Just "41041259331764") shiny ""
                      , ralLook "GB" "7035" (Just "41041259659444") shiny ""
                      , ralLook "GC" "7042" (Just "41041259921588") shiny ""
                     ]
                  }
                , { id= "H"
                  , description= Dict.fromList [ ("de", "Braun"), ("en", "Brown") ]
                  , looks=
                      [ ralLook "HA" "8000" (Just "41041259626676") shiny ""
                      , ralLook "HB" "8003" (Just "41041259233460") shiny ""
                      , ralLook "HC" "8011" (Just "41041259266228") shiny ""
                      , ralLook "HD" "8017" (Just "41041259298996") shiny ""
                     ]
                  }
                , { id= "I"
                  , description= Dict.fromList [ ("en", "Black"), ("de", "Schwarz") ]
                  , looks=
                        [ patchDescription 
                            (ralLook "IA" "9005" (Just "41041260150964") [ UsdInput "roughness" "0.9" Nothing ] "_matt")
                            <| Dict.fromList
                              [ ( "de", "Tiefschwarz\nmatt" )
                              , ( "en", "Jet black\nmat" )
                              , ( "fr", "Noir foncé\nmat" )
                              , ( "it", "Nero intenso\nopaco" )
                              , ( "es", "Negro intenso\nmate" )
                              ]
                        , patchDescription 
                            (ralLook "IB" "9005" (Just "41041260085428") [ UsdInput "roughness" "0.8" Nothing ] "_seidenmatt")
                            <| Dict.fromList
                              [ ( "de", "Tiefschwarz\nseidenmatt" )
                              , ( "en", "Jet black\nsatin" )
                              , ( "fr", "Noir foncé\nsatiné" )
                              , ( "it", "Nero intenso\nsemilucido" )
                              , ( "es", "Negro intenso\nsemibrillante" )
                              ]
                        , patchDescription 
                            (ralLook "IC" "9005" (Just "41041259757748") shiny "_glanz")
                            <| Dict.fromList
                              [ ( "de", "Tiefschwarz\nglänzend" )
                              , ( "en", "Jet black\nglossy" )
                              , ( "fr", "Noir foncé\nbrillant" )
                              , ( "it", "Nero intenso\nlucido" )
                              , ( "es", "Negro intenso\nbrillo" )
                              ]
                        ]
                  }
                , { id= "J"
                  , description= Dict.fromList [ ("en", "White"), ("de", "Weiß") ]
                  , looks=
                        [ patchDescription 
                            (ralLook "JA" "9010" (Just "41041260216500") [ UsdInput "roughness" "0.9" Nothing ] "_matt")
                            <| Dict.fromList
                              [ ( "de", "Reinweiß matt" )
                              , ( "en", "Pure white mat" )
                              , ( "fr", "Blanc pur mat" )
                              , ( "it", "Bianco puro opaco" )
                              , ( "es", "Blanco puro mate" )
                              ]
                        , patchDescription 
                            (ralLook "JB" "9010" (Just "41041260282036") shiny "_glanz")
                            <| Dict.fromList
                              [ ( "de", "Reinweiß glänzend" )
                              , ( "en", "Pure white glossy" )
                              , ( "fr", "Blanc pur brilliant" )
                              , ( "it", "Bianco puro lucido" )
                              , ( "es", "Blanco puro brillo" )
                              ]
                        ]
                  }
                , { id= "K"
                  , description= Dict.fromList [ ("en", "Metallic Spray"), ("de", "Metallspray") ]
                  , looks=
                        [ ralLook "KA" "R307" (Just "41041260183732") metallic "_glanz"
                        , ralLook "KB" "R308" (Just "41041259397300") metallic "_glanz"
                        , ralLook "KC" "R309" (Just "41041259856052") metallic "_glanz"
                        , ralLook "KD" "R310" (Just "41041259790516") metallic "_glanz"
                        ]
                  }
                ]
            {-
            , startGrpLook= ("BlackWhite", "9010")
            , lookGroups=
                [ { id= "Yellow1"
                  , description= Dict.fromList [ ("en", "Yellow/beige 1"), ("de", "Gelb/Beige 1") ]
                  , looks=
                        [ ralLook "1000" (Just "1000") occ
                        , ralLook "1001" (Just "1001") occ
                        , ralLook "1002" (Just "1002") occ
                        , ralLook "1003" (Just "1003") occ
                        , ralLook "1004" (Just "1004") occ
                        , ralLook "1005" (Just "1005") occ
                        , ralLook "1006" (Just "1006") occ
                        , ralLook "1007" (Just "1007") occ
                        , ralLook "1011" (Just "1011") occ
                        , ralLook "1012" (Just "1012") occ
                        , ralLook "1013" (Just "1013") occ
                        , ralLook "1014" (Just "1014") occ
                        , ralLook "1015" (Just "1015") occ
                        , ralLook "1016" (Just "1016") occ
                        , ralLook "1017" (Just "1017") occ
                        , ralLook "1018" (Just "1018") occ
                        ]
                  }
                , { id= "Yellow2"
                  , description= Dict.fromList [ ("en", "Yellow/beige 2"), ("de", "Gelb/Beige 2") ]
                  , looks=
                        [ ralLook "1019" (Just "1019") occ
                        , ralLook "1020" (Just "1020") occ
                        , ralLook "1021" (Just "1021") occ
                        , ralLook "1023" (Just "1023") occ
                        , ralLook "1024" (Just "1024") occ
                        , ralLook "1026" (Just "1026") occ
                        , ralLook "1027" (Just "1027") occ
                        , ralLook "1028" (Just "1028") occ
                        , ralLook "1032" (Just "1032") occ
                        , ralLook "1033" (Just "1033") occ
                        , ralLook "1034" (Just "1034") occ
                        , ralLook "1035" (Just "1035") occ
                        , ralLook "1036" (Just "1036") occ
                        , ralLook "1037" (Just "1037") occ
                        ]
                  }
                , { id= "Orange"
                  , description= Dict.fromList [ ("en", "Orange"), ("de", "Orange") ]
                  , looks=
                        [ ralLook "2000" (Just "2000") occ
                        , ralLook "2001" (Just "2001") occ
                        , ralLook "2002" (Just "2002") occ
                        , ralLook "2003" (Just "2003") occ
                        , ralLook "2004" (Just "2004") occ
                        , ralLook "2005" (Just "2005") occ
                        , ralLook "2007" (Just "2007") occ
                        , ralLook "2008" (Just "2008") occ
                        , ralLook "2009" (Just "2009") occ
                        , ralLook "2010" (Just "2010") occ
                        , ralLook "2011" (Just "2011") occ
                        , ralLook "2012" (Just "2012") occ
                        , ralLook "2013" (Just "2013") occ
                        , ralLook "2017" (Just "2017") occ
                        ]
                  }
                , { id= "Red1"
                  , description= Dict.fromList [ ("en", "Red 1"), ("de", "Rot 1") ]
                  , looks=
                        [ ralLook "3000" (Just "3000") occ
                        , ralLook "3001" (Just "3001") occ
                        , ralLook "3002" (Just "3002") occ
                        , ralLook "3003" (Just "3003") occ
                        , ralLook "3004" (Just "3004") occ
                        , ralLook "3005" (Just "3005") occ
                        , ralLook "3007" (Just "3007") occ
                        , ralLook "3009" (Just "3009") occ
                        , ralLook "3011" (Just "3011") occ
                        , ralLook "3012" (Just "3012") occ
                        , ralLook "3013" (Just "3013") occ
                        , ralLook "3014" (Just "3014") occ
                        , ralLook "3015" (Just "3015") occ
                        ]
                  }
                , { id= "Red2"
                  , description= Dict.fromList [ ("en", "Red 2"), ("de", "Rot 2") ]
                  , looks=
                        [ ralLook "3016" (Just "3016") occ
                        , ralLook "3017" (Just "3017") occ
                        , ralLook "3018" (Just "3018") occ
                        , ralLook "3020" (Just "3020") occ
                        , ralLook "3022" (Just "3022") occ
                        , ralLook "3024" (Just "3024") occ
                        , ralLook "3026" (Just "3026") occ
                        , ralLook "3027" (Just "3027") occ
                        , ralLook "3028" (Just "3028") occ
                        , ralLook "3031" (Just "3031") occ
                        , ralLook "3032" (Just "3032") occ
                        , ralLook "3033" (Just "3033") occ
                        ]
                  }
                , { id= "Lilac"
                  , description= Dict.fromList [ ("en", "Lilac"), ("de", "Violett") ]
                  , looks=
                        [ ralLook "4001" (Just "4001") occ
                        , ralLook "4002" (Just "4002") occ
                        , ralLook "4003" (Just "4003") occ
                        , ralLook "4004" (Just "4004") occ
                        , ralLook "4005" (Just "4005") occ
                        , ralLook "4006" (Just "4006") occ
                        , ralLook "4007" (Just "4007") occ
                        , ralLook "4008" (Just "4008") occ
                        , ralLook "4009" (Just "4009") occ
                        , ralLook "4010" (Just "4010") occ
                        , ralLook "4011" (Just "4011") occ
                        , ralLook "4012" (Just "4012") occ
                        ]
                  }
                , { id= "Blue"
                  , description= Dict.fromList [ ("en", "Blue"), ("de", "Blau") ]
                  , looks=
                        [ ralLook "5000" (Just "5000") occ
                        , ralLook "5001" (Just "5001") occ
                        , ralLook "5002" (Just "5002") occ
                        , ralLook "5003" (Just "5003") occ
                        , ralLook "5004" (Just "5004") occ
                        , ralLook "5005" (Just "5005") occ
                        , ralLook "5007" (Just "5007") occ
                        , ralLook "5008" (Just "5008") occ
                        , ralLook "5009" (Just "5009") occ
                        , ralLook "5010" (Just "5010") occ
                        , ralLook "5011" (Just "5011") occ
                        , ralLook "5012" (Just "5012") occ
                        , ralLook "5013" (Just "5013") occ
                        , ralLook "5014" (Just "5014") occ
                        , ralLook "5015" (Just "5015") occ
                        , ralLook "5017" (Just "5017") occ
                        , ralLook "5018" (Just "5018") occ
                        , ralLook "5019" (Just "5019") occ
                        , ralLook "5020" (Just "5020") occ
                        , ralLook "5021" (Just "5021") occ
                        , ralLook "5022" (Just "5022") occ
                        , ralLook "5023" (Just "5023") occ
                        , ralLook "5024" (Just "5024") occ
                        , ralLook "5025" (Just "5025") occ
                        , ralLook "5026" (Just "5026") occ
                        ]
                  }
                , { id= "Green1"
                  , description= Dict.fromList [ ("en", "Green 1"), ("de", "Grün 1") ]
                  , looks=
                        [ ralLook "6000" (Just "6000") occ
                        , ralLook "6001" (Just "6001") occ
                        , ralLook "6002" (Just "6002") occ
                        , ralLook "6003" (Just "6003") occ
                        , ralLook "6004" (Just "6004") occ
                        , ralLook "6005" (Just "6005") occ
                        , ralLook "6006" (Just "6006") occ
                        , ralLook "6007" (Just "6007") occ
                        , ralLook "6008" (Just "6008") occ
                        , ralLook "6009" (Just "6009") occ
                        , ralLook "6010" (Just "6010") occ
                        , ralLook "6011" (Just "6011") occ
                        , ralLook "6012" (Just "6012") occ
                        , ralLook "6013" (Just "6013") occ
                        , ralLook "6014" (Just "6014") occ
                        , ralLook "6015" (Just "6015") occ
                        , ralLook "6016" (Just "6016") occ
                        , ralLook "6017" (Just "6017") occ
                        , ralLook "6018" (Just "6018") occ
                        , ralLook "6019" (Just "6019") occ
                        ]
                  }
                , { id= "Green2"
                  , description= Dict.fromList [ ("en", "Green 2"), ("de", "Grün 2") ]
                  , looks=
                        [ ralLook "6020" (Just "6020") occ
                        , ralLook "6021" (Just "6021") occ
                        , ralLook "6022" (Just "6022") occ
                        , ralLook "6024" (Just "6024") occ
                        , ralLook "6025" (Just "6025") occ
                        , ralLook "6026" (Just "6026") occ
                        , ralLook "6027" (Just "6027") occ
                        , ralLook "6028" (Just "6028") occ
                        , ralLook "6029" (Just "6029") occ
                        , ralLook "6032" (Just "6032") occ
                        , ralLook "6033" (Just "6033") occ
                        , ralLook "6034" (Just "6034") occ
                        , ralLook "6035" (Just "6035") occ
                        , ralLook "6036" (Just "6036") occ
                        , ralLook "6037" (Just "6037") occ
                        , ralLook "6038" (Just "6038") occ
                        , ralLook "6039" (Just "6039") occ
                        ]
                  }
                , { id= "Grey1"
                  , description= Dict.fromList [ ("en", "Grey 1"), ("de", "Grau 1") ]
                  , looks=
                        [ ralLook "7000" (Just "7000") occ
                        , ralLook "7001" (Just "7001") occ
                        , ralLook "7002" (Just "7002") occ
                        , ralLook "7003" (Just "7003") occ
                        , ralLook "7004" (Just "7004") occ
                        , ralLook "7005" (Just "7005") occ
                        , ralLook "7006" (Just "7006") occ
                        , ralLook "7008" (Just "7008") occ
                        , ralLook "7009" (Just "7009") occ
                        , ralLook "7010" (Just "7010") occ
                        , ralLook "7011" (Just "7011") occ
                        , ralLook "7012" (Just "7012") occ
                        , ralLook "7013" (Just "7013") occ
                        , ralLook "7015" (Just "7015") occ
                        , ralLook "7016" (Just "7016") occ
                        , ralLook "7021" (Just "7021") occ
                        , ralLook "7022" (Just "7022") occ
                        , ralLook "7023" (Just "7023") occ
                        , ralLook "7024" (Just "7024") occ
                        , ralLook "7026" (Just "7026") occ
                        ]
                  }
                , { id= "Grey2"
                  , description= Dict.fromList [ ("en", "Grey 2"), ("de", "Grau 2") ]
                  , looks=
                        [ ralLook "7030" (Just "7030") occ
                        , ralLook "7031" (Just "7031") occ
                        , ralLook "7032" (Just "7032") occ
                        , ralLook "7033" (Just "7033") occ
                        , ralLook "7034" (Just "7034") occ
                        , ralLook "7035" (Just "7035") occ
                        , ralLook "7036" (Just "7036") occ
                        , ralLook "7037" (Just "7037") occ
                        , ralLook "7038" (Just "7038") occ
                        , ralLook "7039" (Just "7039") occ
                        , ralLook "7040" (Just "7040") occ
                        , ralLook "7042" (Just "7042") occ
                        , ralLook "7043" (Just "7043") occ
                        , ralLook "7044" (Just "7044") occ
                        , ralLook "7045" (Just "7045") occ
                        , ralLook "7046" (Just "7046") occ
                        , ralLook "7047" (Just "7047") occ
                        , ralLook "7048" (Just "7048") occ
                        ]
                  }
                , { id= "Brown"
                  , description= Dict.fromList [ ("en", "Brown"), ("de", "Braun") ]
                  , looks=
                        [ ralLook "8000" (Just "8000") occ
                        , ralLook "8001" (Just "8001") occ
                        , ralLook "8002" (Just "8002") occ
                        , ralLook "8003" (Just "8003") occ
                        , ralLook "8004" (Just "8004") occ
                        , ralLook "8007" (Just "8007") occ
                        , ralLook "8008" (Just "8008") occ
                        , ralLook "8011" (Just "8011") occ
                        , ralLook "8012" (Just "8012") occ
                        , ralLook "8014" (Just "8014") occ
                        , ralLook "8015" (Just "8015") occ
                        , ralLook "8016" (Just "8016") occ
                        , ralLook "8017" (Just "8017") occ
                        , ralLook "8019" (Just "8019") occ
                        , ralLook "8022" (Just "8022") occ
                        , ralLook "8023" (Just "8023") occ
                        , ralLook "8024" (Just "8024") occ
                        , ralLook "8025" (Just "8025") occ
                        , ralLook "8028" (Just "8028") occ
                        , ralLook "8029" (Just "8029") occ
                        ]
                  }
                , { id= "BlackWhite"
                  , description= Dict.fromList [ ("en", "Black/white"), ("de", "Schwarz/Weiß") ]
                  , looks=
                        [ ralLook "9001" (Just "9001") occ
                        , ralLook "9002" (Just "9002") occ
                        , ralLook "9003" (Just "9003") occ
                        , ralLook "9004" (Just "9004") occ
                        , ralLook "9005" (Just "9005") occ
                        , ralLook "9006" (Just "9006") occ
                        , ralLook "9007" (Just "9007") occ
                        , ralLook "9010" (Just "9010") occ
                        , ralLook "9011" (Just "9011") occ
                        , ralLook "9012" (Just "9012") occ
                        , ralLook "9016" (Just "9016") occ
                        , ralLook "9017" (Just "9017") occ
                        , ralLook "9018" (Just "9018") occ
                        , ralLook "9020" (Just "9020") occ
                        , ralLook "9022" (Just "9022") occ
                        , ralLook "9023" (Just "9023") occ
                        ]
                  }
                , { id= "MetallicSpray"
                  , description= Dict.fromList [ ("en", "Metallic Spray"), ("de", "Metallspray") ]
                  , looks=
                        [ ralLook "R307" (Just "R307") occ
                        , ralLook "R308" (Just "R308") occ
                        , ralLook "R309" (Just "R309") occ
                        , ralLook "R310" (Just "R310") occ
                        ]
                  }
                , { id= "Bundeswehr"
                  , description= Dict.fromList [ ("en", "Military"), ("de", "Bundeswehr") ]
                  , looks=
                        [ ralLook "6031" (Just "6031") occ
                        , ralLook "8027" (Just "8027") occ
                        , ralLook "9021" (Just "9021") occ
                        , ralLook "1039" (Just "1039") occ
                        , ralLook "1040" (Just "1040") occ
                        , ralLook "6040" (Just "6040") occ
                        , ralLook "7050" (Just "7050") occ
                        , ralLook "8031" (Just "8031") occ
                        ]
                  }
                ]
            -}
            }
        ]
-}
    }


{-
Potentielle Kunden:

https://white-boards.de/trennwand-pinnwand-doppelseitig-18-ral-farben-zur-auswahl-180x120-cm

https://www.designyourbike.com/de/wp-content/uploads/sites/2/2019/12/RAL-PALETTE-DYB.pdf
https://www.kirchberger-metall.de/muelltonnenbox-verzinkt-pulverbeschichtet-modell-line-2er-box-240-liter.html
https://www.resorti-muelltonnenboxen.de/muelltonnenboxen/muelltonnenboxen-aus-metall/meisterbox-1er-muelltonnenbox
https://www.nicewaste.de/muelltonnenbox-edelstahl-ral-farben/
http://www.briefkasten-heidner.de/FAH515-Renz-Stand-Paket-Briefkastenanlage-2er-Stele-QUBO-L-Entnahme-hinten-RAL-Auswahl
http://www.mh.de/ral_farben.php
https://belutec-konfigurator.de/selectgt.php
https://www.brustor.com/de-de/farben
https://www.folgner-rolladen.de/medien/medienpool/folgner-insektenschutzkatalog-einzelseiten-ral.pdf
https://rollladen-schroeder.de/k294/Markisen-von-Lewens-Gestellfarben-RAL
https://www.eggersmann.com/de/
https://www.ballerina.de
https://www.one-bath.de/Freistehende-Badewanne-Tula-170x80x50cm-Mineralguss-RAL-Farben
https://www.panelsell.com/ral-farben-von-sandwichplatten

https://www.harzspezialisten.de/polyesterharz_topcoat_premium_qualitaet_RAL
https://www.euro-lock.de/de/liste-der-ral-farbvarianten-seite-1-von-3.html
https://www.mawi-pulverbeschichtung.de/verfahren.html
https://pulverlacke24.de
https://www.farben-frost.de
https://www.industriefarbe.de/Eisenglimmer-Lack-Schmiedelack-Farbton-DB-701-matt-1-Liter-Gebinde
https://cosmoslac.de/products/ral

https://www.schulungshandbuch.de/epages/62784718.sf/de_LU/?ObjectPath=/Shops/62784718/Products/M-0001


https://www.pfleiderer.com/dach-de/produkte/produktfinder-strukturen#start#14   26 Pberflächenstrukturen
https://www.pfleiderer.com/dach-de/produkte/dekore#start                        151 Uni-Dekore (aus insgesamt 397)
https://www.pfleiderer.com/dach-de/mustershop/musterfaecher-mustersets


-- TODO https://www.heroal.de/en/topics/design/ral-and-db-colours/index.php
{-

heroal - Johann Henkenjohann GmbH & Co. KG
Österwieher Str. 80
33415 Verl
-
Umsetzung Konfigurator:
redPlant - realtime studios
Elisabethstr. 101
40217 Düsseldorf
http://www.redplant.de

https://konfigurator.system4.ch (Beispiel mit glTF download)

-}
-}