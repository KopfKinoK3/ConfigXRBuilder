module Intro.SwivelProject exposing (items, fallBackLang, descr, sceneId, urlAR, urlPreview, urlPrefix, shopUrlPrefix)

import Dict exposing (Dict)

fallBackLang = "en"

sceneId = "Swivel"

urlPrefix =   "https://www.kreativekk.de/Swivel.html"
urlAR =       "https://www.kreativekommunikationskonzepte.de/wp-content/uploads/ar/Swivel_LANGUAGE.reality#allowsContentScaling=0&canonicalWebPageURL=https://kreativekk.de/Swivel.html"
urlPreview =  "https://www.kreativekommunikationskonzepte.de/wp-content/uploads/ar/Chair-Config-V1.png"

shopUrlPrefix = Dict.fromList
  [ ("de", "https://mdd.eu/de/sitzmobel/sava/?conf=")
  , ("en", "https://mdd.eu/gb/seating/sava/?conf=")
  , ("pl", "https://mdd.pl/siedziska/sava/?conf=")
  ]

descr = Dict.fromList [ ("de", "Ergonomie und Schönheit vereint in einem Stuhl"), ("en", "Combine ergonomics and beauty in a chair"), ("pl", "Ergonomia i estetyka polaczona w jednym krzesle") ]

items =
  [ { item= "Rollen"
    , descr= Dict.fromList [ ("de", "Rollen"), ("en", "Castors"), ("pl", "Kółka") ]
    , codeWidth= 2
    , codes=
        [ { id= "CA", shopId= "10457_3", group= Dict.fromList [ ("en", "") ], descr= Dict.fromList [ ("de", "Weich (Parkett)"), ("en", "Soft (wooden floor")] }
        , { id= "CB", shopId= "10457_2", group= Dict.fromList [ ("en", "") ], descr= Dict.fromList [ ("de", "Hart (Teppich)"),  ("en", "Hard (carpet)")] }
        ]
    }
  , { item= "Untergestell"
    , descr= Dict.fromList [ ("de", "Untergestell"), ("en", "Base"), ("pl", "Podstawa") ]
    , codeWidth= 2
    , codes=
        [ { id= "BA", shopId= "530_2", group= Dict.fromList [ ("en", "") ], descr= Dict.fromList [ ("en", "Nylon black"), ("de", "Nylon Schwarz") ] }
        , { id= "BB", shopId= "530_3", group= Dict.fromList [ ("en", "") ], descr= Dict.fromList [ ("en", "Aluminium polished"), ("de", "Aluminium poliert") ] }
        ]
    }
  , { item= "Polster"
    , descr= Dict.fromList [ ("de", "Sitzbezug"), ("en", "Seat fabric"), ("pl", "Materiał siedziska") ]
    , codeWidth= 2
    , codes=
        [ { id= "NA", shopId= "11793_2",    group= Dict.fromList [ ("en", "Nemo") ],   descr= Dict.fromList [ ("de", "Ne-15 Navyblau"   ), ("en", "Ne-15 navy blue"   ) ] }
        , { id= "NB", shopId= "11793_3",    group= Dict.fromList [ ("en", "Nemo") ],   descr= Dict.fromList [ ("de", "Ne-10 Grau"       ), ("en", "Ne-10 grey"        ) ] }
        , { id= "NC", shopId= "11793_4",    group= Dict.fromList [ ("en", "Nemo") ],   descr= Dict.fromList [ ("de", "Ne-5 Rot"         ), ("en", "Ne-5 red"          ) ] }
        , { id= "ND", shopId= "11793_5",    group= Dict.fromList [ ("en", "Nemo") ],   descr= Dict.fromList [ ("de", "Ne-3 Orange"      ), ("en", "Ne-3 orange"       ) ] }
        , { id= "NE", shopId= "11793_6",    group= Dict.fromList [ ("en", "Nemo") ],   descr= Dict.fromList [ ("de", "Ne-16 Schwarz"    ), ("en", "Ne-16 black"       ) ] }
        , { id= "NF", shopId= "11793_7",    group= Dict.fromList [ ("en", "Nemo") ],   descr= Dict.fromList [ ("de", "Ne-2 Gelb"        ), ("en", "Ne-2 yellow"       ) ] }
        , { id= "NG", shopId= "11793_8",    group= Dict.fromList [ ("en", "Nemo") ],   descr= Dict.fromList [ ("de", "Ne-9 Grün"        ), ("en", "Ne-9 green"        ) ] }
        , { id= "NH", shopId= "11793_9",    group= Dict.fromList [ ("en", "Nemo") ],   descr= Dict.fromList [ ("de", "Ne-14 Graphit"    ), ("en", "Ne-14 graphite"    ) ] }
        , { id= "NI", shopId= "11793_10",   group= Dict.fromList [ ("en", "Nemo") ],   descr= Dict.fromList [ ("de", "Ne-1 Beige"       ), ("en", "Ne-1 beige"        ) ] }
        , { id= "NJ", shopId= "11793_11",   group= Dict.fromList [ ("en", "Nemo") ],   descr= Dict.fromList [ ("de", "Ne-17 Blau"       ), ("en", "Ne-17 blue"        ) ] }
        , { id= "RA", shopId= "4686*317_2", group= Dict.fromList [ ("en", "Runner") ], descr= Dict.fromList [ ("de", "68056 Grün"       ), ("en", "68056 green"       ) ] }
        , { id= "RB", shopId= "4686*317_3", group= Dict.fromList [ ("en", "Runner") ], descr= Dict.fromList [ ("de", "63034 Orange"     ), ("en", "63034 orange"      ) ] }
        , { id= "RC", shopId= "4686*317_4", group= Dict.fromList [ ("en", "Runner") ], descr= Dict.fromList [ ("de", "60011 Grau"       ), ("en", "60011 grey"        ) ] }
        , { id= "RD", shopId= "4686*317_5", group= Dict.fromList [ ("en", "Runner") ], descr= Dict.fromList [ ("de", "60999 Schwarz"    ), ("en", "60999 black"       ) ] }
        , { id= "RE", shopId= "4686*317_6", group= Dict.fromList [ ("en", "Runner") ], descr= Dict.fromList [ ("de", "66063 Blau"       ), ("en", "66063 blue"        ) ] }
        , { id= "RF", shopId= "4686*317_7", group= Dict.fromList [ ("en", "Runner") ], descr= Dict.fromList [ ("de", "61128 Beige"      ), ("en", "61128 beige"       ) ] }
        , { id= "RG", shopId= "4686*317_8", group= Dict.fromList [ ("en", "Runner") ], descr= Dict.fromList [ ("de", "60025 Anthrazit"  ), ("en", "60025 anthrazit"   ) ] }
        , { id= "RH", shopId= "4686*317_9", group= Dict.fromList [ ("en", "Runner") ], descr= Dict.fromList [ ("de", "64089 Rot"        ), ("en", "64089 red"         ) ] }
        , { id= "RI", shopId= "4686*317_10",group= Dict.fromList [ ("en", "Runner") ], descr= Dict.fromList [ ("de", "62064 Dunkelblau" ), ("en", "62064 dark blue"   ) ] }
        , { id= "RJ", shopId= "4686*317_11",group= Dict.fromList [ ("en", "Runner") ], descr= Dict.fromList [ ("de", "61130 Braun"      ), ("en", "61130 brown"       ) ] }
        , { id= "RK", shopId= "4686*317_3", group= Dict.fromList [ ("en", "Runner") ], descr= Dict.fromList [ ("de", "K3 Orange"        ), ("en", "K3 orange"         ) ] }
        , { id= "XA", shopId= "4687*318_2", group= Dict.fromList [ ("en", "Xtreme") ], descr= Dict.fromList [ ("de", "Xr108 Beige"      ), ("en", "Xr108 beige"       ) ] }
        , { id= "XB", shopId= "4687*318_3", group= Dict.fromList [ ("en", "Xtreme") ], descr= Dict.fromList [ ("de", "Xr094 Grau"       ), ("en", "Xr094 grey"        ) ] }
        , { id= "XC", shopId= "4687*318_4", group= Dict.fromList [ ("en", "Xtreme") ], descr= Dict.fromList [ ("de", "Xr081 Dunkelgrau" ), ("en", "Xr081 dark grey"   ) ] }
        , { id= "XD", shopId= "4687*318_5", group= Dict.fromList [ ("en", "Xtreme") ], descr= Dict.fromList [ ("de", "Xr420 Oliv"       ), ("en", "Xr420 olive"       ) ] }
        , { id= "XE", shopId= "4687*318_6", group= Dict.fromList [ ("en", "Xtreme") ], descr= Dict.fromList [ ("de", "Xr009 Schwarz"    ), ("en", "Xr009 black"       ) ] }
        , { id= "XF", shopId= "4687*318_7", group= Dict.fromList [ ("en", "Xtreme") ], descr= Dict.fromList [ ("de", "Xr026 Navyblau"   ), ("en", "Xr026 navy blue"   ) ] }
        , { id= "XG", shopId= "4687*318_8", group= Dict.fromList [ ("en", "Xtreme") ], descr= Dict.fromList [ ("de", "Ys071 Hellbraun"  ), ("en", "Ys071 light brown" ) ] }
        , { id= "XH", shopId= "4687*318_9", group= Dict.fromList [ ("en", "Xtreme") ], descr= Dict.fromList [ ("de", "Ys020 Burgund"    ), ("en", "Ys020 burgundy"    ) ] }
        , { id= "XI", shopId= "4687*318_10",group= Dict.fromList [ ("en", "Xtreme") ], descr= Dict.fromList [ ("de", "Ys076 Orange"     ), ("en", "Ys076 orange"      ) ] }
        , { id= "XJ", shopId= "4687*318_11",group= Dict.fromList [ ("en", "Xtreme") ], descr= Dict.fromList [ ("de", "Ys072 Gelb"       ), ("en", "Ys072 yellow"      ) ] }
        , { id= "XK", shopId= "4687*318_12",group= Dict.fromList [ ("en", "Xtreme") ], descr= Dict.fromList [ ("de", "Ys045 Grün"       ), ("en", "Ys045 green"       ) ] }
        , { id= "XL", shopId= "4687*318_13",group= Dict.fromList [ ("en", "Xtreme") ], descr= Dict.fromList [ ("de", "Ys100 Blau"       ), ("en", "Ys100 blue"        ) ] }
        ]
    }
  , { item= "Netz"
    , descr= Dict.fromList [ ("de", "Netzrücken"), ("en", "Net"), ("pl", "Oparcie") ]
    , codeWidth= 2
    , codes=
        [ { id= "BA", shopId= "529_4", group= Dict.fromList [ ("en", "Runner") ], descr= Dict.fromList [ ("en", "68056 green"), ("de", "68056 Grün") ] }
        , { id= "BB", shopId= "529_5", group= Dict.fromList [ ("en", "Runner") ], descr= Dict.fromList [ ("en", "63034 orange"), ("de", "63034 Orange") ] }
        , { id= "BC", shopId= "529_3", group= Dict.fromList [ ("en", "Runner") ], descr= Dict.fromList [ ("en", "60011 grey"), ("de", "60011 Grau") ] }
        , { id= "BD", shopId= "529_6", group= Dict.fromList [ ("en", "Runner") ], descr= Dict.fromList [ ("en", "60999 black"), ("de", "60999 Schwarz") ] }
        , { id= "BE", shopId= "529_7", group= Dict.fromList [ ("en", "Runner") ], descr= Dict.fromList [ ("en", "66063 blue"), ("de", "66063 Blau") ] }
        , { id= "BF", shopId= "529_8", group= Dict.fromList [ ("en", "Runner") ], descr= Dict.fromList [ ("en", "61128 beige"), ("de", "61128 Beige") ] }
        , { id= "BG", shopId= "529_2", group= Dict.fromList [ ("en", "Runner") ], descr= Dict.fromList [ ("en", "60025 anthrazit"), ("de", "60025 Anthrazit") ] }
        , { id= "BH", shopId= "529_9", group= Dict.fromList [ ("en", "Runner") ], descr= Dict.fromList [ ("en", "64089 red"), ("de", "64089 Rot") ] }
        , { id= "BI", shopId= "529_10",group= Dict.fromList [ ("en", "Runner") ], descr= Dict.fromList [ ("en", "62064 dark blue"), ("de", "62064 Dunkelblau") ] }
        , { id= "BJ", shopId= "529_11",group= Dict.fromList [ ("en", "Runner") ], descr= Dict.fromList [ ("en", "61130 brown"), ("de", "61130 Braun") ] }
        , { id= "BK", shopId= "529_5", group= Dict.fromList [ ("en", "Runner") ], descr= Dict.fromList [ ("en", "K3 orange"), ("de", "K3 Orange") ] }
        ]
    }
  , { item= "Armlehnen"
    , descr= Dict.fromList [ ("de", "Armlehnen"), ("en", "Armrests"), ("pl", "Podłokietników") ]
    , codeWidth= 1
    , codes=
        [ { id= "Y", shopId="527_3", group= Dict.fromList [], descr= Dict.fromList [ ("de", "mit"), ("en", "with"), ("pl", "Tak") ] }
        , { id= "N", shopId="527_2", group= Dict.fromList [], descr= Dict.fromList [ ("de", "ohne"), ("en", "w/out"), ("pl", "Nie") ] }
        ]
    }
  , { item= "Lenden"
    , descr= Dict.fromList [ ("de", "Lordosenstütze"), ("en", "Lumbar support"), ("pl", "Podparcie lędźwiowe") ]
    , codeWidth= 1
    , codes=
        [ { id= "Y", shopId="1982_3", group= Dict.fromList [], descr= Dict.fromList [ ("de", "mit"), ("en", "with"), ("pl", "Tak") ] }
        , { id= "N", shopId="1982_2", group= Dict.fromList [], descr= Dict.fromList [ ("de", "ohne"), ("en", "w/out"), ("pl", "Nie") ] }
        ]
    }
  ]
