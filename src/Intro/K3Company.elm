module Intro.K3Company exposing (company)

import Dict exposing (Dict)

company =
    { accentColor= (255, 128, 0)
    , mailto= Dict.fromList
        [ ("de", "hallo@KreativeKK.de")
        ]
    , impressUrl= Dict.fromList
        [ ("de", "https://kreativekommunikationskonzepte.de/impressum/")
        ]
    , impressText= Dict.fromList
        [ ("de", "Impressum")
        ]
    , dsgvoUrl= Dict.fromList
        [ ("de", "https://kreativekommunikationskonzepte.de/datenschutzerklaerung/")
        ]
    , dsgvoText= Dict.fromList
        [ ("de", "Datenschutzerklärung")
        ]
    , concatUrl= Dict.fromList
        [ ("de", "https://kreativekommunikationskonzepte.de/kontakt/")
        ]
    , concatText= Dict.fromList
        [ ("de", "Kontakt")
        ]
    , logoSrc= "https://kreativekommunikationskonzepte.de/wp-content/uploads/2020/04/Logo_K3_100.png"
    , logoText= Dict.fromList
        [ ("de", "Imagefilm / 360 Grad Video / Video-Marketing / Video-Produktion")
        ]
    , copyright= Dict.fromList
        [ ("de", "© 2022 Kreative KommunikationsKonzepte GmbH")
        ]
    }

