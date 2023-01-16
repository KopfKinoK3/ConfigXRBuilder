module Intro.Swivel exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Events
import Cmd.Extra exposing (withCmd, withCmds, withNoCmd)
import Delay exposing (sequence)
import Element
    exposing
        ( Element
        , alignLeft, alignRight, centerX
        , alignTop, alignBottom, centerY
        , column
        , el
        , fill, fillPortion
        , height
        , paddingXY, padding
        , px, minimum, maximum
        , rgb255
        , row
        , spacing, spacingXY, scrollbarY
        , text
        , width, wrappedRow
        , rotate, inFront
        , moveDown, moveLeft, moveRight, moveUp
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Attribute, Html)
import Html.Attributes
import Svg.Attributes as SvgA
import QRCode
import Url exposing (percentEncode)
import Dict exposing (Dict)
import Intro.SwivelProject exposing (..)
import Intro.K3Company exposing (..)

type alias LanguageString=  Dict String String

type alias Code =
    { id : String
    , shopId : String
    , group : LanguageString
    , descr : LanguageString
    }

type alias CodeItem =
    { item : String
    , descr : LanguageString
    , codeWidth : Int
    , codes : List Code
    }

type alias Model =
    { width : Float
    , height : Float
    , userLang : String
    , urlParam : String
    , codeInput : String
    , supportsARQL : Bool
    , showQRcode : Bool
    , firstVisit : Bool
    }

type alias Flags =
    { width : Float
    , height : Float
    , userLang : String
    , urlParam : String
    , lastUrl : String
    , thisUrl : String
    , supportsARQL : Bool
    }

type Msg
    = NoOp
    | WindowSize Int Int
    | ShowQRcode Bool
    | ChangeCode String
    | SetLang String


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { width= flags.width
      , height= flags.height
      , userLang= flags.userLang
      , urlParam= flags.urlParam
      , supportsARQL= flags.supportsARQL
      , showQRcode= False
      , codeInput= String.replace "?" "" flags.urlParam
      , firstVisit= flags.lastUrl == ""
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onResize WindowSize ]


main : Program Flags Model Msg
main =
    Browser.element
        { init= init
        , view= view
        , update= update
        , subscriptions= subscriptions
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp -> ( model, Cmd.none )
        
        ShowQRcode show ->
            ( { model | showQRcode= show }
            , Cmd.none
            )

        WindowSize width height ->
            ( { model
                | width = toFloat width
                , height = toFloat height
              }
            , Cmd.none
            )

        ChangeCode str ->
            ( { model | codeInput= str }
            , Cmd.none
            )

        SetLang newLang ->
            ( { model | userLang= newLang }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    let
        langGet : LanguageString  -> String
        langGet ls  =
            case Dict.get model.userLang ls of
                Nothing ->
                    case String.split "-" model.userLang of
                        master :: _ ->
                            case Dict.get master ls of
                                Nothing -> 
                                    (Dict.get fallBackLang ls 
                                        |> Maybe.withDefault "")
                                Just str -> str
                        _ ->
                            (Dict.get fallBackLang ls 
                                |> Maybe.withDefault "")
                Just str -> str
        footer =
            wrappedRow [ width fill, spacingXY 20 10, padding 10 ]
                [ Element.newTabLink [ width fill ]
                    { url= langGet company.impressUrl
                    , label= el [Font.underline, Font.color mainColor, Element.centerX ] <| text <| langGet company.impressText
                    }
                , Element.newTabLink [ width fill ]
                    { url= langGet company.dsgvoUrl
                    , label= el [Font.underline, Font.color mainColor, Element.centerX ] <| text <| langGet company.dsgvoText
                    }
                , Element.newTabLink [ width fill ]
                    { url= langGet company.concatUrl
                    , label= el [Font.underline, Font.color mainColor, Element.centerX ] <| text <| langGet company.concatText
                    }
                , el [ width fill ] <| el [ centerX ] <| text <| langGet company.copyright
                ]
        blogLink=
            Element.newTabLink [ centerX ]
                { url= langGet translations.blogUrl
                , label= el [Font.underline, Font.color mainColor, Element.centerX ] <|
                    text <| langGet translations.blogText
                }

        header=
            Element.image
                [ centerX, width (px 80) ]
                { src= company.logoSrc
                , description= langGet company.logoText
                }
        title=
            column [ centerX, spacing 5, paddingXY 0 20 ]
                [ el [ centerX ] <| text <| langGet translations.headline1
                , el [ centerX, Font.bold, Font.size 20, Font.color mainColor ] <| text <| sceneId
                , el [ centerX, Font.bold, Font.size 12, Font.color mainColor ] <| text <| langGet descr
                , if model.firstVisit
                    then el [ paddingXY 0 10, centerX ] <| Element.paragraph [ Font.size 13 ] <|
                        List.map (\t -> text (t++" ")) (String.split " " <| langGet translations.invite)
                    else el [ centerX ] <| text <| langGet translations.headline2
                ]
        itemHeader  = langGet translations.itemHeader
        groupHeader = langGet translations.groupHeader
        lookHeader  = langGet translations.lookHeader
        gather codes =
            List.foldl
                (\item f ->
                    let
                        currCode= String.left item.codeWidth f.codes
                        matching=
                            List.filter
                                (\code ->
                                    code.id == currCode
                                )
                                item.codes
                        itemDescr= langGet item.descr
                        newUI=
                            row [ width fill, spacing 10 ] 
                                [ el [ width (fillPortion 2) ] <| text <| itemDescr
                                , case matching of
                                    [] -> el [ width (fillPortion 4) ] <| text <| (langGet translations.noMatch)++currCode
                                    code :: _ ->
                                        row [ width (fillPortion 4), spacing 10 ]
                                            [ el [ width fill ] <| text <| langGet code.group
                                            , el [ width fill ] <| text <| langGet code.descr
                                            ]
                                ]
                        newText= String.concat
                            [ itemDescr
                            , ": "
                            , case matching of
                                [] -> (langGet translations.noMatch)++currCode
                                code :: _ -> String.join ", " <| List.filter (\s -> s /= "")
                                    [ langGet code.group
                                    , langGet code.descr
                                    ]
                            , "\n"
                            ]
                        newShopId=
                            case matching of
                                [] -> Nothing
                                code :: _ -> 
                                    case code.shopId of
                                        "" -> Nothing
                                        id -> Just id 

                    in
                    { codes= String.dropLeft item.codeWidth f.codes
                    , ui= f.ui ++ [ newUI ]
                    , text= f.text ++ [ newText ]
                    , shopIds= newShopId :: f.shopIds
                    }
                )
                { codes= codes
                , ui=
                    [ row [ width fill, spacing 10, Font.bold, Font.underline ]
                        [ el [ width (fillPortion 2) ] <| text <| itemHeader
                        , row [ width (fillPortion 4), spacing 10 ]
                            [ el [ width fill ] <| text groupHeader
                            , el [ width fill ] <| text lookHeader
                            ]
                        ]
                    ]
                , text= []
                , shopIds= [Just "1981_2"] -- TODO "ohne Kopfstütze"
                }
                items
        buildUrl lines = 
            "mailto:"++ (langGet company.mailto)
            ++ "?subject=" ++  (percentEncode <| langGet translations.emailSubject)
            ++ "&body=" ++ (
                    percentEncode <| 
                        ( langGet translations.emailBody ++ "\n\n" ++ urlPrefix ++ model.urlParam ++ "\n\n" ++
                            String.concat lines
                        )
                )
        stampLook=
            [ padding 5, Border.color mainColor, Border.width 2, Border.rounded 6, Font.color mainColor ]
        maxWidth= 512
        langUi=
            row
                [ spacing 10, centerX, alignTop, padding 10 ] <|
                List.map
                    (\(key,name) ->
                        Input.button [ Font.underline, Font.color mainColor ]
                            { onPress= Just (SetLang key)
                            , label= text name
                            }
                    ) <|
                    Dict.toList translations.languages
        explainUi=
            Element.paragraph [ width (fill |> maximum maxWidth), centerX, paddingXY 0 5 ] <|
                List.map (\t -> text (t++" ")) (String.split " " <| langGet translations.explain)
    in
    Element.layoutWith { options = [ Element.focusStyle myFocusStyle ] }
        [ Background.color <| gray 255
        , Font.family
            [ Font.typeface "atkinson hyperlegible"
            , Font.sansSerif
            ]        
        , Font.color <| gray 0
        , Font.size 13
        ] <|
        column
            [ centerX
            , width (fill |> maximum maxWidth)
            , spacing 10, padding 30
            , scrollbarY
            , inFront <| el ([alignLeft, rotate (degrees -30), moveDown 30, moveRight 5 ] ++ stampLook) <| text "!! TECH DEMO !!"
            , inFront <| column ([alignRight, rotate (degrees  30), moveDown 27, moveLeft 10 ] ++ stampLook) [ text "First on Apple", text "AR Quick Look" ]
            , inFront <|
                case Dict.get model.userLang translations.languages of
                    Nothing ->
                        case String.split "-" model.userLang of
                            master :: _ ->
                                case Dict.get master translations.languages of
                                    Nothing -> langUi
                                    _ -> Element.none
                            _ -> langUi
                    _ -> Element.none
            ] <|
            case String.uncons model.urlParam of
                Just ('?', codes) ->
                    let
                        gatherItems = gather codes
                        emailUrl= buildUrl gatherItems.text
                        shopUrl= 
                            langGet shopUrlPrefix 
                            ++ ( String.join "*" <| List.filterMap (\id -> id) gatherItems.shopIds )
                    in
                    header :: title :: gatherItems.ui ++ 
                    [ row [ padding 30, centerX, spacing 20 ]
                        [ Element.newTabLink
                            btnLook
                            { url= emailUrl
                            , label= text <| langGet translations.emailButton
                            }
                        , Element.newTabLink
                            btnLook
                            { url= shopUrl
                            , label= text <| langGet translations.webshopButton
                            }
                        ]
                    , explainUi
                    , blogLink
                    , footer
                    ]
                _ ->
                    let
                        neededLength = List.sum <| List.map .codeWidth items
                    in
                    if String.length model.codeInput == neededLength
                        then
                            let
                                gatherItems = gather model.codeInput
                                emailUrl= buildUrl gatherItems.text
                                shopUrl= 
                                    langGet shopUrlPrefix 
                                    ++ ( String.join "*" <| List.filterMap (\id -> id) gatherItems.shopIds )
                            in
                            header :: title :: gatherItems.ui ++ 
                            [ row [ padding 30, centerX, spacing 20 ]
                                [ Element.newTabLink
                                    btnLook
                                    { url= emailUrl
                                    , label= text <| langGet translations.emailButton
                                    }
                                , Element.newTabLink
                                    btnLook
                                    { url= emailUrl
                                    , label= text <| langGet translations.webshopButton
                                    }
                                ]
                            , Input.button [ centerX ]
                                { onPress= Just (ChangeCode "")
                                , label= el [Font.underline, Font.color mainColor ] <| text <| langGet translations.backToProduct
                                }
                            , explainUi
                            , blogLink
                            , footer
                            ]
                        else
                            let
                                arLang= -- TODO Add languages with a matching .reality here
                                    case String.slice 0 2 model.userLang of
                                        "de" -> "de"
                                        "pl" -> "pl"
                                        _ -> "en"   -- TODO change fallback
                                usdUrl= String.replace "LANGUAGE" arLang urlAR
                                imgUrl= urlPreview 
                            in
                            [ header
                            , title
                            , if model.firstVisit
                                then Element.none
                                else
                                    column [centerX, width (fill |> maximum maxWidth), spacing 10 ]
                                        [ Input.text [ centerX, Font.size 15 ]
                                            { onChange= ChangeCode
                                            , text= model.codeInput
                                            , placeholder= Nothing
                                            , label= Input.labelLeft [] <| text (urlPrefix++"?")
                                            }
                                        , Element.paragraph [ Font.size 13 ] <|
                                            List.map (\t -> text (t++" ")) (String.split " " <| langGet translations.enterCode)
                                        ]
                            , usdz
                                [ centerX
                                , width (fill |> maximum maxWidth)
                                ] 
                                { usdzUrl= usdUrl
                                , imgUrl= imgUrl
                                , supportsARQL= model.supportsARQL
                                , showQRcode= model.showQRcode
                                , scanText= langGet translations.scan
                                }
                            , explainUi
                            , blogLink
                            , footer
                            ]


{- 
Define your own elm-ui Element that works with Safari AR Quicklook feature on iOS 12+
The problem with the standard Element nodes is that Safari wants the <a> node to only have
a single child and that needs to be an <img>
-}

usdz : List (Element.Attribute Msg)
    -> { usdzUrl : String, imgUrl : String, supportsARQL : Bool, showQRcode : Bool, scanText : String }
    -> Element Msg
usdz attrs rec =
    if rec.supportsARQL
        then 
            Element.link
                ( Element.htmlAttribute (Html.Attributes.attribute "rel" "ar")
                :: attrs
                )
                { label= Element.html <|
                    Html.node "img" 
                        [ Html.Attributes.src   rec.imgUrl
                        , Html.Attributes.style "width"  "100%"
                        , Html.Attributes.style "height" "100%"
                        ] 
                        [] -- no other content allowed here by Apple
                , url=rec.usdzUrl
                }
        else 
            Element.image (
                ( Element.inFront <|
                    if rec.showQRcode
                        then
                            column
                                [ spacing 5, centerX, centerY, padding 20
                                , Background.color <| gray 255
                                , Element.inFront <|
                                    Input.button [ Element.alignRight, Element.alignTop, padding 5 ]
                                        { onPress= Just (ShowQRcode False)
                                        , label= text "✕"
                                        }
                                ]
                                [ Input.button []
                                    { onPress= Just (ShowQRcode False)
                                    , label= Element.html
                                        ( QRCode.fromString rec.usdzUrl
                                            |> Result.map
                                                (QRCode.toSvg
                                                    [ SvgA.width "200px"
                                                    , SvgA.height "200px"
                                                    ]
                                                )
                                            |> Result.withDefault (Html.text "Error while encoding to QR-Code.")
                                        )
                                    }
                                , el [ centerX, Font.size 10 ] <| text rec.scanText
                                ]
                        else
                            Input.button [ alignRight, alignTop, moveDown 10, moveLeft 10 ]
                                { onPress= Just (ShowQRcode True)
                                , label= 
                                    Element.image [ width (px 40) ] {src= "./ArKitGlyph.svg", description="ArKit Icon" } 
                                    --text "AR QR-Code"
                                }
                ) :: attrs )
                { src=rec.imgUrl
                , description=""
                }


translations=
    { itemHeader= Dict.fromList
        [ ("de", "Komponente")
        , ("en", "Component")
        ]
    , groupHeader= Dict.fromList
        [ ("de", "Materialgruppe")
        , ("en", "Material group")
        ]
    , lookHeader= Dict.fromList
        [ ("de", "Material")
        , ("en", "Material")
        ]
    , headline1= Dict.fromList
        [ ("de", "Sie interessieren sich für das Produkt")
        , ("en", "You're interested in the product")
        ]
    , headline2= Dict.fromList
        [ ("de", "in folgender Ausstattung:")
        , ("en", "with the following features:")
        ]
    , invite= Dict.fromList
        [ ("de", "Mit dieser AR Experience (keine Installation nötig!) können Sie auf ihrem iPhone oder iPad interaktiv Stoffe, Farben und Ausstattung zusammenstellen und kehren über einen Bestell-Link mit Ihrer Wunschkonfiguration wieder hierhin zurück.")
        , ("en", "Use the AR Experience below (no install needed!) on your iPhone or iPad to interactively select fabrics, colors and options. It will present you with a custom order link to return back here with the settings you choose.")
        ]
    , webshopButton= Dict.fromList
        [ ("de", "Zum Webshop")
        , ("en", "visit webshop")
        ]
    , emailButton= Dict.fromList
        [ ("de", "Email Anfrage")
        , ("en", "Email request")
        ]
    , emailSubject= Dict.fromList
        [ ("de", "Interesse an ihrem AR Produkt-Konfigurator")
        , ("en", "Interest in your AR product configurator")
        ]
    , emailBody= Dict.fromList
        [ ("de", "Bitte informieren Sie mich über dieses aussergewöhnliche Marketing-Werkzeug und wie ich es in unsere Prozesse einbinden kann!")
        , ("en", "Please send me more information about this outstanding marketing tool and how to put this to work for us!")
        ]
    , noMatch= Dict.fromList
        [ ("de", "Nichts passendes gefunden für den Code ")
        , ("en", "Nothing matches the code ")
        ]
    , scan= Dict.fromList
        [ ("de", "Für iPhone6S oder jünger, sowie iPad")
        , ("en", "Scan with an iPad or iPhone6S (or newer)")
        ]
    , enterCode= Dict.fromList
        [ ("de", "Falls Sie das Produkt bereits in AR Quick Look konfiguriert haben, geben Sie hier den dabei ermittelten Ausstattungs-Code ein. Falls nicht, starten Sie die folgende interaktive AR Experience auf Ihrem iOS-Gerät:")
        , ("en", "If you already configured this product in AR Quick Look, enter the resulting feature code above, or launch the following interactive AR experience on your iOS device:")
        --, ("pl", "Jeśli ten produkt został już skonfigurowany w AR Quick Look, wpisz wynikowy kod funkcji powyżej lub uruchom następujące interaktywne środowisko AR na urządzeniu z systemem iOS:")
        ]
    , backToProduct= Dict.fromList
        [ ("de", "zurück zum Produkt")
        , ("en", "back to the product")
        ]
    , blogUrl= Dict.fromList
        [ ("de", "https://kreativekommunikationskonzepte.de/verkaufen-mit-dem-augmented-reality-stuhl-konfigurator-ar-by-apple-mit--usdz---reality/")
        , ("en", "https://kreativekommunikationskonzepte-de.translate.goog/verkaufen-mit-dem-augmented-reality-stuhl-konfigurator-ar-by-apple-mit--usdz---reality/?_x_tr_sl=auto&_x_tr_tl=en&_x_tr_hl=de")
        , ("pl", "https://kreativekommunikationskonzepte-de.translate.goog/verkaufen-mit-dem-augmented-reality-stuhl-konfigurator-ar-by-apple-mit--usdz---reality/?_x_tr_sl=auto&_x_tr_tl=pl&_x_tr_hl=de")
        , ("fr", "https://kreativekommunikationskonzepte-de.translate.goog/verkaufen-mit-dem-augmented-reality-stuhl-konfigurator-ar-by-apple-mit--usdz---reality/?_x_tr_sl=auto&_x_tr_tl=fr&_x_tr_hl=de")
        , ("nl", "https://kreativekommunikationskonzepte-de.translate.goog/verkaufen-mit-dem-augmented-reality-stuhl-konfigurator-ar-by-apple-mit--usdz---reality/?_x_tr_sl=auto&_x_tr_tl=nl&_x_tr_hl=de")
        , ("it", "https://kreativekommunikationskonzepte-de.translate.goog/verkaufen-mit-dem-augmented-reality-stuhl-konfigurator-ar-by-apple-mit--usdz---reality/?_x_tr_sl=auto&_x_tr_tl=it&_x_tr_hl=de")
        ]
    , blogText= Dict.fromList
        [ ("de", "Unser Blogbeitrag zu dieser Technologie-Demonstration")
        , ("en", "Learn more about this tech demo in our blog post (auto-translated)")
        ]
    , explain= Dict.fromList
        [ ("de", "HINWEIS: Wir selbst vertreiben keine Produkte, demonstrieren hier nur wie wir Herstellern modernste Methoden für AR bereitstellen können. Jede Variante unseres Phantasie-Produktes Swivel wird über den Webshop-Button zu einer real verfügbaren Variante des Produktes \"Sava\" des polnischen Herstellers MDD verlinkt! Wir stehen in keiner Geschäftsbeziehung zu MDD, verwenden nur dankbar das 3D-Modell das dieser Hersteller in einer freien Lizenz öffentlich bereitgestellt hat.")
        , ("en", "DISCLAIMER: We do not sell products, but use this to demonstrate a most modern use case of interactive AR technology for companies. All variants of our fantasy product Swivel can be resolved as a real product variant of \"Sava\" by the polish company MDD by using the Webshop button. We are not affiliated but would like to thank MDD for making their chair 3D model publicly available.")
        ]
    , languages= Dict.fromList
        [ ("en", "English")
        , ("de", "Deutsch")
        --, ("pl", "Polskie")
        ]
    }


pxFloat : Float -> Element.Length
pxFloat p =
    px (round p)

gray g = rgb255 g g g

mainColor = rgb255 255 128 0

btnLook =
    [ padding 15
    , Border.rounded 10, Border.solid, Border.color <| gray 255
    , Background.color <| mainColor
    , Font.color <| gray 255
    , Font.size 16
    ]

myFocusStyle =
    { borderColor = Nothing
    , backgroundColor = Nothing
    , shadow = Nothing
    }

{-
elm make src/Intro/Swivel.elm --output=public-intro/Swivel.js


TODO:
new url parser https://package.elm-lang.org/packages/Janiczek/elm-url-codec/latest/
-}
