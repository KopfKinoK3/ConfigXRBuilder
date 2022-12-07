module ConfigXR.Helper exposing ( simpleLooks, simpleLooksWithShop, ralLook, dbLook, pantoneLook, fixDescr, sanitize, toLinearStr, patchDescription, patchInputs, idFromIdx )

import ConfigXR.Types exposing (UsdInput, Look, LanguageString, newLook, defaultTx)
import Dict exposing (Dict, fromList)
import String
import Char exposing (isDigit, isAlphaNum, toCode)


fixDescr : String -> String
fixDescr str =
    str 
        --|> String.replace "ß" "ss"
        --|> String.replace "ö" "oe"
        --|> String.replace "ü" "ue"
        --|> String.replace "ä" "ae"
        --|> String.replace "Ü" "Ue"
        --|> String.replace "Ö" "Oe"
        --|> String.replace "Ä" "Ae"
        --|> String.replace "é" "e"
        --|> String.map
        --    (\c -> 
        --        if (toCode c) > 255
        --            then '?'
        --            else c
        --    )

sanitize : String -> String -> String
sanitize default str =
    let
        clean =
            String.replace " " "_" str
            |> String.filter 
                (\c -> 
                    isAlphaNum c ||  c == '_'
                )
    in
    if String.isEmpty clean
        then default
        else
            case String.toList clean of
                first :: _ -> if Char.isDigit first then "X"++clean else clean
                _ -> clean

{-
def srgb2lin(s):
    if s <= 0.0404482362771082:
        lin = s / 12.92
    else:
        lin = pow(((s + 0.055) / 1.055), 2.4)
    return lin
-}

-- new according to physicallybased.info
sRgbToLinear : Dict Int Float
sRgbToLinear=
    let
        step = 1.0 / 255.0
    in
    Dict.fromList
        ( List.map
            (\v ->
                ( v
                , (
                    let
                        s= toFloat v * step
                    in
                    if s <= 0.0404482362771082
                        then s * 0.0773993808
                        else ((s * 0.9478672986) + 0.0521327014) ^ 2.4
                  )
                )
            )
            <| List.range 0 255
        )
{-
sRgbToLinear : Dict Int Float
sRgbToLinear=
    let
        step = 1.0 / 255.0
    in
    Dict.fromList
        ( List.map
            (\v ->
                ( v
                , (
                    let
                        s= toFloat v * step
                    in
                    if s <= 0.0404482362771082
                        then s / 12.92
                        else ((s + 0.055) / 1.055) ^ 2.4
                  ) --* 1.1236
                )
            )
            <| List.range 0 255
        )
-}

{-
sRgbToLinear= 
    Dict.fromList <| List.indexedMap Tuple.pair
          [ 0.0, 3.035269835488375e-4, 6.07053967097675e-4, 9.105809506465125e-4,
            0.00121410793419535,  0.0015176349177441874,  0.001821161901293025,  0.0021246888848418626,
            0.0024282158683907,  0.0027317428519395373,   0.003035269835488375,  0.003346535763899161,
            0.003676507324047436,  0.004024717018496307,  0.004391442037410293,  0.004776953480693729,
            0.005181516702338386,  0.005605391624202723,  0.006048833022857054,  0.006512090792594475,
            0.006995410187265387,  0.007499032043226175,  0.008023192985384994,  0.008568125618069307,
            0.009134058702220787,  0.00972121732023785,   0.010329823029626936,  0.010960094006488246,
            0.011612245179743885,  0.012286488356915872,  0.012983032342173012,  0.013702083047289686,
            0.014443843596092545,  0.01520851442291271,   0.01599629336550963,  0.016807375752887384,
            0.017641954488384078,  0.018500220128379697,  0.019382360956935723, 0.0202885630566524,
            0.021219010376003555,  0.022173884793387385,  0.02315336617811041,  0.024157632448504756,
            0.02518685962736163,  0.026241221894849898,   0.027320891639074894, 0.028426039504420793,
            0.0295568344378088,  0.030713443732993635,    0.03189603307301153,  0.033104766570885055,
            0.03433980680868217,  0.03560131487502034,    0.03688945040110004,  0.0382043715953465,
            0.03954623527673284,  0.04091519690685319,    0.042311410620809675, 0.043735029256973465,
            0.04518620438567554,  0.046665086336880095,   0.04817182422688942,  0.04970656598412723,
            0.05126945837404324,  0.052860647023180246,   0.05448027644244237,  0.05612849004960009,
            0.05780543019106723,  0.0595112381629812,  0.06124605423161761, 0.06301001765316767,
            0.06480326669290577,  0.06662593864377289, 0.06847816984440017, 0.07036009569659588,
            0.07227185068231748,  0.07421356838014963, 0.07618538148130785, 0.07818742180518633,
            0.08021982031446832,  0.0822827071298148,  0.08437621154414882, 0.08650046203654976,
            0.08865558628577294,  0.09084171118340768, 0.09305896284668745, 0.0953074666309647,
            0.09758734714186246,  0.09989872824711389, 0.10224173308810132, 0.10461648409110419,
            0.10702310297826761,  0.10946171077829933, 0.1119324278369056,  0.11443537382697373,
            0.11697066775851084,  0.11953842798834562, 0.12213877222960187, 0.12477181756095049,
            0.12743768043564743,  0.1301364766903643,  0.13286832155381798, 0.13563332965520566,
            0.13843161503245183,  0.14126329114027164, 0.14412847085805777, 0.14702726649759498,
            0.14995978981060856,  0.15292615199615017, 0.1559264637078274,  0.1589608350608804,
            0.162029375639111,  0.1651321945016676,    0.16826940018969075, 0.1714411007328226,
            0.17464740365558504,  0.17788841598362912, 0.18116424424986022, 0.184474994500441,
            0.18782077230067787,  0.19120168274079138, 0.1946178304415758,  0.19806931955994886,
            0.20155625379439707,  0.20507873639031693, 0.20863687014525575, 0.21223075741405523,
            0.21586050011389926,  0.2195261997292692,  0.2232279573168085,  0.22696587351009836,
            0.23074004852434915,  0.23455058216100522, 0.238397573812271,   0.24228112246555486,   -- ca. 50%
            0.24620132670783548,  0.25015828472995344, 0.25415209433082675, 0.2581828529215958,
            0.26225065752969623,  0.26635560480286247, 0.2704977910130658,  0.27467731206038465,
            0.2788942634768104,   0.2831487404299921,  0.2874408377269175,  0.29177064981753587,
            0.2961382707983211,   0.3005437944157765,  0.3049873140698863,  0.30946892281750854,
            0.31398871337571754,  0.31854677812509186, 0.32314320911295075, 0.3277780980565422,
            0.33245153634617935,  0.33716361504833037, 0.3419144249086609,  0.3467040563550296,
            0.35153259950043936,  0.3564001441459435,  0.3613067797835095,  0.3662525955988395,
            0.3712376804741491,   0.3762621229909065,  0.38132601143253014, 0.386429433787049,
            0.39157247774972326,  0.39675523072562685, 0.4019777798321958,  0.4072402119017367,
            0.41254261348390375,  0.4178850708481375,  0.4232676699860717,  0.4286904966139066,
            0.43415363617474895,  0.4396571738409188,  0.44520119451622786, 0.45078578283822346,
            0.45641102318040466,  0.4620769996544071,  0.467783796112159,  0.47353149614800955,
            0.4793201831008268,   0.4851499400560704,  0.4910208498478356, 0.4969329950608704,
            0.5028864580325687,   0.5088813208549338,  0.5149176653765214, 0.5209955732043543,
            0.5271151257058131,   0.5332764040105052,  0.5394794890121072, 0.5457244613701866,
            0.5520114015120001,   0.5583403896342679,  0.5647115057049292, 0.5711248294648731,
            0.5775804404296506,   0.5840784178911641,  0.5906188409193369, 0.5972017883637634,
            0.6038273388553378,   0.6104955708078648,  0.6172065624196511, 0.6239603916750761,
            0.6307571363461468,   0.6375968739940326,  0.6444796819705821, 0.6514056374198242,
            0.6583748172794485,   0.665387298282272,   0.6724431569576875, 0.6795424696330938,
            0.6866853124353135,   0.6938717612919899,  0.7011018919329731, 0.7083757798916868,
            0.7156935005064807,   0.7230551289219693,  0.7304607400903537, 0.7379104087727308,
            0.7454042095403874,   0.7529422167760779,  0.7605245046752924, 0.768151147247507,
            0.7758222183174236,   0.7835377915261935,  0.7912979403326302, 0.799102738014409,
            0.8069522576692516,   0.8148465722161012,  0.8227857543962835, 0.8307698767746546,
            0.83879901174074,     0.846873231509858,   0.8549926081242338, 0.8631572134541023,
            0.8713671191987972,   0.8796223968878317,  0.8879231178819663, 0.8962693533742664,
            0.9046611743911496,   0.9130986517934192,  0.9215818562772946, 0.9301108583754237,
            0.938685728457888,    0.9473065367331999,  0.9559733532492861, 0.9646862478944651,
            0.9734452903984125,   0.9822505503331171,  0.9911020971138298, 1.0
          ]
-}

toLinearStr : (Int, Int, Int) -> String
toLinearStr (r, g, b) =
    "(" ++ String.fromFloat (Maybe.withDefault 0.0 <| Dict.get r sRgbToLinear) ++
    ", "++ String.fromFloat (Maybe.withDefault 0.0 <| Dict.get g sRgbToLinear) ++
    ", "++ String.fromFloat (Maybe.withDefault 0.0 <| Dict.get b sRgbToLinear) ++
    ")"
 

idFromIdx : Int -> String
idFromIdx idx=
    if idx <= 26
        then String.fromChar <| Char.fromCode (65+idx)         -- A..Z
        else if idx <= 52
            then String.fromChar <| Char.fromCode (97 + idx - 26)  -- a..z
            else if idx <= 62
                then String.fromChar <| Char.fromCode (48 + idx - 52)  -- 0..9
                else ( "_"++String.fromInt idx )


simpleLooks : String -> String -> List UsdInput -> List ((String, List (String, String)), (Int, Int, Int)) -> List Look
simpleLooks prefix meshId extra entries =
    List.map
        (\(idx, ((rawName, nameList), sRGB)) ->
            let
                id= idFromIdx idx
                name= fixDescr rawName
                num = sanitize "" <| Maybe.withDefault id <| List.head <| String.split " " name
                linearColor= toLinearStr sRGB
           in
            { id= prefix ++ id
            , description= Dict.fromList nameList
            , expandedView= False
            , thumbnail= Nothing
            , shopId= Nothing
            , btnScaleU= 1.0
            , btnScaleV= 1.0
            , meshId= meshId
            , material=
                { id= "mat"++ prefix ++ num
                , inputs=
                    (UsdInput "diffuseColor" linearColor Nothing) :: extra
                }
            }
        )
        (List.indexedMap Tuple.pair entries)


simpleLooksWithShop : String -> String -> List UsdInput -> List ((String, List (String, String)), Maybe String, (Int, Int, Int)) -> List Look
simpleLooksWithShop prefix meshId extra entries =
    List.map
        (\(idx, ((rawName, nameList), shop, raw)) ->
            let
                id= idFromIdx idx
                name= fixDescr rawName
                num = sanitize "" <| Maybe.withDefault id <| List.head <| String.split " " name
                linearColor= toLinearStr raw
           in
            { id= prefix ++ id
            , description= Dict.fromList nameList
            , expandedView= False
            , thumbnail= Nothing
            , shopId= shop
            , btnScaleU= 1.0
            , btnScaleV= 1.0
            , meshId= meshId
            , material=
                { id= "mat"++ prefix ++ num
                , inputs=
                    (UsdInput "diffuseColor" linearColor Nothing) :: extra
                }
            }
        )
        (List.indexedMap Tuple.pair entries)


lowRoughness : Float
lowRoughness = 0.5
defaultRoughness : Float
defaultRoughness = 0.75
satinRoughness : Float
satinRoughness = 0.9

ralK5Dict : Dict String (String, LanguageString)
ralK5Dict =
    Dict.fromList -- Gelb und Beige
        [ ( "1000", ( toLinearStr ( 190, 189, 127 ), Dict.fromList [ ( "de", "Grünbeige" )      , ( "en", "Green beige"             ), ( "fr", "Beige vert"             ), ( "it", "Beige verdastro"        ), ( "es", "Beige verdoso"          ) ] ) )
        , ( "1001", ( toLinearStr ( 194, 176, 120 ), Dict.fromList [ ( "de", "Beige" )          , ( "en", "Beige"                   ), ( "fr", "Beige"                  ), ( "it", "Beige"                  ), ( "es", "Beige"                  ) ] ) )
        , ( "1002", ( toLinearStr ( 198, 166, 100 ), Dict.fromList [ ( "de", "Sandgelb" )       , ( "en", "Sand yellow"             ), ( "fr", "Jaune sable"            ), ( "it", "Giallo sabbia"          ), ( "es", "Amarillo arena"         ) ] ) )
        , ( "1003", ( toLinearStr ( 229, 190,   1 ), Dict.fromList [ ( "de", "Signalgelb" )     , ( "en", "Signal yellow"           ), ( "fr", "Jaune de sécurité"      ), ( "it", "Giallo segnale"         ), ( "es", "Amarillo señales"       ) ] ) )
        , ( "1004", ( toLinearStr ( 205, 164,  52 ), Dict.fromList [ ( "de", "Goldgelb" )       , ( "en", "Golden yellow"           ), ( "fr", "Jaune or"               ), ( "it", "Giallo oro"             ), ( "es", "Amarillo oro"           ) ] ) )
        , ( "1005", ( toLinearStr ( 169, 131,   7 ), Dict.fromList [ ( "de", "Honiggelb" )      , ( "en", "Honey yellow"            ), ( "fr", "Jaune miel"             ), ( "it", "Giallo miele"           ), ( "es", "Amarillo miel"          ) ] ) )
        , ( "1006", ( toLinearStr ( 228, 160,  16 ), Dict.fromList [ ( "de", "Maisgelb" )       , ( "en", "Maize yellow"            ), ( "fr", "Jaune maïs"             ), ( "it", "Giallo polenta"         ), ( "es", "Amarillo maiz"          ) ] ) )
        , ( "1007", ( toLinearStr ( 220, 156,   0 ), Dict.fromList [ ( "de", "Narzissengelb" )  , ( "en", "Daffodil yellow"         ), ( "fr", "Jaune narcisse"         ), ( "it", "Giallo narciso"         ), ( "es", "Amarillo narciso"       ) ] ) )
        , ( "1011", ( toLinearStr ( 138, 102,  66 ), Dict.fromList [ ( "de", "Braunbeige" )     , ( "en", "Brown beige"             ), ( "fr", "Beige brun"             ), ( "it", "Beige marrone"          ), ( "es", "Beige pardo"            ) ] ) )
        , ( "1012", ( toLinearStr ( 199, 180,  70 ), Dict.fromList [ ( "de", "Zitronengelb" )   , ( "en", "Lemon yellow"            ), ( "fr", "Jaune citron"           ), ( "it", "Giallo limone"          ), ( "es", "Amarillo limón"         ) ] ) )
        , ( "1013", ( toLinearStr ( 234, 230, 202 ), Dict.fromList [ ( "de", "Perlweiß" )       , ( "en", "Oyster white"            ), ( "fr", "Blanc perlé"            ), ( "it", "Bianco perla"           ), ( "es", "Blanco perla"           ) ] ) )
        , ( "1014", ( toLinearStr ( 225, 204,  79 ), Dict.fromList [ ( "de", "Elfenbein" )      , ( "en", "Ivory"                   ), ( "fr", "Ivoire"                 ), ( "it", "Avorio"                 ), ( "es", "Marfil"                 ) ] ) )
        , ( "1015", ( toLinearStr ( 230, 214, 144 ), Dict.fromList [ ( "de", "Hellelfenbein" )  , ( "en", "Light ivory"             ), ( "fr", "Ivoire clair"           ), ( "it", "Avorio chiaro"          ), ( "es", "Marfil claro"           ) ] ) )
        , ( "1016", ( toLinearStr ( 237, 255,  33 ), Dict.fromList [ ( "de", "Schwefelgelb" )   , ( "en", "Sulfur yellow"           ), ( "fr", "Jaune soufre"           ), ( "it", "Giallo zolfo"           ), ( "es", "Amarillo azufre"        ) ] ) )
        , ( "1017", ( toLinearStr ( 245, 208,  51 ), Dict.fromList [ ( "de", "Safrangelb" )     , ( "en", "Saffron yellow"          ), ( "fr", "Jaune safran"           ), ( "it", "Giallo zafferano"       ), ( "es", "Amarillo azafrán"       ) ] ) )
        , ( "1018", ( toLinearStr ( 248, 243,  53 ), Dict.fromList [ ( "de", "Zinkgelb" )       , ( "en", "Zinc yellow"             ), ( "fr", "Jaune zinc"             ), ( "it", "Giallo zinco"           ), ( "es", "Amarillo de zinc"       ) ] ) )
        , ( "1019", ( toLinearStr ( 158, 151, 100 ), Dict.fromList [ ( "de", "Graubeige" )      , ( "en", "Grey beige"              ), ( "fr", "Beige gris"             ), ( "it", "Beige grigiastro"       ), ( "es", "Beige agrisado"         ) ] ) )
        , ( "1020", ( toLinearStr ( 153, 153,  80 ), Dict.fromList [ ( "de", "Olivgelb" )       , ( "en", "Olive yellow"            ), ( "fr", "Jaune olive"            ), ( "it", "Giallo olivastro"       ), ( "es", "Amarillo oliva"         ) ] ) )
        , ( "1021", ( toLinearStr ( 243, 218,  11 ), Dict.fromList [ ( "de", "Rapsgelb" )       , ( "en", "Rape yellow"             ), ( "fr", "Jaune colza"            ), ( "it", "Giallo navone"          ), ( "es", "Amarillo colza"         ) ] ) )
        , ( "1023", ( toLinearStr ( 250, 210,   1 ), Dict.fromList [ ( "de", "Verkehrsgelb" )   , ( "en", "Traffic yellow"          ), ( "fr", "Jaune signalisation"    ), ( "it", "Giallo traffico"        ), ( "es", "Amarillo tráfico"       ) ] ) )
        , ( "1024", ( toLinearStr ( 174, 160,  75 ), Dict.fromList [ ( "de", "Ockergelb" )      , ( "en", "Ochre yellow"            ), ( "fr", "Jaune ocre"             ), ( "it", "Giallo ocra"            ), ( "es", "Amarillo ocre"          ) ] ) )
        , ( "1026", ( toLinearStr ( 255, 255,   0 ), Dict.fromList [ ( "de", "Leuchtgelb" )     , ( "en", "Luminous yellow"         ), ( "fr", "Jaune brillant"         ), ( "it", "Giallo brillante"       ), ( "es", "Amarillo brillante"     ) ] ) )
        , ( "1027", ( toLinearStr ( 157, 145,   1 ), Dict.fromList [ ( "de", "Currygelb" )      , ( "en", "Curry"                   ), ( "fr", "Jaune curry"            ), ( "it", "Giallo curry"           ), ( "es", "Amarillo curry"         ) ] ) )
        , ( "1028", ( toLinearStr ( 244, 169,   0 ), Dict.fromList [ ( "de", "Melonengelb" )    , ( "en", "Melon yellow"            ), ( "fr", "Jaune melon"            ), ( "it", "Giallo melone"          ), ( "es", "Amarillo melón"         ) ] ) )
        , ( "1032", ( toLinearStr ( 214, 174,   1 ), Dict.fromList [ ( "de", "Ginstergelb" )    , ( "en", "Broom yellow"            ), ( "fr", "Jaune genêt"            ), ( "it", "Giallo scopa"           ), ( "es", "Amarillo retama"        ) ] ) )
        , ( "1033", ( toLinearStr ( 243, 165,   5 ), Dict.fromList [ ( "de", "Dahliengelb" )    , ( "en", "Dahlia yellow"           ), ( "fr", "Jaune dahlia"           ), ( "it", "Giallo dahlien"         ), ( "es", "Amarillo dalia"         ) ] ) )
        , ( "1034", ( toLinearStr ( 239, 169,  74 ), Dict.fromList [ ( "de", "Pastellgelb" )    , ( "en", "Pastel yellow"           ), ( "fr", "Jaune pastel"           ), ( "it", "Giallo pastello"        ), ( "es", "Amarillo pastel"        ) ] ) )
        , ( "1035", ( toLinearStr ( 106,  93,  77 ), Dict.fromList [ ( "de", "Perlbeige" )      , ( "en", "Pearl beige"             ), ( "fr", "Beige nacré"            ), ( "it", "Beige perlato"          ), ( "es", "Beige perlado"          ) ] ) )
        , ( "1036", ( toLinearStr ( 112,  83,  53 ), Dict.fromList [ ( "de", "Perlgold" )       , ( "en", "Pearl gold"              ), ( "fr", "Or nacré"               ), ( "it", "Oro perlato"            ), ( "es", "Oro perlado"            ) ] ) )
        , ( "1037", ( toLinearStr ( 243, 159,  24 ), Dict.fromList [ ( "de", "Sonnengelb" )     , ( "en", "Sun yellow"              ), ( "fr", "Jaune soleil"           ), ( "it", "Giallo sole"            ), ( "es", "Amarillo sol"           ) ] ) )

        -- Orange
        , ( "2000", ( toLinearStr ( 237, 118,  14 ), Dict.fromList [ ( "de", "Gelborange" )     , ( "en", "Yellow orange"           ), ( "fr", "Orangé jaune"           ), ( "it", "Arancio giallastro"     ), ( "es", "Amarillo naranja"       ) ] ) )
        , ( "2001", ( toLinearStr ( 201,  60,  32 ), Dict.fromList [ ( "de", "Rotorange" )      , ( "en", "Red orange"              ), ( "fr", "Orangé rouge"           ), ( "it", "Arancio rossastro"      ), ( "es", "Rojo anaranjado"        ) ] ) )
        , ( "2002", ( toLinearStr ( 203,  40,  33 ), Dict.fromList [ ( "de", "Blutorange" )     , ( "en", "Vermilion"               ), ( "fr", "Orangé sang"            ), ( "it", "Arancio sanguigno"      ), ( "es", "Naranja sanguineo"      ) ] ) )
        , ( "2003", ( toLinearStr ( 255, 117,  20 ), Dict.fromList [ ( "de", "Pastellorange" )  , ( "en", "Pastel orange"           ), ( "fr", "Orangé pastel"          ), ( "it", "Arancio pastello"       ), ( "es", "Naranja pálido"         ) ] ) )
        , ( "2004", ( toLinearStr ( 244,  70,  17 ), Dict.fromList [ ( "de", "Reinorange" )     , ( "en", "Pure orange"             ), ( "fr", "Orangé pur"             ), ( "it", "Arancio puro"           ), ( "es", "Naranja puro"           ) ] ) )
        , ( "2005", ( toLinearStr ( 255,  35,   1 ), Dict.fromList [ ( "de", "Leuchtorange" )   , ( "en", "Luminous orange"         ), ( "fr", "Orangé brillant"        ), ( "it", "Arancio brillante"      ), ( "es", "Naranja brillante"      ) ] ) )
        , ( "2007", ( toLinearStr ( 255, 164,  32 ), Dict.fromList [ ( "de", "Leuchthellorange"), ( "en", "Luminous bright orange"  ), ( "fr", "Orangé clair brillant"  ), ( "it", "Arancio chiaro brillante"),( "es", "Naranja claro brillante") ] ) )
        , ( "2008", ( toLinearStr ( 247,  94,  37 ), Dict.fromList [ ( "de", "Hellrotorange" )  , ( "en", "Bright red orange"       ), ( "fr", "Orangé rouge clair"     ), ( "it", "Rosso arancio chiaro"   ), ( "es", "Rojo claro anaranjado"  ) ] ) )
        , ( "2009", ( toLinearStr ( 245,  64,  33 ), Dict.fromList [ ( "de", "Verkehrsorange" ) , ( "en", "Traffic orange"          ), ( "fr", "Orangé signalisation"   ), ( "it", "Arancio traffico"       ), ( "es", "Naranja tráfico"        ) ] ) )
        , ( "2010", ( toLinearStr ( 216,  75,  32 ), Dict.fromList [ ( "de", "Signalorange" )   , ( "en", "Signal orange"           ), ( "fr", "Orangé de sécurité"     ), ( "it", "Arancio segnale"        ), ( "es", "Naranja señales"        ) ] ) )
        , ( "2011", ( toLinearStr ( 236, 124,  38 ), Dict.fromList [ ( "de", "Tieforange" )     , ( "en", "Deep orange"             ), ( "fr", "Orangé foncé"           ), ( "it", "Arancio profondo"       ), ( "es", "Naranja intenso"        ) ] ) )
        , ( "2012", ( toLinearStr ( 235, 106,  14 ), Dict.fromList [ ( "de", "Lachsorange" )    , ( "en", "Salmon orange"           ), ( "fr", "Orangé saumon"          ), ( "it", "Arancio salmone"        ), ( "es", "Naranja salmón"         ) ] ) )
        , ( "2013", ( toLinearStr ( 195,  88,  49 ), Dict.fromList [ ( "de", "Perlorange" )     , ( "en", "Pearl orange"            ), ( "fr", "Orangé nacré"           ), ( "it", "Arancio perlato"        ), ( "es", "Naranja perlado"        ) ] ) )
        , ( "2017", ( toLinearStr ( 250,  68,   2 ), Dict.fromList [ ( "de", "RAL Orange" ) ] ) )

        -- Rot
        , ( "3000", ( toLinearStr ( 175,  43,  30 ), Dict.fromList [ ( "de", "Feuerrot" )       , ( "en", "Flame red"               ), ( "fr", "Rouge feu"              ), ( "it", "Rosso fuoco"            ), ( "es", "Rojo vivo"              ) ] ) )
        , ( "3001", ( toLinearStr ( 165,  32,  25 ), Dict.fromList [ ( "de", "Signalrot" )      , ( "en", "Signal red"              ), ( "fr", "Rouge de sécurité"      ), ( "it", "Rosso  segnale"         ), ( "es", "Rojo señales"           ) ] ) )
        , ( "3002", ( toLinearStr ( 162,  35,  29 ), Dict.fromList [ ( "de", "Karminrot" )      , ( "en", "Carmine red"             ), ( "fr", "Rouge carmin"           ), ( "it", "Rosso carminio"         ), ( "es", "Rojo carmin"            ) ] ) )
        , ( "3003", ( toLinearStr ( 155,  17,  30 ), Dict.fromList [ ( "de", "Rubinrot" )       , ( "en", "Ruby red"                ), ( "fr", "Rouge rubis"            ), ( "it", "Rosso rubino"           ), ( "es", "Rojo rubí"              ) ] ) )
        , ( "3004", ( toLinearStr ( 117,  21,  30 ), Dict.fromList [ ( "de", "Purpurrot" )      , ( "en", "Purple red"              ), ( "fr", "Rouge pourpre"          ), ( "it", "Rosso porpora"          ), ( "es", "Rojo purpura"           ) ] ) )
        , ( "3005", ( toLinearStr (  94,  33,  41 ), Dict.fromList [ ( "de", "Weinrot" )        , ( "en", "Wine red"                ), ( "fr", "Rouge vin"              ), ( "it", "Rosso vino"             ), ( "es", "Rojo vino"              ) ] ) )
        , ( "3007", ( toLinearStr (  65,  34,  39 ), Dict.fromList [ ( "de", "Schwarzrot" )     , ( "en", "Black red"               ), ( "fr", "Rouge noir"             ), ( "it", "Rosso nerastro"         ), ( "es", "Rojo negruzco"          ) ] ) )
        , ( "3009", ( toLinearStr ( 100,  36,  36 ), Dict.fromList [ ( "de", "Oxidrot" )        , ( "en", "Oxide red"               ), ( "fr", "Rouge oxyde"            ), ( "it", "Rosso  ossido"          ), ( "es", "Rojo óxido"             ) ] ) )
        , ( "3011", ( toLinearStr ( 120,  31,  25 ), Dict.fromList [ ( "de", "Braunrot" )       , ( "en", "Brown red"               ), ( "fr", "Rouge brun"             ), ( "it", "Rosso marrone"          ), ( "es", "Rojo pardo"             ) ] ) )
        , ( "3012", ( toLinearStr ( 193, 135, 107 ), Dict.fromList [ ( "de", "Beigerot" )       , ( "en", "Beige red"               ), ( "fr", "Rouge beige"            ), ( "it", "Rosso beige"            ), ( "es", "Rojo beige"             ) ] ) )
        , ( "3013", ( toLinearStr ( 161,  35,  18 ), Dict.fromList [ ( "de", "Tomatenrot" )     , ( "en", "Tomato red"              ), ( "fr", "Rouge tomate"           ), ( "it", "Rosso pomodoro"         ), ( "es", "Rojo tomate"            ) ] ) )
        , ( "3014", ( toLinearStr ( 211, 110, 112 ), Dict.fromList [ ( "de", "Altrosa" )        , ( "en", "Antique pink"            ), ( "fr", "Vieux rose"             ), ( "it", "Rosa antico"            ), ( "es", "Rojo viejo"             ) ] ) )
        , ( "3015", ( toLinearStr ( 234, 137, 154 ), Dict.fromList [ ( "de", "Hellrosa" )       , ( "en", "Light pink"              ), ( "fr", "Rose clair"             ), ( "it", "Rosa chiaro"            ), ( "es", "Rosa claro"             ) ] ) )
        , ( "3016", ( toLinearStr ( 179,  40,  33 ), Dict.fromList [ ( "de", "Korallenrot" )    , ( "en", "Coral red"               ), ( "fr", "Rouge corail"           ), ( "it", "Rosso corallo"          ), ( "es", "Rojo coral"             ) ] ) )
        , ( "3017", ( toLinearStr ( 230,  50,  68 ), Dict.fromList [ ( "de", "Rosé" )           , ( "en", "Rose"                    ), ( "fr", "Rosé"                   ), ( "it", "Rosato"                 ), ( "es", "Rosa"                   ) ] ) )
        , ( "3018", ( toLinearStr ( 213,  48,  50 ), Dict.fromList [ ( "de", "Erdbeerrot" )     , ( "en", "Strawberry red"          ), ( "fr", "Rouge fraise"           ), ( "it", "Rosso fragola"          ), ( "es", "Rojo fresa"             ) ] ) )
        , ( "3020", ( toLinearStr ( 204,   6,   5 ), Dict.fromList [ ( "de", "Verkehrsrot" )    , ( "en", "Traffic red"             ), ( "fr", "Rouge signalisation"    ), ( "it", "Rosso traffico"         ), ( "es", "Rojo tráfico"           ) ] ) )
        , ( "3022", ( toLinearStr ( 217,  80,  48 ), Dict.fromList [ ( "de", "Lachsrot" )       , ( "en", "Salmon pink"             ), ( "fr", "Rouge saumon"           ), ( "it", "Rosso salmone"          ), ( "es", "Rojo salmón"            ) ] ) )
        , ( "3024", ( toLinearStr ( 248,   0,   0 ), Dict.fromList [ ( "de", "Leuchtrot" )      , ( "en", "Luminous red"            ), ( "fr", "Rouge brillant"         ), ( "it", "Rosso brillante"        ), ( "es", "Rojo brillante"         ) ] ) )
        , ( "3026", ( toLinearStr ( 254,   0,   0 ), Dict.fromList [ ( "de", "Leuchthellrot" )  , ( "en", "Luminous breight red"    ), ( "fr", "Rouge clair brillant"   ), ( "it", "Rosso chiaro brillante" ), ( "es", "Rojo claro brillante"   ) ] ) )
        , ( "3027", ( toLinearStr ( 197,  29,  52 ), Dict.fromList [ ( "de", "Himbeerrot" )     , ( "en", "Raspberry red"           ), ( "fr", "Rouge framboise"        ), ( "it", "Rosso lampone"          ), ( "es", "Rojo frambuesa"         ) ] ) )
        , ( "3028", ( toLinearStr ( 231,  37,  18 ), Dict.fromList [ ( "de", "Reinrot" )        , ( "en", "Pure red"                ), ( "fr", "Rouge puro"             ), ( "it", "Rosso puro"             ), ( "es", "Rojo puro"              ) ] ) )
        , ( "3031", ( toLinearStr ( 179,  36,  40 ), Dict.fromList [ ( "de", "Orientrot" )      , ( "en", "Orient red"              ), ( "fr", "Rouge oriental"         ), ( "it", "Rosso oriente"          ), ( "es", "Rojo oriente"           ) ] ) )
        , ( "3032", ( toLinearStr ( 114,  20,  34 ), Dict.fromList [ ( "de", "Perlrubinrot" )   , ( "en", "Pearl ruby red"          ), ( "fr", "Rouge rubis nacré"      ), ( "it", "Rosso rubino perlato"   ), ( "es", "Rojo rubí perlado"      ) ] ) )
        , ( "3033", ( toLinearStr ( 180,  76,  67 ), Dict.fromList [ ( "de", "Perlrosa" )       , ( "en", "Pearl pink"              ), ( "fr", "Rose nacré"             ), ( "it", "Rosa perlato"           ), ( "es", "Rosa perlado"           ) ] ) )

        -- Violett
        , ( "4001", ( toLinearStr ( 222,  76, 138 ), Dict.fromList [ ( "de", "Rotlila" )        , ( "en", "Red lilac"               ), ( "fr", "Lilas rouge"            ), ( "it", "Lilla rossastro"        ), ( "es", "Rojo lila"              ) ] ) )
        , ( "4002", ( toLinearStr ( 146,  43,  62 ), Dict.fromList [ ( "de", "Rotviolett" )     , ( "en", "Red violet"              ), ( "fr", "Violet rouge"           ), ( "it", "Viola rossastro"        ), ( "es", "Rojo violeta"           ) ] ) )
        , ( "4003", ( toLinearStr ( 222,  76, 138 ), Dict.fromList [ ( "de", "Erikaviolett" )   , ( "en", "Heather violet"          ), ( "fr", "Violet bruyère"         ), ( "it", "Viola erica"            ), ( "es", "Violeta érica"          ) ] ) )
        , ( "4004", ( toLinearStr ( 110,  28,  52 ), Dict.fromList [ ( "de", "Bordeauxviolett" ), ( "en", "Claret violet"           ), ( "fr", "Violet bordeaux"        ), ( "it", "Viola bordeaux"         ), ( "es", "Burdeos"                ) ] ) )
        , ( "4005", ( toLinearStr ( 108,  70, 117 ), Dict.fromList [ ( "de", "Blaulila" )       , ( "en", "Blue lilac"              ), ( "fr", "Lilas bleu"             ), ( "it", "Lilla bluastro"         ), ( "es", "Lila azulado"           ) ] ) )
        , ( "4006", ( toLinearStr ( 160,  52, 114 ), Dict.fromList [ ( "de", "Verkehrspurpur" ) , ( "en", "Traffic purple"          ), ( "fr", "Pourpre signalisation"  ), ( "it", "Porpora traffico"       ), ( "es", "Púrpurá tráfico"        ) ] ) )
        , ( "4007", ( toLinearStr (  74,  25,  44 ), Dict.fromList [ ( "de", "Purpurviolett" )  , ( "en", "Purple violet"           ), ( "fr", "Violet pourpre"         ), ( "it", "Porpora violetto"       ), ( "es", "Violeta púrpura"        ) ] ) )
        , ( "4008", ( toLinearStr ( 140,  86, 138 ), Dict.fromList [ ( "de", "Signalviolett" )  , ( "en", "Signal violet"           ), ( "fr", "Violet de sécurité"     ), ( "it", "Violetto segnale"       ), ( "es", "Violeta señales"        ) ] ) )
        , ( "4009", ( toLinearStr ( 164, 125, 144 ), Dict.fromList [ ( "de", "Pastellviolett" ) , ( "en", "Pastel violet"           ), ( "fr", "Violet pastel"          ), ( "it", "Violetto pastello"      ), ( "es", "Violeta pastel"         ) ] ) )
        , ( "4010", ( toLinearStr ( 215,  45, 109 ), Dict.fromList [ ( "de", "Telemagenta" )    , ( "en", "Telemagenta"             ), ( "fr", "Telemagenta"            ), ( "it", "Tele Magenta"           ), ( "es", "Magenta tele"           ) ] ) )
        , ( "4011", ( toLinearStr ( 134, 115, 161 ), Dict.fromList [ ( "de", "Perlviolett" )    , ( "en", "Pearl violet"            ), ( "fr", "Violet nacré"           ), ( "it", "Violetto perlato"       ), ( "es", "Violeta perlado"        ) ] ) )
        , ( "4012", ( toLinearStr ( 108, 104, 129 ), Dict.fromList [ ( "de", "Perlbrombeer" )   , ( "en", "Pearl black berry"       ), ( "fr", "Mûre nacré"             ), ( "it", "Mora perlato"           ), ( "es", "Morado perlado"         ) ] ) )

        -- Blau
        , ( "5000",    ( toLinearStr (  42,  46,  75 ), Dict.fromList [ ( "de", "Violettblau" )    , ( "en", "Violet blue"             ), ( "fr", "Bleu violet"            ), ( "it", "Blu violaceo"           ), ( "es", "Azul violeta"           ) ] ) )
        , ( "5001",    ( toLinearStr (  31,  52,  56 ), Dict.fromList [ ( "de", "Grünblau" )       , ( "en", "Green blue"              ), ( "fr", "Bleu vert"              ), ( "it", "Blu verdastro"          ), ( "es", "Azul verdoso"           ) ] ) )
        , ( "5002",    ( toLinearStr (  32,  33,  79 ), Dict.fromList [ ( "de", "Ultramarinblau" ) , ( "en", "Ultramarine blue"        ), ( "fr", "Bleu outremer"          ), ( "it", "Blu oltremare"          ), ( "es", "Azul ultramar"          ) ] ) )
        , ( "5003",    ( toLinearStr (  29,  30,  51 ), Dict.fromList [ ( "de", "Saphirblau" )     , ( "en", "Saphire blue"            ), ( "fr", "Bleu saphir"            ), ( "it", "Blu zaffiro"            ), ( "es", "Azul zafiro"            ) ] ) )
        , ( "5004",    ( toLinearStr (  32,  33,  79 ), Dict.fromList [ ( "de", "Schwarzblau" )    , ( "en", "Black blue"              ), ( "fr", "Bleu noir"              ), ( "it", "Blu nerastro"           ), ( "es", "Azul negruzco"          ) ] ) )
        , ( "5005",    ( toLinearStr (  30,  45, 110 ), Dict.fromList [ ( "de", "Signalblau" )     , ( "en", "Signal blue"             ), ( "fr", "Bleu de sécurité"       ), ( "it", "Blu segnale"            ), ( "es", "Azul señales"           ) ] ) )
        , ( "5007",    ( toLinearStr (  62,  95, 138 ), Dict.fromList [ ( "de", "Brillantblau" )   , ( "en", "Brillant blue"           ), ( "fr", "Bleu brillant"          ), ( "it", "Blu brillante"          ), ( "es", "Azul brillante"         ) ] ) )
        , ( "5008",    ( toLinearStr (  38,  37,  45 ), Dict.fromList [ ( "de", "Graublau" )       , ( "en", "Grey blue"               ), ( "fr", "Bleu gris"              ), ( "it", "Blu grigiastro"         ), ( "es", "Azul grisáceo"          ) ] ) )
        , ( "5009",    ( toLinearStr (   2,  86, 105 ), Dict.fromList [ ( "de", "Azurblau" )       , ( "en", "Azure blue"              ), ( "fr", "Bleu azur"              ), ( "it", "Blu  azzurro"           ), ( "es", "Azul azur"              ) ] ) )
        , ( "5010",    ( toLinearStr (  14,  41,  75 ), Dict.fromList [ ( "de", "Enzianblau" )     , ( "en", "Gentian blue"            ), ( "fr", "Bleu gentiane"          ), ( "it", "Blu  genziana"          ), ( "es", "Azul genciana"          ) ] ) )
        , ( "5011",    ( toLinearStr (  35,  26,  36 ), Dict.fromList [ ( "de", "Stahlblau" )      , ( "en", "Steel blue"              ), ( "fr", "Bleu acier"             ), ( "it", "Blu acciaio"            ), ( "es", "Azul acero"             ) ] ) )
        , ( "5012",    ( toLinearStr (  59, 131, 189 ), Dict.fromList [ ( "de", "Lichtblau" )      , ( "en", "Light blue"              ), ( "fr", "Bleu clair"             ), ( "it", "Blu luce"               ), ( "es", "Azul luminoso"          ) ] ) )
        , ( "5013",    ( toLinearStr (  37,  41,  74 ), Dict.fromList [ ( "de", "Kobaltblau" )     , ( "en", "Cobalt blue"             ), ( "fr", "Bleu cobalt"            ), ( "it", "Blu cobalto"            ), ( "es", "Azul cobalto"           ) ] ) )
        , ( "5014",    ( toLinearStr (  96, 111, 140 ), Dict.fromList [ ( "de", "Taubenblau" )     , ( "en", "Pigeon blue"             ), ( "fr", "Bleu pigeon"            ), ( "it", "Blu colomba"            ), ( "es", "Azul olombino"          ) ] ) )
        , ( "5015",    ( toLinearStr (  34, 113, 179 ), Dict.fromList [ ( "de", "Himmelblau" )     , ( "en", "Sky blue"                ), ( "fr", "Bleu ciel"              ), ( "it", "Blu cielo"              ), ( "es", "Azul celeste"           ) ] ) )
        , ( "5017",    ( toLinearStr (   6,  57, 113 ), Dict.fromList [ ( "de", "Verkehrsblau" )   , ( "en", "Traffic blue"            ), ( "fr", "Bleu signalisation"     ), ( "it", "Blu traffico"           ), ( "es", "Azul tráfico"           ) ] ) )
        , ( "5018",    ( toLinearStr (  63, 136, 143 ), Dict.fromList [ ( "de", "Türkisblau" )     , ( "en", "Turquoise blue"          ), ( "fr", "Bleu turquoise"         ), ( "it", "Blu turchese"           ), ( "es", "Azul turquesa"          ) ] ) )
        , ( "5019",    ( toLinearStr (  27,  85, 131 ), Dict.fromList [ ( "de", "Capriblau" )      , ( "en", "Capri blue"              ), ( "fr", "Bleu capri"             ), ( "it", "Blu capri"              ), ( "es", "Azul capri"             ) ] ) )
        , ( "5020",    ( toLinearStr (  29,  51,  74 ), Dict.fromList [ ( "de", "Ozeanblau" )      , ( "en", "Ocean blue"              ), ( "fr", "Bleu océan"             ), ( "it", "Blu oceano"             ), ( "es", "Azul oceano"            ) ] ) )
        , ( "5021",    ( toLinearStr (  37, 109, 123 ), Dict.fromList [ ( "de", "Wasserblau" )     , ( "en", "Water blue"              ), ( "fr", "Bleu d'eau"             ), ( "it", "Blu acqua"              ), ( "es", "Azul agua"              ) ] ) )
        , ( "5022",    ( toLinearStr (  37,  40,  80 ), Dict.fromList [ ( "de", "Nachtblau" )      , ( "en", "Night blue"              ), ( "fr", "Bleu nocturne"          ), ( "it", "Blu notte"              ), ( "es", "Azul noche"             ) ] ) )
        , ( "5023",    ( toLinearStr (  73, 103, 141 ), Dict.fromList [ ( "de", "Fernblau" )       , ( "en", "Distant blue"            ), ( "fr", "Bleu distant"           ), ( "it", "Blu distante"           ), ( "es", "Azul lejanía"           ) ] ) )
        , ( "5024",    ( toLinearStr (  93, 155, 155 ), Dict.fromList [ ( "de", "Pastellblau" )    , ( "en", "Pastel blue"             ), ( "fr", "Bleu pastel"            ), ( "it", "Blu pastello"           ), ( "es", "Azul pastel"            ) ] ) )
        , ( "5025",    ( toLinearStr (  42, 100, 120 ), Dict.fromList [ ( "de", "Perlenzian" )     , ( "en", "Pearl gentian blue"      ), ( "fr", "Gentiane nacré"         ), ( "it", "Blu genziana perlato"   ), ( "es", "Gencian perlado"        ) ] ) )
        , ( "5026",    ( toLinearStr (  16,  44,  84 ), Dict.fromList [ ( "de", "Perlnachtblau" )  , ( "en", "Pearl night blue"        ), ( "fr", "Bleu nuit nacré"        ), ( "it", "Blu notte perlato"      ), ( "es", "Azul noche perlado"     ) ] ) )

        -- Grün
        , ( "6000", ( toLinearStr (  49, 102,  80 ), Dict.fromList [ ( "de", "Patinagrün" )     , ( "en", "Patina green"            ), ( "fr", "ert patine"             ), ( "it", "Verde patina"           ), ( "es", "Verde patina"           ) ] ) )
        , ( "6001", ( toLinearStr (  40, 114,  51 ), Dict.fromList [ ( "de", "Smaragdgrün" )    , ( "en", "Emerald green"           ), ( "fr", "Vert émeraude"          ), ( "it", "Verde smeraldo"         ), ( "es", "Verde esmeralda"        ) ] ) )
        , ( "6002", ( toLinearStr (  45,  87,  44 ), Dict.fromList [ ( "de", "Laubgrün" )       , ( "en", "Leaf green"              ), ( "fr", "Vert feuillage"         ), ( "it", "Verde foglia"           ), ( "es", "Verde hoja"             ) ] ) )
        , ( "6003", ( toLinearStr (  66,  70,  50 ), Dict.fromList [ ( "de", "Olivgrün" )       , ( "en", "Olive green"             ), ( "fr", "Vert olive"             ), ( "it", "Verde oliva"            ), ( "es", "Verde oliva"            ) ] ) )
        , ( "6004", ( toLinearStr (  31,  58,  61 ), Dict.fromList [ ( "de", "Blaugrün" )       , ( "en", "Blue green"              ), ( "fr", "Vert bleu"              ), ( "it", "Verde bluastro"         ), ( "es", "Verde azulado"          ) ] ) )
        , ( "6005", ( toLinearStr (  47,  69,  56 ), Dict.fromList [ ( "de", "Moosgrün" )       , ( "en", "Moss green"              ), ( "fr", "Vert mousse"            ), ( "it", "Verde muschio"          ), ( "es", "Verde musgo"            ) ] ) )
        , ( "6006", ( toLinearStr (  62,  59,  50 ), Dict.fromList [ ( "de", "Grauoliv" )       , ( "en", "Grey olive"              ), ( "fr", "Olive gris"             ), ( "it", "Oliva grigiastro"       ), ( "es", "Oliva grisáceo"         ) ] ) )
        , ( "6007", ( toLinearStr (  52,  59,  41 ), Dict.fromList [ ( "de", "Flaschengrün" )   , ( "en", "Bottle green"            ), ( "fr", "Vert bouteille"         ), ( "it", "Verde bottiglia"        ), ( "es", "Verde botella"          ) ] ) )
        , ( "6008", ( toLinearStr (  57,  53,  42 ), Dict.fromList [ ( "de", "Braungrün" )      , ( "en", "Brown green"             ), ( "fr", "Vert brun"              ), ( "it", "Verde brunastro"        ), ( "es", "Verde parduzco"         ) ] ) )
        , ( "6009", ( toLinearStr (  49,  55,  43 ), Dict.fromList [ ( "de", "Tannengrün" )     , ( "en", "Fir green"               ), ( "fr", "Vert sapin"             ), ( "it", "Verde abete"            ), ( "es", "Verde abeto"            ) ] ) )
        , ( "6010", ( toLinearStr (  53, 104,  45 ), Dict.fromList [ ( "de", "Grasgrün" )       , ( "en", "Grass green"             ), ( "fr", "Vert herbe"             ), ( "it", "Verde erba"             ), ( "es", "Verde hierba"           ) ] ) )
        , ( "6011", ( toLinearStr (  88, 114,  70 ), Dict.fromList [ ( "de", "Resedagrün" )     , ( "en", "Reseda green"            ), ( "fr", "Vert réséda"            ), ( "it", "Verde reseda"           ), ( "es", "Verde reseda"           ) ] ) )
        , ( "6012", ( toLinearStr (  52,  62,  64 ), Dict.fromList [ ( "de", "Schwarzgrün" )    , ( "en", "Black green"             ), ( "fr", "Vert noir"              ), ( "it", "Verde nerastro"         ), ( "es", "Verde negruzco"         ) ] ) )
        , ( "6013", ( toLinearStr ( 108, 113,  86 ), Dict.fromList [ ( "de", "Schilfgrün" )     , ( "en", "Reed green"              ), ( "fr", "Vert jonc"              ), ( "it", "Verde canna"            ), ( "es", "Verde caña"             ) ] ) )
        , ( "6014", ( toLinearStr (  71,  64,  46 ), Dict.fromList [ ( "de", "Gelboliv" )       , ( "en", "Yellow olive"            ), ( "fr", "Olive jaune"            ), ( "it", "Oliva giallastro"       ), ( "es", "Amarillo oliva"         ) ] ) )
        , ( "6015", ( toLinearStr (  59,  60,  54 ), Dict.fromList [ ( "de", "Schwarzoliv" )    , ( "en", "Black olive"             ), ( "fr", "Olive noir"             ), ( "it", "Oliva nerastro"         ), ( "es", "Oliva negruzco"         ) ] ) )
        , ( "6016", ( toLinearStr (  30,  89,  69 ), Dict.fromList [ ( "de", "Türkisgrün" )     , ( "en", "Turquoise green"         ), ( "fr", "Vert turquoise"         ), ( "it", "Verde turchese"         ), ( "es", "Verde turquesa"         ) ] ) )
        , ( "6017", ( toLinearStr (  76, 145,  65 ), Dict.fromList [ ( "de", "Maigrün" )        , ( "en", "May green"               ), ( "fr", "Vert mai"               ), ( "it", "Verde maggio"           ), ( "es", "Verde mayo"             ) ] ) )
        , ( "6018", ( toLinearStr (  87, 166,  57 ), Dict.fromList [ ( "de", "Gelbgrün" )       , ( "en", "Yellow green"            ), ( "fr", "Vert jaune"             ), ( "it", "Verde giallastro"       ), ( "es", "Verde amarillento"      ) ] ) )
        , ( "6019", ( toLinearStr ( 189, 236, 182 ), Dict.fromList [ ( "de", "Weißgrün" )       , ( "en", "Pastel green"            ), ( "fr", "Vert blanc"             ), ( "it", "Verde biancastro"       ), ( "es", "Verde lanquecino"       ) ] ) )
        , ( "6020", ( toLinearStr (  46,  58,  35 ), Dict.fromList [ ( "de", "Chromoxidgrün" )  , ( "en", "Chrome green"            ), ( "fr", "Vert oxyde chromique"   ), ( "it", "Verde cromo"            ), ( "es", "Verde cromo"            ) ] ) )
        , ( "6021", ( toLinearStr ( 137, 172, 118 ), Dict.fromList [ ( "de", "Blassgrün" )      , ( "en", "Pale green"              ), ( "fr", "Vert pâle"              ), ( "it", "Verde pallido"          ), ( "es", "Verde pálido"           ) ] ) )
        , ( "6022", ( toLinearStr (  37,  34,  27 ), Dict.fromList [ ( "de", "Braunoliv" )      , ( "en", "Olive drab"              ), ( "fr", "Olive brun"             ), ( "it", "Oliva brunastro"        ), ( "es", "Oliva parduzco"         ) ] ) )
        , ( "6024", ( toLinearStr (  48, 132,  70 ), Dict.fromList [ ( "de", "Verkehrsgrün" )   , ( "en", "Traffic green"           ), ( "fr", "Vert signalisation"     ), ( "it", "Verde traffico"         ), ( "es", "Verde tráfico"          ) ] ) )
        , ( "6025", ( toLinearStr (  61, 100,  45 ), Dict.fromList [ ( "de", "Farngrün" )       , ( "en", "Fern green"              ), ( "fr", "Vert fougère"           ), ( "it", "Verde felce"            ), ( "es", "Verde helecho"          ) ] ) )
        , ( "6026", ( toLinearStr (   1,  93,  82 ), Dict.fromList [ ( "de", "Opalgrün" )       , ( "en", "Opal green"              ), ( "fr", "Vert opale"             ), ( "it", "Verde opale"            ), ( "es", "Verde opalo"            ) ] ) )
        , ( "6027", ( toLinearStr ( 132, 195, 190 ), Dict.fromList [ ( "de", "Lichtgrün" )      , ( "en", "Light green"             ), ( "fr", "Vert clair"             ), ( "it", "Verde chiaro"           ), ( "es", "Verde luminoso"         ) ] ) )
        , ( "6028", ( toLinearStr (  44,  85,  69 ), Dict.fromList [ ( "de", "Kieferngrün" )    , ( "en", "Pine green"              ), ( "fr", "Vert pin"               ), ( "it", "Verde pino"             ), ( "es", "Verde pino"             ) ] ) )
        , ( "6029", ( toLinearStr (  32,  96,  61 ), Dict.fromList [ ( "de", "Minzgrün" )       , ( "en", "Mint green"              ), ( "fr", "Vert menthe"            ), ( "it", "Verde menta"            ), ( "es", "Verde menta"            ) ] ) )
        , ( "6032", ( toLinearStr (  49, 127,  67 ), Dict.fromList [ ( "de", "Signalgrün" )     , ( "en", "Signal green"            ), ( "fr", "Vert de sécurité"       ), ( "it", "Verde segnale"          ), ( "es", "Verde señales"          ) ] ) )
        , ( "6033", ( toLinearStr (  73, 126, 118 ), Dict.fromList [ ( "de", "Minttürkis" )     , ( "en", "Mint turquoise"          ), ( "fr", "Turquoise menthe"       ), ( "it", "Turchese menta"         ), ( "es", "Turquesa menta"         ) ] ) )
        , ( "6034", ( toLinearStr ( 127, 181, 181 ), Dict.fromList [ ( "de", "Pastelltürkis" )  , ( "en", "Pastel turquoise"        ), ( "fr", "Turquoise pastel"       ), ( "it", "Turchese pastello"      ), ( "es", "Turquesa pastel"        ) ] ) )
        , ( "6035", ( toLinearStr (  28,  84,  45 ), Dict.fromList [ ( "de", "Perlgrün" )       , ( "en", "Pearl green"             ), ( "fr", "Vert nacré"             ), ( "it", "Verde perlato"          ), ( "es", "Verde perlado"          ) ] ) )
        , ( "6036", ( toLinearStr (  22,  53,  55 ), Dict.fromList [ ( "de", "Perlopalgrün" )   , ( "en", "Pearl opal green"        ), ( "fr", "Vert opal nacré"        ), ( "it", "Verde opalo perlato"    ), ( "es", "Verde ópalo perlado"    ) ] ) )
        , ( "6037", ( toLinearStr (  36, 231,  17 ), Dict.fromList [ ( "de", "Reingrün" )       , ( "en", "Pure green"              ), ( "fr", "Vert pur"               ), ( "it", "Verde puro"             ), ( "es", "Verde puro"             ) ] ) )
        , ( "6038", ( toLinearStr (   0, 247,   0 ), Dict.fromList [ ( "de", "Leuchtgrün" )     , ( "en", "Luminous green"          ), ( "fr", "Vert brillant"          ), ( "it", "Verde brillante"        ), ( "es", "Verde brillante"        ) ] ) )
        , ( "6039", ( toLinearStr ( 179, 197,  63 ), Dict.fromList [ ( "de", "Fasergrün" )      , ( "en", "Fiber green"             ), ( "fr", "Fibre verte"            ), ( "it", "Vibre verte"            ), ( "es", "Fibra verde"            ) ] ) )

        -- Grau
        , ( "7000", ( toLinearStr ( 120, 133, 139 ), Dict.fromList [ ( "de", "Fehgrau" )        , ( "en", "Squirrel grey"           ), ( "fr", "Gris petit-gris"        ), ( "it", "Grigio vaio"            ), ( "es", "Gris ardilla"           ) ] ) )
        , ( "7001", ( toLinearStr ( 138, 149, 151 ), Dict.fromList [ ( "de", "Silbergrau" )     , ( "en", "Silver grey"             ), ( "fr", "Gris argent"            ), ( "it", "Grigio argento"         ), ( "es", "Gris plata"             ) ] ) )
        , ( "7002", ( toLinearStr ( 126, 123,  82 ), Dict.fromList [ ( "de", "Olivgrau" )       , ( "en", "Olive grey"              ), ( "fr", "Gris olive"             ), ( "it", "Grigio olivastro"       ), ( "es", "Gris oliva"             ) ] ) )
        , ( "7003", ( toLinearStr ( 108, 112,  89 ), Dict.fromList [ ( "de", "Moosgrau" )       , ( "en", "Moss grey"               ), ( "fr", "Gris mousse"            ), ( "it", "Grigio muschio"         ), ( "es", "Gris musgo"             ) ] ) )
        , ( "7004", ( toLinearStr ( 150, 153, 146 ), Dict.fromList [ ( "de", "Signalgrau" )     , ( "en", "Signal grey"             ), ( "fr", "Gris de sécurité"       ), ( "it", "Grigio segnale"         ), ( "es", "Gris señales"           ) ] ) )
        , ( "7005", ( toLinearStr ( 100, 107,  99 ), Dict.fromList [ ( "de", "Mausgrau" )       , ( "en", "Mouse grey"              ), ( "fr", "Gris souris"            ), ( "it", "Grigio topo"            ), ( "es", "Gris ratón"             ) ] ) )
        , ( "7006", ( toLinearStr ( 109, 101,  82 ), Dict.fromList [ ( "de", "Beigegrau" )      , ( "en", "Beige grey"              ), ( "fr", "Gris beige"             ), ( "it", "Grigio beige"           ), ( "es", "Gris beige"             ) ] ) )
        , ( "7008", ( toLinearStr ( 106,  95,  49 ), Dict.fromList [ ( "de", "Khakigrau" )      , ( "en", "Khaki grey"              ), ( "fr", "Gris kaki"              ), ( "it", "Grigio kaki"            ), ( "es", "Gris caqui"             ) ] ) )
        , ( "7009", ( toLinearStr (  77,  86,  69 ), Dict.fromList [ ( "de", "Grüngrau" )       , ( "en", "Green grey"              ), ( "fr", "Gris vert"              ), ( "it", "Grigio verdastro"       ), ( "es", "Gris verdoso"           ) ] ) )
        , ( "7010", ( toLinearStr (  76,  81,  74 ), Dict.fromList [ ( "de", "Zeltgrau" )       , ( "en", "Tarpaulin grey"          ), ( "fr", "Gris tente"             ), ( "it", "Grigio tenda"           ), ( "es", "Gris lona"              ) ] ) )
        , ( "7011", ( toLinearStr (  67,  75,  77 ), Dict.fromList [ ( "de", "Eisengrau" )      , ( "en", "Iron grey"               ), ( "fr", "Gris fer"               ), ( "it", "Grigio ferro"           ), ( "es", "Gris hierro"            ) ] ) )
        , ( "7012", ( toLinearStr (  78,  87,  84 ), Dict.fromList [ ( "de", "Basaltgrau" )     , ( "en", "Basalt grey"             ), ( "fr", "Gris basalte"           ), ( "it", "Grigio basalto"         ), ( "es", "Gris basalto"           ) ] ) )
        , ( "7013", ( toLinearStr (  70,  69,  49 ), Dict.fromList [ ( "de", "Braungrau" )      , ( "en", "Brown grey"              ), ( "fr", "Gris brun"              ), ( "it", "Grigio brunastro"       ), ( "es", "Gris parduzco"          ) ] ) )
        , ( "7015", ( toLinearStr (  67,  71,  80 ), Dict.fromList [ ( "de", "Schiefergrau" )   , ( "en", "Slate grey"              ), ( "fr", "Gris ardoise"           ), ( "it", "Grigio ardesia"         ), ( "es", "Gris pizarra"           ) ] ) )
        , ( "7016", ( toLinearStr (  41,  49,  51 ), Dict.fromList [ ( "de", "Anthrazitgrau" )  , ( "en", "Anthracite grey"         ), ( "fr", "Gris anthracite"        ), ( "it", "Grigio antracite"       ), ( "es", "Gris antracita"         ) ] ) )
        , ( "7021", ( toLinearStr (  35,  40,  43 ), Dict.fromList [ ( "de", "Schwarzgrau" )    , ( "en", "Black grey"              ), ( "fr", "Gris noir"              ), ( "it", "Grigio nerastro"        ), ( "es", "Gris negruzco"          ) ] ) )
        , ( "7022", ( toLinearStr (  51,  47,  44 ), Dict.fromList [ ( "de", "Umbragrau" )      , ( "en", "Umbra grey"              ), ( "fr", "Gris terre d'ombre"     ), ( "it", "Grigio ombra"           ), ( "es", "Gris sombra"            ) ] ) )
        , ( "7023", ( toLinearStr ( 104, 108,  94 ), Dict.fromList [ ( "de", "Betongrau" )      , ( "en", "Concrete grey"           ), ( "fr", "Gris béton"             ), ( "it", "Grigio calcestruzzo"    ), ( "es", "Gris hormigón"          ) ] ) )
        , ( "7024", ( toLinearStr (  71,  74,  81 ), Dict.fromList [ ( "de", "Graphitgrau" )    , ( "en", "Graphite grey"           ), ( "fr", "Gris graphite"          ), ( "it", "Grigio grafite"         ), ( "es", "Gris grafito"           ) ] ) )
        , ( "7026", ( toLinearStr (  47,  53,  59 ), Dict.fromList [ ( "de", "Granitgrau" )     , ( "en", "Granite grey"            ), ( "fr", "Gris granit"            ), ( "it", "Grigio granito"         ), ( "es", "Gris granito"           ) ] ) )
        , ( "7030", ( toLinearStr ( 139, 140, 122 ), Dict.fromList [ ( "de", "Steingrau" )      , ( "en", "Stone grey"              ), ( "fr", "Gris pierre"            ), ( "it", "Grigio pietra"          ), ( "es", "Gris piedra"            ) ] ) )
        , ( "7031", ( toLinearStr (  71,  75,  78 ), Dict.fromList [ ( "de", "Blaugrau" )       , ( "en", "Blue grey"               ), ( "fr", "Gris bleu"              ), ( "it", "Grigio bluastro"        ), ( "es", "Gris azulado"           ) ] ) )
        , ( "7032", ( toLinearStr ( 184, 183, 153 ), Dict.fromList [ ( "de", "Kieselgrau" )     , ( "en", "Pebble grey"             ), ( "fr", "Gris silex"             ), ( "it", "Grigio ghiaia"          ), ( "es", "Gris guijarro"          ) ] ) )
        , ( "7033", ( toLinearStr ( 125, 132, 113 ), Dict.fromList [ ( "de", "Zementgrau" )     , ( "en", "Cement grey"             ), ( "fr", "Gris ciment"            ), ( "it", "Grigio cemento"         ), ( "es", "Gris cemento"           ) ] ) )
        , ( "7034", ( toLinearStr ( 143, 139, 102 ), Dict.fromList [ ( "de", "Gelbgrau" )       , ( "en", "Yellow grey"             ), ( "fr", "Gris jaune"             ), ( "it", "Grigio giallastro"      ), ( "es", "Gris amarillento"       ) ] ) )
        , ( "7035", ( toLinearStr ( 215, 215, 215 ), Dict.fromList [ ( "de", "Lichtgrau" )      , ( "en", "Light grey"              ), ( "fr", "Gris clair"             ), ( "it", "Grigio luce"            ), ( "es", "Gris luminoso"          ) ] ) )
        , ( "7036", ( toLinearStr ( 127, 118, 121 ), Dict.fromList [ ( "de", "Platingrau" )     , ( "en", "Platinum grey"           ), ( "fr", "Gris platine"           ), ( "it", "Grigio platino"         ), ( "es", "Gris platino"           ) ] ) )
        , ( "7037", ( toLinearStr ( 125, 127, 120 ), Dict.fromList [ ( "de", "Staubgrau" )      , ( "en", "Dusty grey"              ), ( "fr", "Gris poussière"         ), ( "it", "Grigio polvere"         ), ( "es", "Gris polvo"             ) ] ) )
        , ( "7038", ( toLinearStr ( 195, 195, 195 ), Dict.fromList [ ( "de", "Achatgrau" )      , ( "en", "Agate grey"              ), ( "fr", "Gris agate"             ), ( "it", "Grigio agata"           ), ( "es", "Gris ágata"             ) ] ) )
        , ( "7039", ( toLinearStr ( 108, 105,  96 ), Dict.fromList [ ( "de", "Quarzgrau" )      , ( "en", "Quartz grey"             ), ( "fr", "Gris quartz"            ), ( "it", "Grigio quarzo"          ), ( "es", "Gris cuarzo"            ) ] ) )
        , ( "7040", ( toLinearStr ( 157, 161, 170 ), Dict.fromList [ ( "de", "Fenstergrau" )    , ( "en", "Window grey"             ), ( "fr", "Gris fenêtre"           ), ( "it", "Grigio finestra"        ), ( "es", "Gris ventana"           ) ] ) )
        , ( "7042", ( toLinearStr ( 141, 148, 141 ), Dict.fromList [ ( "de", "Verkehrsgrau A" ) , ( "en", "Traffic grey A"          ), ( "fr", "Gris signalisation A"   ), ( "it", "Grigio traffico A"      ), ( "es", "Gris tráfico A"         ) ] ) )
        , ( "7043", ( toLinearStr (  78,  84,  82 ), Dict.fromList [ ( "de", "Verkehrsgrau B" ) , ( "en", "Traffic grey B"          ), ( "fr", "Gris signalisation B"   ), ( "it", "Grigio traffico B"      ), ( "es", "Gris tráfico B"         ) ] ) )
        , ( "7044", ( toLinearStr ( 202, 196, 176 ), Dict.fromList [ ( "de", "Seidengrau" )     , ( "en", "Silk grey"               ), ( "fr", "Gris soie"              ), ( "it", "Grigio seta"            ), ( "es", "Gris seda"              ) ] ) )
        , ( "7045", ( toLinearStr ( 144, 144, 144 ), Dict.fromList [ ( "de", "Telegrau 1" )     , ( "en", "Telegrey 1"              ), ( "fr", "Telegris 1"             ), ( "it", "Tele grigio 1"          ), ( "es", "Gris tele 1"            ) ] ) )
        , ( "7046", ( toLinearStr ( 130, 137, 143 ), Dict.fromList [ ( "de", "Telegrau 2" )     , ( "en", "Telegrey 3"              ), ( "fr", "Telegris 2"             ), ( "it", "Tele grigio 2"          ), ( "es", "Gris tele 2"            ) ] ) )
        , ( "7047", ( toLinearStr ( 208, 208, 208 ), Dict.fromList [ ( "de", "Telegrau 4" )     , ( "en", "Telegrey 4"              ), ( "fr", "Telegris 4"             ), ( "it", "Tele grigio 4"          ), ( "es", "Gris tele 4"            ) ] ) )
        , ( "7048", ( toLinearStr ( 137, 129, 118 ), Dict.fromList [ ( "de", "Perlmausgrau" )   , ( "en", "Pearl mouse grey"        ), ( "fr", "Gris souris nacré"      ), ( "it", "Grigio topo perlato"    ), ( "es", "Gris musgo perlado"     ) ] ) )

        -- Braun
        , ( "8000", ( toLinearStr ( 130, 108,  52 ), Dict.fromList [ ( "de", "Grünbraun" )      , ( "en", "Green brown"             ), ( "fr", "Brun vert"              ), ( "it", "Marrone verdastro"      ), ( "es", "Pardo verdoso"          ) ] ) )
        , ( "8001", ( toLinearStr ( 149,  95,  32 ), Dict.fromList [ ( "de", "Ockerbraun" )     , ( "en", "Ochre brown"             ), ( "fr", "Brun terre de Sienne"   ), ( "it", "Marrone ocra"           ), ( "es", "Pardo ocre"             ) ] ) )
        , ( "8002", ( toLinearStr ( 108,  59,  42 ), Dict.fromList [ ( "de", "Signalbraun" )    , ( "en", "Signal brown"            ), ( "fr", "Brun de sécurité"       ), ( "it", "Marrone segnale"        ), ( "es", "Marrón señales"         ) ] ) )
        , ( "8003", ( toLinearStr ( 115,  66,  34 ), Dict.fromList [ ( "de", "Lehmbraun" )      , ( "en", "Clay brown"              ), ( "fr", "Brun argile"            ), ( "it", "Marrone fango"          ), ( "es", "Pardo arcilla"          ) ] ) )
        , ( "8004", ( toLinearStr ( 142,  64,  42 ), Dict.fromList [ ( "de", "Kupferbraun" )    , ( "en", "Copper brown"            ), ( "fr", "Brun cuivré"            ), ( "it", "Marrone"                ), ( "es", "Pardo cobre"            ) ] ) )
        , ( "8007", ( toLinearStr (  89,  53,  31 ), Dict.fromList [ ( "de", "Rehbraun" )       , ( "en", "Fawn brown"              ), ( "fr", "Brun fauve"             ), ( "it", "Marrone capriolo"       ), ( "es", "Pardo corzo"            ) ] ) )
        , ( "8008", ( toLinearStr ( 111,  79,  40 ), Dict.fromList [ ( "de", "Olivbraun" )      , ( "en", "Olive brown"             ), ( "fr", "Brun olive"             ), ( "it", "Marrone oliva"          ), ( "es", "Pardo oliva"            ) ] ) )
        , ( "8011", ( toLinearStr (  91,  58,  41 ), Dict.fromList [ ( "de", "Nussbraun" )      , ( "en", "Nut brown"               ), ( "fr", "Brun noisette"          ), ( "it", "Marrone noce"           ), ( "es", "Pardo nuez"             ) ] ) )
        , ( "8012", ( toLinearStr (  89,  35,  33 ), Dict.fromList [ ( "de", "Rotbraun" )       , ( "en", "Red brown"               ), ( "fr", "Brun rouge"             ), ( "it", "Marrone rossiccio"      ), ( "es", "Pardo rojo"             ) ] ) )
        , ( "8014", ( toLinearStr (  56,  44,  30 ), Dict.fromList [ ( "de", "Sepiabraun" )     , ( "en", "Sepia brown"             ), ( "fr", "Brun sépia"             ), ( "it", "Marrone seppia"         ), ( "es", "Sepia"                  ) ] ) )
        , ( "8015", ( toLinearStr (  99,  58,  52 ), Dict.fromList [ ( "de", "Kastanienbraun" ) , ( "en", "Chestnut brown"          ), ( "fr", "Marron"                 ), ( "it", "Marrone castagna"       ), ( "es", "Castaño"                ) ] ) )
        , ( "8016", ( toLinearStr (  76,  47,  39 ), Dict.fromList [ ( "de", "Mahagonibraun" )  , ( "en", "Mahogany brown"          ), ( "fr", "Brun acajou"            ), ( "it", "Marrone mogano"         ), ( "es", "Caoba"                  ) ] ) )
        , ( "8017", ( toLinearStr (  69,  50,  46 ), Dict.fromList [ ( "de", "Schokoladenbraun"), ( "en", "Chocolate brown"         ), ( "fr", "Brun chocolat"          ), ( "it", "Marrone cioccolata"     ), ( "es", "Chocolate"              ) ] ) )
        , ( "8019", ( toLinearStr (  64,  58,  58 ), Dict.fromList [ ( "de", "Graubraun" )      , ( "en", "Grey brown"              ), ( "fr", "Brun gris"              ), ( "it", "Marrone grigiastro"     ), ( "es", "Pardo grisáceo"         ) ] ) )
        , ( "8022", ( toLinearStr (  33,  33,  33 ), Dict.fromList [ ( "de", "Schwarzbraun" )   , ( "en", "Black brown"             ), ( "fr", "Brun noir"              ), ( "it", "Marrone nerastro"       ), ( "es", "Pardo negruzco"         ) ] ) )
        , ( "8023", ( toLinearStr ( 166,  94,  46 ), Dict.fromList [ ( "de", "Orangebraun" )    , ( "en", "Orange brown"            ), ( "fr", "Brun orangé"            ), ( "it", "Marrone arancio"        ), ( "es", "Pardo anaranjado"       ) ] ) )
        , ( "8024", ( toLinearStr ( 121,  85,  61 ), Dict.fromList [ ( "de", "Beigebraun" )     , ( "en", "Beige brown"             ), ( "fr", "Brun beige"             ), ( "it", "Marrone beige"          ), ( "es", "Pardo beige"            ) ] ) )
        , ( "8025", ( toLinearStr ( 117,  92,  72 ), Dict.fromList [ ( "de", "Blassbraun" )     , ( "en", "Pale brown"              ), ( "fr", "Brun pâle"              ), ( "it", "Marrone pallido"        ), ( "es", "Pardo pálido"           ) ] ) )
        , ( "8028", ( toLinearStr (  78,  59,  49 ), Dict.fromList [ ( "de", "Terrabraun" )     , ( "en", "Terra brown"             ), ( "fr", "Brun terre"             ), ( "it", "Marrone terra"          ), ( "es", "Marrón tierra"          ) ] ) )
        , ( "8029", ( toLinearStr ( 118,  60,  40 ), Dict.fromList [ ( "de", "Perlkupfer" )     , ( "en", "Pearl copper"            ), ( "fr", "Cuivre nacré"           ), ( "it", "Rame perlato"           ), ( "es", "Cobre perlado"          ) ] ) )

        -- Schwarz / Weiß
        , ( "9001", ( toLinearStr ( 250, 244, 227 ), Dict.fromList [ ( "de", "Cremeweiß" )      , ( "en", "Cream"                   ), ( "fr", "Blanc crème"            ), ( "it", "Bianco crema"           ), ( "es", "Blanco crema"           ) ] ) )
        , ( "9002", ( toLinearStr ( 231, 235, 218 ), Dict.fromList [ ( "de", "Grauweiß" )       , ( "en", "Grey white"              ), ( "fr", "Blanc gris"             ), ( "it", "Bianco grigiastro"      ), ( "es", "Blanco grisáceo"        ) ] ) )
        , ( "9003", ( toLinearStr ( 244, 244, 244 ), Dict.fromList [ ( "de", "Signalweiß" )     , ( "en", "Signal white"            ), ( "fr", "Blanc de sécurité"      ), ( "it", "Bianco segnale"         ), ( "es", "Blanco señales"         ) ] ) )
        , ( "9004", ( toLinearStr (  40,  40,  40 ), Dict.fromList [ ( "de", "Signalschwarz" )  , ( "en", "Signal black"            ), ( "fr", "Noir de sécurité"       ), ( "it", "Nero segnale"           ), ( "es", "Negro señales"          ) ] ) )
        , ( "9005", ( toLinearStr (  10,  10,  10 ), Dict.fromList [ ( "de", "Tiefschwarz" )    , ( "en", "Jet black"               ), ( "fr", "Noir foncé"             ), ( "it", "Nero intenso"           ), ( "es", "Negro intenso"          ) ] ) )
        , ( "9006", ( toLinearStr ( 165, 165, 165 ), Dict.fromList [ ( "de", "Weißaluminium" )  , ( "en", "White aluminium"         ), ( "fr", "Aluminium blanc"        ), ( "it", "Aluminio brillante"     ), ( "es", "Aluminio blanco"        ) ] ) )
        , ( "9007", ( toLinearStr ( 143, 143, 143 ), Dict.fromList [ ( "de", "Graualuminium" )  , ( "en", "Grey aluminium"          ), ( "fr", "Aluminium gris"         ), ( "it", "Aluminio grigiastro"    ), ( "es", "Aluminio gris"          ) ] ) )
        , ( "9010", ( toLinearStr ( 255, 255, 255 ), Dict.fromList [ ( "de", "Reinweiß" )       , ( "en", "Pure white"              ), ( "fr", "Blanc pur"              ), ( "it", "Bianco puro"            ), ( "es", "Blanco puro"            ) ] ) )
        , ( "9011", ( toLinearStr (  28,  28,  28 ), Dict.fromList [ ( "de", "Graphitschwarz" ) , ( "en", "Graphite black"          ), ( "fr", "Noir graphite"          ), ( "it", "Nero grafite"           ), ( "es", "Negro grafito"          ) ] ) )
        -- Reinraumweiß ist leicht gelblich https://www.chemanager-online.com/news/reine-farbe-fuer-reine-raeume
        , ( "9012", ( toLinearStr ( 255, 255, 246 ), Dict.fromList [ ( "de", "Reinraumweiß" )   , ( "en", "Clean room white"        ), ( "fr", "Salle blanche blanche"  ), ( "it", "Camera bianca bianca"   ), ( "es", "Sala limpia blanca"     ) ] ) )
        , ( "9016", ( toLinearStr ( 246, 246, 246 ), Dict.fromList [ ( "de", "Verkehrsweiß" )   , ( "en", "Traffic white"           ), ( "fr", "Blanc signalisation"    ), ( "it", "Bianco traffico"        ), ( "es", "Blanco tráfico"         ) ] ) )
        , ( "9017", ( toLinearStr (  30,  30,  30 ), Dict.fromList [ ( "de", "Verkehrsschwarz" ), ( "en", "Traffic black"           ), ( "fr", "Noir signalisation"     ), ( "it", "Nero traffico"          ), ( "es", "Negro tráfico"          ) ] ) )
        , ( "9018", ( toLinearStr ( 215, 215, 215 ), Dict.fromList [ ( "de", "Papyrusweiß" )    , ( "en", "Papyrus white"           ), ( "fr", "Blanc papyrus"          ), ( "it", "Bianco papiro"          ), ( "es", "Blanco papiro"          ) ] ) )
        , ( "9020", ( toLinearStr ( 246, 246, 246 ), Dict.fromList [ ( "de", "Seidenmatt-Weiß") , ( "en", "Satin white"             ), ( "fr", "Blanc satiné"           ), ( "it", "Bianco satinato"        ), ( "es", "Blanco satinado"        ) ] ) )
        , ( "9022", ( toLinearStr ( 156, 156, 156 ), Dict.fromList [ ( "de", "Perlhellgrau" )   , ( "en", "Pearl light grey"        ), ( "fr", "Gris clair nacré"       ), ( "it", "Grigio chiaro perlato"  ), ( "es", "Gris claro perlado"     ) ] ) )
        , ( "9023", ( toLinearStr ( 130, 130, 130 ), Dict.fromList [ ( "de", "Perldunkelgrau" ) , ( "en", "Pearl dark grey"         ), ( "fr", "Gris fonçé nacré"       ), ( "it", "Grigio scuro perlato"   ), ( "es", "Gris oscuro perlado"    ) ] ) )

        -- Militär (RAL F9 3-Farben Fleckenanstrich)
        , ( "6031", ( toLinearStr (  72,  87,  70 ), Dict.fromList [ ( "de", "Bronzegrün" )     , ( "en", "Bronze green"            ), ( "fr", "Vert bronzé"            ), ( "it", "Verde bronzo"           ), ( "es", "Verde bronce"           ) ] ) )
        , ( "8027", ( toLinearStr (  84,  70,  58 ), Dict.fromList [ ( "de", "Lederbraun" ) ]) )
        , ( "9021", ( toLinearStr (   1,   5,  14 ), Dict.fromList [ ( "de", "Teerschwarz" ) ]) )

        -- Umtarnfarben der Bundeswehr für Wüsten- und Steppenregionen
        , ( "1039", ( toLinearStr ( 206, 193, 158 ), Dict.fromList [ ( "de", "Sandbeige" ) ]) )
        , ( "1040", ( toLinearStr ( 187, 172, 129 ), Dict.fromList [ ( "de", "Lehmbeige" ) ]) )
        , ( "6040", ( toLinearStr ( 130, 126,  88 ), Dict.fromList [ ( "de", "Helloliv"  ) ]) )
        , ( "7050", ( toLinearStr ( 130, 136, 122 ), Dict.fromList [ ( "de", "Tarngrau"  ) ]) )
        , ( "8031", ( toLinearStr ( 180, 157, 123 ), Dict.fromList [ ( "de", "Sandbraun" ) ]) )

        -- Metallic aus dem COSMOLAC-Programm   https://cosmoslac.de/collections/alle-farben
        , ( "R307", ( toLinearStr ( 120, 119, 124 ), Dict.fromList [ ( "de", "Silber" ), ( "en", "Silver" ) ]) )
        , ( "R308", ( toLinearStr ( 144, 106,  23 ), Dict.fromList [ ( "de", "Gold"   ), ( "en", "Gold"   ) ]) )
        , ( "R309", ( toLinearStr ( 150,  80,  52 ), Dict.fromList [ ( "de", "Kupfer" ), ( "en", "Copper" ) ]) )
        , ( "R310", ( toLinearStr ( 158, 100,  47 ), Dict.fromList [ ( "de", "Bronze" ), ( "en", "Brass"  ) ]) )

        , ( "MIN",  ( toLinearStr (   0,   0,   0 ), Dict.fromList [ ( "de", "#000000" ) ]))
        , ( "MAX",  ( toLinearStr ( 255, 255, 255 ), Dict.fromList [ ( "de", "#FFFFFF" ) ]))
        ]

-- TODO https://de.wikipedia.org/wiki/RAL-Farbe (evtl. RAL F9 (Tarnfarben der Bundeswehr) nachtragen)
-- TODO https://www.militaerlacke.de/fahrzeuglacke/1k-eimergebinde/lshd-zb-und-thw/
-- TODO https://www.ral-farben.de/ral-p2-kunststofffarbmuster#fancybox-cpl_main_ImageGalleryc81e7d4b498ec4dd9877f4f5c273b62ca-1
-- TODO https://www.ral-farben.de/ral-p2-kunststofffarbmuster
-- TODO https://www.visual-graphics.de/de/service/pantone-farbtabelle/


ralLook : String -> String -> Maybe String -> List UsdInput -> String -> Look
ralLook id ral shopId extraInputs suffix=
    case Dict.get ral ralK5Dict of
        Nothing ->
            { newLook | shopId = shopId }

        Just (rgb, descr) ->
            { id= id
            , description= descr
            , material=
                { id= "matRAL_" ++ ral ++ suffix
                , inputs=
                    ( [ UsdInput "diffuseColor" rgb Nothing --(Just ("ral/"++ ral ++".jpg"))
                      --, UsdInput "metallic" "1" Nothing
                      ] ++ extraInputs )
                }
            , thumbnail= Nothing
            , shopId= shopId
            , btnScaleU= 1.0
            , btnScaleV= 1.0
            , meshId= "normal"
            , expandedView= False
            }


patchDescription : Look -> LanguageString -> Look
patchDescription look descr =
    { look | description = descr }


patchInputs : Look -> List UsdInput -> Look
patchInputs look inputs =
    let
        material = look.material
        newMaterial = { material | inputs = inputs }
    in
    { look | material = newMaterial }


dbMattDict : Dict String (String, LanguageString)
dbMattDict=
    Dict.fromList
        [ ( "501",  ( toLinearStr (  93, 119, 121 ), Dict.fromList [ ("de", "DB 501 Matt") ]) )
        , ( "502",  ( toLinearStr (  39,  75,  81 ), Dict.fromList [ ("de", "DB 502 Matt") ]) )
        , ( "701",  ( toLinearStr ( 142, 142, 140 ), Dict.fromList [ ("de", "DB 701 Matt") ]) )
        , ( "702",  ( toLinearStr ( 118, 119, 113 ), Dict.fromList [ ("de", "DB 702 Matt") ]) )
        , ( "703",  ( toLinearStr (  61,  66,  66 ), Dict.fromList [ ("de", "DB 703 Matt") ]) )
        , ( "704",  ( toLinearStr ( 139, 141, 138 ), Dict.fromList [ ("de", "DB 704 Matt") ]) )
        ]


dbFeinDict : Dict String (String, LanguageString)
dbFeinDict=
    Dict.fromList
        [ ( "701",  ( toLinearStr ( 142, 142, 140 ), Dict.fromList [ ("de", "DB 701 Feinstruktur") ]) )
        , ( "702",  ( toLinearStr ( 118, 119, 113 ), Dict.fromList [ ("de", "DB 702 Feinstruktur") ]) )
        , ( "703",  ( toLinearStr (  61,  66,  66 ), Dict.fromList [ ("de", "DB 703 Feinstruktur") ]) )
        ]


dbLook : String -> Bool -> Maybe String -> List UsdInput -> Look
dbLook id fine shopId extraInputs =
    let
        rough = String.fromFloat satinRoughness
    in
    if fine
        then
            case Dict.get id dbMattDict of
                Nothing ->
                    { newLook | shopId = shopId }

                Just (rgb, descr) ->
                    { id= id
                    , description= descr
                    , material=
                        { id= "matDB_" ++ id ++ "_matt"
                        , inputs=
                            ( [ UsdInput "diffuseColor" rgb Nothing
                              , UsdInput "metallic" "1" Nothing
                              , UsdInput "roughness" rough Nothing
                              ] ++ extraInputs
                            )
                        }
                    , thumbnail= Nothing
                    , shopId= shopId
                    , btnScaleU= 1.0
                    , btnScaleV= 1.0
                    , meshId= "normal"
                    , expandedView= False
                    }
        else
            case Dict.get id dbFeinDict of
                Nothing ->
                    { newLook | shopId = shopId }

                Just (rgb, descr) ->
                    { id= id
                    , description= descr
                    , material=
                        { id= "matDB_" ++ id ++ "_fein"
                        , inputs=
                            ( [ UsdInput "diffuseColor" rgb Nothing
                              , UsdInput "metallic" "1" (Just (defaultTx "db/metalness.png" 0))
                              , UsdInput "roughness" rough (Just (defaultTx "db/roughness.png" 0))
                              ] ++ extraInputs
                            )
                        }
                    , thumbnail= Nothing
                    , shopId= shopId
                    , btnScaleU= 1.0
                    , btnScaleV= 1.0
                    , meshId= "normal"
                    , expandedView= False
                    }


pantoneLook : String -> Maybe String -> List UsdInput -> Look
pantoneLook id shopId extraInputs=
    case Dict.get id pantoneDict of
        Nothing ->
            { newLook | shopId = shopId }

        Just (rgb, descr) ->
            { id= id
            , description= descr
            , material=
                { id= "matP_" ++ id
                , inputs=
                    ( [ UsdInput "diffuseColor" rgb Nothing
                        , UsdInput "metallic" "1" Nothing
                        , UsdInput "roughness" "0.5" Nothing
                        ] ++ extraInputs
                    )
                }
            , thumbnail= Nothing
            , shopId= shopId
            , btnScaleU= 1.0
            , btnScaleV= 1.0
            , meshId= "normal"
            , expandedView= False
            }


pantoneDict : Dict String (String, LanguageString)
pantoneDict =
    Dict.fromList
        [ ( "Pantone 100",      ( toLinearStr ( 255, 255, 125 ), Dict.fromList [ ( "en", "Pantone 100"    ) ] ) )
        , ( "Pantone 101",      ( toLinearStr ( 255, 255,  54 ), Dict.fromList [ ( "en", "Pantone 101"    ) ] ) )
        , ( "Pantone 102",      ( toLinearStr ( 255, 255,  13 ), Dict.fromList [ ( "en", "Pantone 102"    ) ] ) )
        , ( "Pantone 103",      ( toLinearStr ( 209, 203,   0 ), Dict.fromList [ ( "en", "Pantone 103"    ) ] ) )
        , ( "Pantone 104",      ( toLinearStr ( 179, 173,   0 ), Dict.fromList [ ( "en", "Pantone 104"    ) ] ) )
        , ( "Pantone 105",      ( toLinearStr ( 128, 124,   0 ), Dict.fromList [ ( "en", "Pantone 105"    ) ] ) )
        , ( "Pantone 106",      ( toLinearStr ( 255, 250,  79 ), Dict.fromList [ ( "en", "Pantone 106"    ) ] ) )
        , ( "Pantone 107",      ( toLinearStr ( 255, 245,  54 ), Dict.fromList [ ( "en", "Pantone 107"    ) ] ) )
        , ( "Pantone 108",      ( toLinearStr ( 255, 240,  13 ), Dict.fromList [ ( "en", "Pantone 108"    ) ] ) )
        , ( "Pantone 109",      ( toLinearStr ( 255, 230,   0 ), Dict.fromList [ ( "en", "Pantone 109"    ) ] ) )
        , ( "Pantone 110",      ( toLinearStr ( 237, 209,   0 ), Dict.fromList [ ( "en", "Pantone 110"    ) ] ) )
        , ( "Pantone 111",      ( toLinearStr ( 186, 166,   0 ), Dict.fromList [ ( "en", "Pantone 111"    ) ] ) )
        , ( "Pantone 112",      ( toLinearStr ( 158, 142,   0 ), Dict.fromList [ ( "en", "Pantone 112"    ) ] ) )
        , ( "Pantone 113",      ( toLinearStr ( 255, 237,  87 ), Dict.fromList [ ( "en", "Pantone 113"    ) ] ) )
        , ( "Pantone 114",      ( toLinearStr ( 255, 235,  69 ), Dict.fromList [ ( "en", "Pantone 114"    ) ] ) )
        , ( "Pantone 115",      ( toLinearStr ( 255, 232,  51 ), Dict.fromList [ ( "en", "Pantone 115"    ) ] ) )
        , ( "Pantone 116",      ( toLinearStr ( 255, 214,   0 ), Dict.fromList [ ( "en", "Pantone 116"    ) ] ) )
        , ( "Pantone 117",      ( toLinearStr ( 217, 178,   0 ), Dict.fromList [ ( "en", "Pantone 117"    ) ] ) )
        , ( "Pantone 118",      ( toLinearStr ( 186, 153,   0 ), Dict.fromList [ ( "en", "Pantone 118"    ) ] ) )
        , ( "Pantone 119",      ( toLinearStr ( 130, 114,   0 ), Dict.fromList [ ( "en", "Pantone 119"    ) ] ) )
        , ( "Pantone 120",      ( toLinearStr ( 255, 232, 107 ), Dict.fromList [ ( "en", "Pantone 120"    ) ] ) )
        , ( "Pantone 1205",     ( toLinearStr ( 255, 242, 176 ), Dict.fromList [ ( "en", "Pantone 1205"   ) ] ) )
        , ( "Pantone 121",      ( toLinearStr ( 255, 227,  79 ), Dict.fromList [ ( "en", "Pantone 121"    ) ] ) )
        , ( "Pantone 1215",     ( toLinearStr ( 255, 232, 140 ), Dict.fromList [ ( "en", "Pantone 1215"   ) ] ) )
        , ( "Pantone 122",      ( toLinearStr ( 255, 212,  51 ), Dict.fromList [ ( "en", "Pantone 122"    ) ] ) )
        , ( "Pantone 1225",     ( toLinearStr ( 255, 212,  97 ), Dict.fromList [ ( "en", "Pantone 1225"   ) ] ) )
        , ( "Pantone 123",      ( toLinearStr ( 255, 194,  15 ), Dict.fromList [ ( "en", "Pantone 123"    ) ] ) )
        , ( "Pantone 1235",     ( toLinearStr ( 255, 181,  23 ), Dict.fromList [ ( "en", "Pantone 1235"   ) ] ) )
        , ( "Pantone 124",      ( toLinearStr ( 209, 151,   0 ), Dict.fromList [ ( "en", "Pantone 124"    ) ] ) )
        , ( "Pantone 1245",     ( toLinearStr ( 240, 173,   0 ), Dict.fromList [ ( "en", "Pantone 1245"   ) ] ) )
        , ( "Pantone 125",      ( toLinearStr ( 189, 140,   0 ), Dict.fromList [ ( "en", "Pantone 125"    ) ] ) )
        , ( "Pantone 1255",     ( toLinearStr ( 168, 123,   0 ), Dict.fromList [ ( "en", "Pantone 1255"   ) ] ) )
        , ( "Pantone 126",      ( toLinearStr ( 161, 120,   0 ), Dict.fromList [ ( "en", "Pantone 126"    ) ] ) )
        , ( "Pantone 1265",     ( toLinearStr ( 125,  91,   0 ), Dict.fromList [ ( "en", "Pantone 1265"   ) ] ) )
        , ( "Pantone 127",      ( toLinearStr ( 255, 237, 128 ), Dict.fromList [ ( "en", "Pantone 127"    ) ] ) )
        , ( "Pantone 128",      ( toLinearStr ( 255, 227,  89 ), Dict.fromList [ ( "en", "Pantone 128"    ) ] ) )
        , ( "Pantone 129",      ( toLinearStr ( 255, 179,   0 ), Dict.fromList [ ( "en", "Pantone 129"    ) ] ) )
        , ( "Pantone 130",      ( toLinearStr ( 255, 232, 107 ), Dict.fromList [ ( "en", "Pantone 130"    ) ] ) )
        , ( "Pantone 131",      ( toLinearStr ( 232, 158,   0 ), Dict.fromList [ ( "en", "Pantone 131"    ) ] ) )
        , ( "Pantone 132",      ( toLinearStr ( 179, 129,   0 ), Dict.fromList [ ( "en", "Pantone 132"    ) ] ) )
        , ( "Pantone 133",      ( toLinearStr ( 112,  90,   0 ), Dict.fromList [ ( "en", "Pantone 133"    ) ] ) )
        , ( "Pantone 134",      ( toLinearStr ( 255, 227, 140 ), Dict.fromList [ ( "en", "Pantone 134"    ) ] ) )
        , ( "Pantone 1345",     ( toLinearStr ( 255, 219, 135 ), Dict.fromList [ ( "en", "Pantone 1345"   ) ] ) )
        , ( "Pantone 135",      ( toLinearStr ( 255, 207, 102 ), Dict.fromList [ ( "en", "Pantone 135"    ) ] ) )
        , ( "Pantone 1355",     ( toLinearStr ( 255, 204, 112 ), Dict.fromList [ ( "en", "Pantone 1355"   ) ] ) )
        , ( "Pantone 136",      ( toLinearStr ( 255, 186,  61 ), Dict.fromList [ ( "en", "Pantone 136"    ) ] ) )
        , ( "Pantone 1365",     ( toLinearStr ( 255, 181,  71 ), Dict.fromList [ ( "en", "Pantone 1365"   ) ] ) )
        , ( "Pantone 137",      ( toLinearStr ( 255, 166,  26 ), Dict.fromList [ ( "en", "Pantone 137"    ) ] ) )
        , ( "Pantone 1375",     ( toLinearStr ( 255, 153,  26 ), Dict.fromList [ ( "en", "Pantone 1375"   ) ] ) )
        , ( "Pantone 138",      ( toLinearStr ( 252, 146,   0 ), Dict.fromList [ ( "en", "Pantone 138"    ) ] ) )
        , ( "Pantone 1385",     ( toLinearStr ( 237, 133,   0 ), Dict.fromList [ ( "en", "Pantone 1385"   ) ] ) )
        , ( "Pantone 139",      ( toLinearStr ( 196, 124,   0 ), Dict.fromList [ ( "en", "Pantone 139"    ) ] ) )
        , ( "Pantone 1395",     ( toLinearStr ( 161,  95,   0 ), Dict.fromList [ ( "en", "Pantone 1395"   ) ] ) )
        , ( "Pantone 140",      ( toLinearStr ( 117,  86,   0 ), Dict.fromList [ ( "en", "Pantone 140"    ) ] ) )
        , ( "Pantone 1405",     ( toLinearStr (  94,  60,   0 ), Dict.fromList [ ( "en", "Pantone 1405"   ) ] ) )
        , ( "Pantone 141",      ( toLinearStr ( 255, 207, 125 ), Dict.fromList [ ( "en", "Pantone 141"    ) ] ) )
        , ( "Pantone 142",      ( toLinearStr ( 255, 184,  61 ), Dict.fromList [ ( "en", "Pantone 142"    ) ] ) )
        , ( "Pantone 143",      ( toLinearStr ( 255, 166,  38 ), Dict.fromList [ ( "en", "Pantone 143"    ) ] ) )
        , ( "Pantone 144",      ( toLinearStr ( 255, 133,   0 ), Dict.fromList [ ( "en", "Pantone 144"    ) ] ) )
        , ( "Pantone 145",      ( toLinearStr ( 235, 124,   0 ), Dict.fromList [ ( "en", "Pantone 145"    ) ] ) )
        , ( "Pantone 146",      ( toLinearStr ( 171,  97,   0 ), Dict.fromList [ ( "en", "Pantone 146"    ) ] ) )
        , ( "Pantone 147",      ( toLinearStr ( 112,  81,   0 ), Dict.fromList [ ( "en", "Pantone 147"    ) ] ) )
        , ( "Pantone 148",      ( toLinearStr ( 255, 214, 161 ), Dict.fromList [ ( "en", "Pantone 148"    ) ] ) )
        , ( "Pantone 1485",     ( toLinearStr ( 255, 186, 117 ), Dict.fromList [ ( "en", "Pantone 1485"   ) ] ) )
        , ( "Pantone 149",      ( toLinearStr ( 255, 196, 135 ), Dict.fromList [ ( "en", "Pantone 149"    ) ] ) )
        , ( "Pantone 1495",     ( toLinearStr ( 255, 171,  84 ), Dict.fromList [ ( "en", "Pantone 1495"   ) ] ) )
        , ( "Pantone 150",      ( toLinearStr ( 255, 166,  77 ), Dict.fromList [ ( "en", "Pantone 150"    ) ] ) )
        , ( "Pantone 1505",     ( toLinearStr ( 255, 148,  59 ), Dict.fromList [ ( "en", "Pantone 1505"   ) ] ) )
        , ( "Pantone 151",      ( toLinearStr ( 255, 133,  13 ), Dict.fromList [ ( "en", "Pantone 151"    ) ] ) )
        , ( "Pantone 152",      ( toLinearStr ( 252, 124,   0 ), Dict.fromList [ ( "en", "Pantone 152"    ) ] ) )
        , ( "Pantone 1525",     ( toLinearStr ( 230,  96,   0 ), Dict.fromList [ ( "en", "Pantone 1525"   ) ] ) )
        , ( "Pantone 153",      ( toLinearStr ( 209, 113,   0 ), Dict.fromList [ ( "en", "Pantone 153"    ) ] ) )
        , ( "Pantone 1535",     ( toLinearStr ( 158,  74,   0 ), Dict.fromList [ ( "en", "Pantone 1535"   ) ] ) )
        , ( "Pantone 154",      ( toLinearStr ( 168,  91,   0 ), Dict.fromList [ ( "en", "Pantone 154"    ) ] ) )
        , ( "Pantone 1545",     ( toLinearStr (  71,  34,   0 ), Dict.fromList [ ( "en", "Pantone 1545"   ) ] ) )
        , ( "Pantone 155",      ( toLinearStr ( 255, 224, 184 ), Dict.fromList [ ( "en", "Pantone 155"    ) ] ) )
        , ( "Pantone 1555",     ( toLinearStr ( 255, 199, 168 ), Dict.fromList [ ( "en", "Pantone 1555"   ) ] ) )
        , ( "Pantone 156",      ( toLinearStr ( 255, 199, 148 ), Dict.fromList [ ( "en", "Pantone 156"    ) ] ) )
        , ( "Pantone 1565",     ( toLinearStr ( 255, 168, 130 ), Dict.fromList [ ( "en", "Pantone 1565"   ) ] ) )
        , ( "Pantone 157",      ( toLinearStr ( 255, 145,  77 ), Dict.fromList [ ( "en", "Pantone 157"    ) ] ) )
        , ( "Pantone 1575",     ( toLinearStr ( 255, 140,  71 ), Dict.fromList [ ( "en", "Pantone 1575"   ) ] ) )
        , ( "Pantone 158",      ( toLinearStr ( 255,  99,   8 ), Dict.fromList [ ( "en", "Pantone 158"    ) ] ) )
        , ( "Pantone 1585",     ( toLinearStr ( 255, 112,  26 ), Dict.fromList [ ( "en", "Pantone 1585"   ) ] ) )
        , ( "Pantone 159",      ( toLinearStr ( 237,  81,   0 ), Dict.fromList [ ( "en", "Pantone 159"    ) ] ) )
        , ( "Pantone 1595",     ( toLinearStr ( 242,  99,   0 ), Dict.fromList [ ( "en", "Pantone 1595"   ) ] ) )
        , ( "Pantone 160",      ( toLinearStr ( 173,  66,   0 ), Dict.fromList [ ( "en", "Pantone 160"    ) ] ) )
        , ( "Pantone 1605",     ( toLinearStr ( 179,  79,   0 ), Dict.fromList [ ( "en", "Pantone 1605"   ) ] ) )
        , ( "Pantone 161",      ( toLinearStr (  92,  44,   0 ), Dict.fromList [ ( "en", "Pantone 161"    ) ] ) )
        , ( "Pantone 1615",     ( toLinearStr ( 145,  64,   0 ), Dict.fromList [ ( "en", "Pantone 1615"   ) ] ) )
        , ( "Pantone 162",      ( toLinearStr ( 255, 217, 199 ), Dict.fromList [ ( "en", "Pantone 162"    ) ] ) )
        , ( "Pantone 1625",     ( toLinearStr ( 255, 176, 161 ), Dict.fromList [ ( "en", "Pantone 1625"   ) ] ) )
        , ( "Pantone 163",      ( toLinearStr ( 255, 176, 143 ), Dict.fromList [ ( "en", "Pantone 163"    ) ] ) )
        , ( "Pantone 1635",     ( toLinearStr ( 255, 156, 133 ), Dict.fromList [ ( "en", "Pantone 1635"   ) ] ) )

--
        , ( "cool grey 1",      ( toLinearStr ( 240, 240, 240 ), Dict.fromList [ ( "en", "cool grey 1"    ) ] ) )
        , ( "cool grey 2",      ( toLinearStr ( 230, 230, 230 ), Dict.fromList [ ( "en", "cool grey 2"    ) ] ) )
        , ( "cool grey 3",      ( toLinearStr ( 212, 212, 212 ), Dict.fromList [ ( "en", "cool grey 3"    ) ] ) )
        , ( "cool grey 4",      ( toLinearStr ( 194, 194, 194 ), Dict.fromList [ ( "en", "cool grey 4"    ) ] ) )
        , ( "cool grey 5",      ( toLinearStr ( 181, 181, 181 ), Dict.fromList [ ( "en", "cool grey 5"    ) ] ) )
        , ( "cool grey 6",      ( toLinearStr ( 176, 176, 176 ), Dict.fromList [ ( "en", "cool grey 6"    ) ] ) )
        , ( "cool grey 7",      ( toLinearStr ( 161, 161, 161 ), Dict.fromList [ ( "en", "cool grey 7"    ) ] ) )
        , ( "cool grey 8",      ( toLinearStr ( 145, 144, 145 ), Dict.fromList [ ( "en", "cool grey 8"    ) ] ) )
        , ( "cool grey 9",      ( toLinearStr ( 125, 124, 125 ), Dict.fromList [ ( "en", "cool grey 9"    ) ] ) )
        , ( "cool grey 10",     ( toLinearStr ( 102, 100, 102 ), Dict.fromList [ ( "en", "cool grey 10"   ) ] ) )
        , ( "cool grey 11",     ( toLinearStr (  82,  80,  82 ), Dict.fromList [ ( "en", "cool grey 11"   ) ] ) )

        , ( "warm grey 1",      ( toLinearStr ( 240, 235, 233 ), Dict.fromList [ ( "en", "warm grey 1"    ) ] ) )
        , ( "warm grey 2",      ( toLinearStr ( 232, 227, 220 ), Dict.fromList [ ( "en", "warm grey 2"    ) ] ) )
        , ( "warm grey 3",      ( toLinearStr ( 212, 203, 195 ), Dict.fromList [ ( "en", "warm grey 3"    ) ] ) )
        , ( "warm grey 4",      ( toLinearStr ( 194, 186, 176 ), Dict.fromList [ ( "en", "warm grey 4"    ) ] ) )
        , ( "warm grey 5",      ( toLinearStr ( 181, 172, 163 ), Dict.fromList [ ( "en", "warm grey 5"    ) ] ) )
        , ( "warm grey 6",      ( toLinearStr ( 176, 165, 155 ), Dict.fromList [ ( "en", "warm grey 6"    ) ] ) )
        , ( "warm grey 7",      ( toLinearStr ( 158, 145, 136 ), Dict.fromList [ ( "en", "warm grey 7"    ) ] ) )
        , ( "warm grey 8",      ( toLinearStr ( 145, 132, 122 ), Dict.fromList [ ( "en", "warm grey 8"    ) ] ) )
        , ( "warm grey 9",      ( toLinearStr ( 135, 120, 108 ), Dict.fromList [ ( "en", "warm grey 9"    ) ] ) )
        , ( "warm grey 10",     ( toLinearStr ( 115,  99,  83 ), Dict.fromList [ ( "en", "warm grey 10"   ) ] ) )
        , ( "warm grey 11",     ( toLinearStr (  97,  80,  64 ), Dict.fromList [ ( "en", "warm grey 11"   ) ] ) )
        ]

{-
Pantone 164
0, 46, 73, 0
255, 138, 69
#FF8A45
Pantone 1645
0, 49, 66, 0
255, 130, 87
#FF8257
Pantone 165
0, 59, 96, 0
255, 105, 10
#FF690A
Pantone 1655
0, 63, 91, 0
255, 94, 23
#FF5E17
Pantone 166
0, 64, 100, 0
255, 92, 0
#FF5C00
Pantone 1665
0, 68, 100, 0
255, 82, 0
#FF5200
Pantone 167
0, 60, 100, 17
212, 85, 0
#D45500
Pantone 1675
0, 67, 100, 28
184, 61, 0
#B83D00
Pantone 168
0, 57, 100, 59
105, 45, 0
#692D00
Pantone 1685
0, 68, 100, 44
143, 46, 0
#8F2E00
Pantone 169
0, 20, 20, 0
255, 204, 204
#FFCCCC
Pantone 170
0, 40, 44, 0
255, 153, 143
#FF998F
Pantone 171
0, 53, 68, 0
255, 120, 82
#FF7852
Pantone 172
0, 66, 88, 0
255, 87, 31
#FF571F
Pantone 173
0, 69, 100, 4
245, 76, 0
#F54C00
Pantone 174
0, 70, 100, 36
163, 49, 0
#A33100
Pantone 175
0, 65, 100, 60
102, 36, 0
#662400
Pantone 176
0, 25, 18, 0
255, 191, 209
#FFBFD1
Pantone 1765
0, 38, 21, 0
255, 158, 201
#FF9EC9
Pantone 1767
0, 27, 12, 0
255, 186, 224
#FFBAE0
Pantone 177
0, 45, 40, 0
255, 140, 153
#FF8C99
Pantone 1775
0, 47, 29, 0
255, 135, 181
#FF87B5
Pantone 1777
0, 58, 36, 0
255, 107, 163
#FF6BA3
Pantone 178
0, 59, 56, 0
255, 105, 112
#FF6970
Pantone 1785
0, 67, 50, 0
255, 84, 128
#FF5480
Pantone 1787
0, 76, 60, 0
255, 61, 102
#FF3D66
Pantone 1788
0, 84, 88, 0
255, 41, 31
#FF291F
Pantone 179
0, 79, 100, 0
255, 54, 0
#FF3600
Pantone 1795
0, 94, 100, 0
255, 15, 0
#FF0F00
Pantone 1797
0, 100, 99, 4
245, 0, 2
#F50002
Pantone 180
0, 79, 100, 11
227, 48, 0
#E33000
Pantone 1805
0, 91, 100, 23
196, 18, 0
#C41200
Pantone 1807
0, 100, 96, 28
184, 0, 7
#B80007
Pantone 181
0, 74, 100, 47
135, 35, 0
#872300
Pantone 1815
0, 90, 100, 51
125, 12, 0
#7D0C00
Pantone 1817
0, 90, 100, 66
87, 9, 0
#570900
Pantone 182
0, 26, 10, 0
255, 189, 230
#FFBDE6
Pantone 183
0, 46, 21, 0
255, 138, 201
#FF8AC9
Pantone 184
0, 68, 41, 0
255, 82, 150
#FF5296
Pantone 185
0, 91, 76, 0
255, 23, 61
#FF173D
Pantone 186
0, 100, 81, 4
245, 0, 47
#F5002F
Pantone 187
0, 100, 79, 20
204, 0, 43
#CC002B
Pantone 188
0, 97, 100, 50
128, 4, 0
#800400
Pantone 189
0, 37, 10, 0
255, 161, 230
#FFA1E6
Pantone 1895
0, 28, 7, 0
255, 184, 237
#FFB8ED
Pantone 190
0, 55, 22, 0
255, 115, 199
#FF73C7
Pantone 1905
0, 41, 9, 0
255, 150, 232
#FF96E8
Pantone 191
0, 76, 38, 0
255, 61, 158
#FF3D9E
Pantone 1915
0, 71, 20, 0
255, 74, 204
#FF4ACC
Pantone 192
0, 100, 68, 0
255, 0, 82
#FF0052
Pantone 1925
0, 100, 55, 0
255, 0, 115
#FF0073
Pantone 193
0, 100, 66, 13
222, 0, 75
#DE004B
Pantone 1935
0, 100, 57, 5
242, 0, 104
#F20068
Pantone 194
0, 100, 64, 33
171, 0, 62
#AB003E
Pantone 1945
0, 100, 56, 19
207, 0, 91
#CF005B
Pantone 195
0, 100, 60, 55
115, 0, 46
#73002E
Pantone 1955
0, 100, 60, 37
161, 0, 64
#A10040
Pantone 196
0, 25, 4, 0
255, 191, 245
#FFBFF5
Pantone 197
0, 45, 10, 0
255, 140, 230
#FF8CE6
Pantone 198
0, 78, 33, 0
255, 56, 171
#FF38AB
Pantone 199
0, 100, 62, 0
255, 0, 97
#FF0061
Pantone 200
0, 100, 63, 12
224, 0, 83
#E00053
Pantone 201
0, 100, 63, 29
181, 0, 67
#B50043
Pantone 202
0, 100, 61, 43
145, 0, 57
#910039
Pantone 203
0, 34, 3, 0
255, 168, 247
#FFA8F7
Pantone 204
0, 58, 3, 0
255, 107, 247
#FF6BF7
Pantone 205
0, 84, 9, 0
255, 41, 232
#FF29E8
Pantone 206
0, 100, 38, 3
247, 0, 153
#F70099
Pantone 207
0, 100, 43, 19
207, 0, 118
#CF0076
Pantone 208
0, 100, 36, 37
161, 0, 103
#A10067
Pantone 209
0, 100, 34, 53
120, 0, 79
#78004F
Pantone 210
0, 39, 6, 0
255, 156, 240
#FF9CF0
Pantone 211
0, 55, 8, 0
255, 115, 235
#FF73EB
Pantone 212
0, 72, 11, 0
255, 71, 227
#FF47E3
Pantone 213
0, 95, 27, 0
255, 13, 186
#FF0DBA
Pantone 214
0, 100, 34, 8
235, 0, 155
#EB009B
Pantone 215
0, 100, 35, 27
186, 0, 121
#BA0079
Pantone 216
0, 95, 40, 49
130, 7, 78
#82074E
Pantone 217
0, 28, 0, 0
255, 184, 255
#FFB8FF
Pantone 218
2, 61, 0, 0
250, 99, 255
#FA63FF
Pantone 219
1, 88, 0, 0
252, 31, 255
#FC1FFF
Pantone 220
0, 100, 13, 17
212, 0, 184
#D400B8
Pantone 221
0, 100, 15, 30
179, 0, 152
#B30098
Pantone 222
0, 100, 10, 59
105, 0, 94
#69005E
Pantone 223
0, 46, 0, 0
255, 138, 255
#FF8AFF
Pantone 224
1, 63, 0, 0
252, 94, 255
#FC5EFF
Pantone 225
1, 83, 0, 0
252, 43, 255
#FC2BFF
Pantone 226
0, 100, 0, 0
255, 0, 255
#FF00FF
Pantone 227
0, 100, 7, 19
207, 0, 192
#CF00C0
Pantone 228
0, 100, 4, 41
150, 0, 144
#960090
Pantone 229
0, 100, 15, 60
102, 0, 87
#660057
Pantone 230
0, 34, 0, 0
255, 168, 255
#FFA8FF
Pantone 231
1, 52, 0, 0
252, 122, 255
#FC7AFF
Pantone 232
3, 67, 0, 0
247, 84, 255
#F754FF
Pantone 233
11, 100, 0, 0
227, 0, 255
#E300FF
Pantone 234
6, 100, 0, 26
177, 0, 189
#B100BD
Pantone 235
5, 100, 0, 40
145, 0, 153
#910099
Pantone 236
1, 30, 0, 0
252, 179, 255
#FCB3FF
Pantone 2365
2, 27, 0, 0
250, 186, 255
#FABAFF
Pantone 237
3, 49, 0, 0
247, 130, 255
#F782FF
Pantone 2375
10, 57, 0, 0
230, 110, 255
#E66EFF
Pantone 238
6, 63, 0, 0
240, 94, 255
#F05EFF
Pantone 2385
19, 79, 0, 0
207, 54, 255
#CF36FF
Pantone 239
11, 79, 0, 0
227, 54, 255
#E336FF
Pantone 2395
27, 95, 0, 0
186, 13, 255
#BA0DFF
Pantone 240
18, 94, 0, 0
209, 15, 255
#D10FFF
Pantone 2405
34, 100, 0, 0
168, 0, 255
#A800FF
Pantone 241
27, 100, 0, 2
182, 0, 250
#B600FA
Pantone 2415
33, 100, 0, 8
157, 0, 235
#9D00EB
Pantone 242
10, 100, 0, 49
117, 0, 130
#750082
Pantone 2425
37, 100, 0, 26
119, 0, 189
#7700BD
Pantone 243
5, 29, 0, 0
242, 181, 255
#F2B5FF
Pantone 244
9, 38, 0, 0
232, 158, 255
#E89EFF
Pantone 245
14, 53, 0, 0
219, 120, 255
#DB78FF
Pantone 246
29, 90, 0, 0
181, 26, 255
#B51AFF
Pantone 247
36, 100, 0, 0
163, 0, 255
#A300FF
Pantone 248
40, 100, 0, 2
150, 0, 250
#9600FA
Pantone 249
40, 100, 0, 28
110, 0, 184
#6E00B8
Pantone 250
5, 18, 0, 0
242, 209, 255
#F2D1FF
Pantone 251
13, 39, 0, 0
222, 156, 255
#DE9CFF
Pantone 252
24, 56, 0, 0
194, 112, 255
#C270FF
Pantone 253
43, 95, 0, 0
145, 13, 255
#910DFF
Pantone 254
50, 100, 0, 0
128, 0, 255
#8000FF
Pantone 255
51, 100, 0, 25
94, 0, 191
#5E00BF
Pantone 256
7, 20, 0, 0
237, 204, 255
#EDCCFF
Pantone 2562
19, 35, 0, 0
207, 166, 255
#CFA6FF
Pantone 2563
22, 33, 0, 0
199, 171, 255
#C7ABFF
Pantone 2567
29, 36, 0, 0
181, 163, 255
#B5A3FF
Pantone 257
14, 34, 0, 0
219, 168, 255
#DBA8FF
Pantone 2572
30, 47, 0, 0
179, 135, 255
#B387FF
Pantone 2573
30, 43, 0, 0
179, 145, 255
#B391FF
Pantone 2577
40, 45, 0, 0
153, 140, 255
#998CFF
Pantone 258
43, 76, 0, 0
145, 61, 255
#913DFF
Pantone 2582
46, 72, 0, 0
138, 71, 255
#8A47FF
Pantone 2583
46, 63, 0, 0
138, 94, 255
#8A5EFF
Pantone 2587
59, 66, 0, 0
105, 87, 255
#6957FF
Pantone 259
56, 100, 0, 15
95, 0, 217
#5F00D9
Pantone 2592
60, 90, 0, 0
102, 26, 255
#661AFF
Pantone 2593
61, 89, 0, 0
99, 28, 255
#631CFF
Pantone 2597
85, 100, 0, 0
38, 0, 255
#2600FF
Pantone 260
52, 100, 0, 26
91, 0, 189
#5B00BD
Pantone 2602
63, 100, 0, 3
92, 0, 247
#5C00F7
Pantone 2603
69, 100, 0, 2
77, 0, 250
#4D00FA
Pantone 2607
81, 100, 0, 7
45, 0, 237
#2D00ED
Pantone 261
48, 100, 0, 40
80, 0, 153
#500099
Pantone 2612
64, 100, 0, 14
79, 0, 219
#4F00DB
Pantone 2613
63, 100, 0, 15
80, 0, 217
#5000D9
Pantone 2617
79, 100, 0, 15
46, 0, 217
#2E00D9
Pantone 262
45, 100, 0, 55
63, 0, 115
#3F0073
Pantone 2622
58, 100, 0, 44
60, 0, 143
#3C008F
Pantone 2623
59, 100, 0, 32
71, 0, 173
#4700AD
Pantone 2627
77, 100, 0, 31
40, 0, 176
#2800B0
Pantone 263
10, 14, 0, 0
230, 219, 255
#E6DBFF
Pantone 2635
28, 27, 0, 0
184, 186, 255
#B8BAFF
Pantone 264
26, 28, 0, 0
189, 184, 255
#BDB8FF
Pantone 2645
40, 36, 0, 0
153, 163, 255
#99A3FF
Pantone 265
54, 56, 0, 0
117, 112, 255
#7570FF
Pantone 2655
54, 49, 0, 0
117, 130, 255
#7582FF
Pantone 266
79, 90, 0, 0
54, 26, 255
#361AFF
Pantone 2665
62, 60, 0, 0
97, 102, 255
#6166FF
Pantone 267
89, 100, 0, 0
28, 0, 255
#1C00FF
Pantone 268
82, 100, 0, 12
40, 0, 224
#2800E0
Pantone 2685
96, 100, 0, 10
9, 0, 230
#0900E6
Pantone 269
78, 100, 0, 33
38, 0, 171
#2600AB
Pantone 2695
91, 100, 0, 49
12, 0, 130
#0C0082
Pantone 270
31, 27, 0, 0
176, 186, 255
#B0BAFF
Pantone 2705
40, 30, 0, 0
153, 179, 255
#99B3FF
Pantone 2706
19, 9, 0, 0
207, 232, 255
#CFE8FF
Pantone 2707
17, 6, 0, 0
212, 240, 255
#D4F0FF
Pantone 2708
26, 10, 0, 0
189, 230, 255
#BDE6FF
Pantone 271
43, 37, 0, 0
145, 161, 255
#91A1FF
Pantone 2715
57, 45, 0, 0
110, 140, 255
#6E8CFF
Pantone 2716
45, 29, 0, 0
140, 181, 255
#8CB5FF
Pantone 2717
29, 12, 0, 0
181, 224, 255
#B5E0FF
Pantone 2718
67, 41, 0, 0
84, 150, 255
#5496FF
Pantone 272
58, 48, 0, 0
107, 133, 255
#6B85FF
Pantone 2725
77, 68, 0, 0
59, 82, 255
#3B52FF
Pantone 2726
79, 66, 0, 0
54, 87, 255
#3657FF
Pantone 2727
71, 42, 0, 0
74, 148, 255
#4A94FF
Pantone 2728
96, 69, 0, 0
10, 79, 255
#0A4FFF
Pantone 273
100, 96, 0, 8
0, 9, 235
#0009EB
Pantone 2735
100, 95, 0, 0
0, 13, 255
#000DFF
Pantone 2736
100, 91, 0, 0
0, 23, 255
#0017FF
Pantone 2738
100, 87, 0, 2
0, 32, 250
#0020FA
Pantone 274
100, 100, 0, 28
0, 0, 184
#0000B8
Pantone 2745
100, 95, 0, 15
0, 11, 217
#000BD9
Pantone 2746
100, 92, 0, 10
0, 18, 230
#0012E6
Pantone 2747
100, 86, 0, 15
0, 30, 217
#001ED9
Pantone 2748
100, 88, 0, 15
0, 26, 217
#001AD9
Pantone 275
98, 100, 0, 43
3, 0, 145
#030091
Pantone 2755
100, 97, 0, 30
0, 5, 179
#0005B3
Pantone 2756
100, 94, 0, 29
0, 11, 181
#000BB5
Pantone 2757
100, 82, 0, 30
0, 32, 179
#0020B3
Pantone 2758
100, 80, 0, 26
0, 38, 189
#0026BD
Pantone 276
98, 100, 0, 55
2, 0, 115
#020073
Pantone 2765
100, 97, 0, 45
0, 4, 140
#00048C
Pantone 2766
100, 94, 0, 47
0, 8, 135
#000887
Pantone 2767
100, 78, 0, 54
0, 26, 117
#001A75
Pantone 2768
100, 78, 0, 44
0, 31, 143
#001F8F
Pantone 277
27, 7, 0, 0
186, 237, 255
#BAEDFF
Pantone 278
39, 14, 0, 0
156, 219, 255
#9CDBFF
Pantone 279
68, 34, 0, 0
82, 168, 255
#52A8FF
Pantone 280
100, 72, 0, 18
0, 59, 209
#003BD1
Pantone 281
100, 72, 0, 32
0, 49, 173
#0031AD
Pantone 282
100, 68, 0, 54
0, 38, 117
#002675
Pantone 283
35, 9, 0, 0
166, 232, 255
#A6E8FF
Pantone 284
55, 19, 0, 0
115, 207, 255
#73CFFF
Pantone 285
89, 43, 0, 0
28, 145, 255
#1C91FF
Pantone 286
100, 66, 0, 2
0, 85, 250
#0055FA
Pantone 287
100, 68, 0, 12
0, 72, 224
#0048E0
Pantone 288
100, 67, 0, 23
0, 65, 196
#0041C4
Pantone 289
100, 66, 0, 58
0, 36, 107
#00246B
Pantone 290
25, 2, 0, 0
191, 250, 255
#BFFAFF
Pantone 291
33, 3, 0, 0
171, 247, 255
#ABF7FF
Pantone 2915
59, 7, 0, 0
105, 237, 255
#69EDFF
Pantone 292
49, 11, 0, 0
130, 227, 255
#82E3FF
Pantone 2925
85, 24, 0, 0
38, 194, 255
#26C2FF
Pantone 293
100, 57, 0, 2
0, 107, 250
#006BFA
Pantone 2935
100, 46, 0, 0
0, 138, 255
#008AFF
Pantone 294
100, 58, 0, 21
0, 85, 201
#0055C9
Pantone 2945
100, 45, 0, 14
0, 121, 219
#0079DB
Pantone 295
100, 57, 0, 37
0, 69, 161
#0045A1
Pantone 2955
100, 45, 0, 37
0, 88, 161
#0058A1
Pantone 296
100, 46, 0, 70
0, 41, 77
#00294D
Pantone 2965
100, 38, 0, 64
0, 57, 92
#00395C
Pantone 297
49, 1, 0, 0
130, 252, 255
#82FCFF
Pantone 2975
30, 0, 5, 0
179, 255, 242
#B3FFF2
Pantone 298
69, 7, 0, 0
79, 237, 255
#4FEDFF
Pantone 2985
59, 0, 6, 0
105, 255, 240
#69FFF0
Pantone 299
85, 19, 0, 0
38, 207, 255
#26CFFF
Pantone 2995
90, 11, 0, 0
26, 227, 255
#1AE3FF
Pantone 300
100, 44, 0, 0
0, 143, 255
#008FFF
Pantone 3005
100, 36, 0, 2
0, 160, 250
#00A0FA
Pantone 301
100, 45, 0, 18
0, 115, 209
#0073D1
Pantone 3015
100, 33, 0, 20
0, 137, 204
#0089CC
Pantone 302
100, 25, 0, 50
0, 96, 128
#006080
Pantone 3025
100, 17, 0, 51
0, 104, 125
#00687D
Pantone 303
100, 11, 0, 74
0, 59, 66
#003B42
Pantone 3035
100, 0, 5, 72
0, 71, 68
#004744
Pantone 304
30, 0, 8, 0
179, 255, 235
#B3FFEB
Pantone 305
51, 0, 9, 0
125, 255, 232
#7DFFE8
Pantone 306
75, 0, 7, 0
64, 255, 237
#40FFED
Pantone 307
100, 16, 0, 27
0, 156, 186
#009CBA
Pantone 308
100, 5, 0, 47
0, 128, 135
#008087
Pantone 309
100, 0, 9, 72
0, 71, 65
#004741
Pantone 310
43, 0, 10, 0
145, 255, 230
#91FFE6
Pantone 3105
43, 0, 12, 0
145, 255, 224
#91FFE0
Pantone 311
63, 0, 12, 0
94, 255, 224
#5EFFE0
Pantone 3115
63, 0, 18, 0
94, 255, 209
#5EFFD1
Pantone 312
96, 0, 11, 0
10, 255, 227
#0AFFE3
Pantone 3125
83, 0, 21, 0
43, 255, 201
#2BFFC9
Pantone 313
100, 0, 8, 13
0, 222, 204
#00DECC
Pantone 3135
100, 0, 16, 9
0, 232, 195
#00E8C3
Pantone 314
100, 0, 9, 30
0, 179, 162
#00B3A2
Pantone 3145
100, 0, 19, 23
0, 196, 159
#00C49F
Pantone 315
100, 0, 12, 43
0, 145, 128
#009180
Pantone 3155
100, 0, 24, 38
0, 158, 120
#009E78
Pantone 316
100, 0, 27, 68
0, 82, 60
#00523C
Pantone 3165
100, 0, 28, 65
0, 89, 64
#005940
Pantone 317
18, 0, 8, 0
209, 255, 235
#D1FFEB
Pantone 318
38, 0, 15, 0
158, 255, 217
#9EFFD9
Pantone 319
52, 0, 19, 0
122, 255, 207
#7AFFCF
Pantone 320
100, 0, 31, 7
0, 237, 164
#00EDA4
Pantone 321
100, 0, 31, 23
0, 196, 135
#00C487
Pantone 322
100, 0, 33, 35
0, 166, 111
#00A66F
Pantone 323
100, 0, 38, 47
0, 135, 84
#008754
Pantone 324
28, 0, 12, 0
184, 255, 224
#B8FFE0
Pantone 3242
37, 0, 18, 0
161, 255, 209
#A1FFD1
Pantone 3245
34, 0, 19, 0
168, 255, 207
#A8FFCF
Pantone 3248
43, 0, 24, 0
145, 255, 194
#91FFC2
Pantone 325
56, 0, 26, 0
112, 255, 189
#70FFBD
Pantone 3252
47, 0, 24, 0
135, 255, 194
#87FFC2
Pantone 3255
49, 0, 28, 0
130, 255, 184
#82FFB8
Pantone 3258
59, 0, 33, 0
105, 255, 171
#69FFAB
Pantone 326
87, 0, 38, 0
33, 255, 158
#21FF9E
Pantone 3262
71, 0, 33, 0
74, 255, 171
#4AFFAB
Pantone 3265
69, 0, 37, 0
79, 255, 161
#4FFFA1
Pantone 3268
90, 0, 49, 0
26, 255, 130
#1AFF82
Pantone 327
100, 0, 44, 17
0, 212, 119
#00D477
Pantone 3272
100, 0, 44, 0
0, 255, 143
#00FF8F
Pantone 3275
95, 0, 47, 0
13, 255, 135
#0DFF87
Pantone 3278
100, 0, 55, 5
0, 242, 109
#00F26D
Pantone 328
100, 0, 45, 32
0, 173, 95
#00AD5F
Pantone 3282
100, 0, 46, 15
0, 217, 117
#00D975
Pantone 3285
100, 0, 50, 7
0, 237, 119
#00ED77
Pantone 3288
100, 0, 54, 20
0, 204, 94
#00CC5E
Pantone 329
100, 0, 46, 46
0, 138, 74
#008A4A
Pantone 3292
100, 0, 49, 46
0, 138, 70
#008A46
Pantone 3295
100, 0, 53, 21
0, 201, 95
#00C95F
Pantone 3298
100, 0, 57, 42
0, 148, 64
#009440
Pantone 330
100, 0, 48, 60
0, 102, 53
#006635
Pantone 3302
100, 0, 54, 69
0, 79, 36
#004F24
Pantone 3305
100, 0, 61, 61
0, 99, 39
#006327
Pantone 3308
100, 0, 60, 72
0, 71, 29
#00471D
Pantone 331
24, 0, 16, 0
194, 255, 214
#C2FFD6
Pantone 332
30, 0, 20, 0
179, 255, 204
#B3FFCC
Pantone 333
43, 0, 27, 0
145, 255, 186
#91FFBA
Pantone 334
100, 0, 60, 3
0, 247, 99
#00F763
Pantone 335
100, 0, 65, 30
0, 179, 62
#00B33E
Pantone 336
100, 0, 67, 47
0, 135, 45
#00872D
Pantone 337
31, 0, 20, 0
176, 255, 204
#B0FFCC
Pantone 3375
35, 0, 25, 0
166, 255, 191
#A6FFBF
Pantone 338
47, 0, 32, 0
135, 255, 173
#87FFAD
Pantone 3385
45, 0, 33, 0
140, 255, 171
#8CFFAB
Pantone 339
84, 0, 56, 0
41, 255, 112
#29FF70
Pantone 3395
61, 0, 45, 0
99, 255, 140
#63FF8C
Pantone 340
100, 0, 66, 9
0, 232, 79
#00E84F
Pantone 3405
85, 0, 65, 0
38, 255, 89
#26FF59
Pantone 341
100, 0, 67, 29
0, 181, 60
#00B53C
Pantone 3415
100, 0, 77, 22
0, 199, 46
#00C72E
Pantone 342
100, 0, 71, 43
0, 145, 42
#00912A
Pantone 3425
100, 0, 78, 42
0, 148, 33
#009421
Pantone 343
98, 0, 72, 61
2, 99, 28
#02631C
Pantone 3435
100, 0, 81, 66
0, 87, 16
#005710
Pantone 344
27, 0, 23, 0
186, 255, 196
#BAFFC4
Pantone 345
38, 0, 32, 0
158, 255, 173
#9EFFAD
Pantone 346
55, 0, 47, 0
115, 255, 135
#73FF87
Pantone 347
100, 0, 86, 3
0, 247, 35
#00F723
Pantone 348
100, 0, 85, 24
0, 194, 29
#00C21D
Pantone 349
100, 0, 91, 42
0, 148, 13
#00940D
Pantone 350
79, 0, 100, 75
13, 64, 0
#0D4000
Pantone 351
17, 0, 16, 0
212, 255, 214
#D4FFD6
Pantone 352
27, 0, 25, 0
186, 255, 191
#BAFFBF
Pantone 353
38, 0, 36, 0
158, 255, 163
#9EFFA3
Pantone 354
80, 0, 90, 0
51, 255, 26
#33FF1A
Pantone 355
94, 0, 100, 0
15, 255, 0
#0FFF00
Pantone 356
95, 0, 100, 27
9, 186, 0
#09BA00
Pantone 357
80, 0, 100, 56
22, 112, 0
#167000
Pantone 358
27, 0, 38, 0
186, 255, 158
#BAFF9E
Pantone 359
36, 0, 49, 0
163, 255, 130
#A3FF82
Pantone 360
58, 0, 80, 0
107, 255, 51
#6BFF33
Pantone 361
69, 0, 100, 0
79, 255, 0
#4FFF00
Pantone 362
70, 0, 100, 9
70, 232, 0
#46E800
Pantone 363
68, 0, 100, 24
62, 194, 0
#3EC200
Pantone 364
65, 0, 100, 42
52, 148, 0
#349400
Pantone 365
12, 0, 29, 0
224, 255, 181
#E0FFB5
Pantone 366
20, 0, 44, 0
204, 255, 143
#CCFF8F
Pantone 367
32, 0, 59, 0
173, 255, 105
#ADFF69
Pantone 368
57, 0, 100, 0
110, 255, 0
#6EFF00
Pantone 369
59, 0, 100, 7
97, 237, 0
#61ED00
Pantone 370
56, 0, 100, 27
82, 186, 0
#52BA00
Pantone 371
43, 0, 100, 56
64, 112, 0
#407000
Pantone 372
10, 0, 33, 0
230, 255, 171
#E6FFAB
Pantone 373
16, 0, 46, 0
214, 255, 138
#D6FF8A
Pantone 374
24, 0, 57, 0
194, 255, 110
#C2FF6E
Pantone 375
41, 0, 78, 0
150, 255, 56
#96FF38
Pantone 376
52, 0, 100, 5
116, 242, 0
#74F200
Pantone 377
45, 0, 100, 24
107, 194, 0
#6BC200
Pantone 378
34, 0, 100, 60
67, 102, 0
#436600
Pantone 379
9, 0, 58, 0
232, 255, 107
#E8FF6B
Pantone 380
13, 0, 72, 0
222, 255, 71
#DEFF47
Pantone 381
20, 0, 91, 0
204, 255, 23
#CCFF17
Pantone 382
29, 0, 100, 0
181, 255, 0
#B5FF00
Pantone 383
20, 0, 100, 19
165, 207, 0
#A5CF00
Pantone 384
18, 0, 100, 31
144, 176, 0
#90B000
Pantone 385
3, 0, 100, 58
104, 107, 0
#686B00
Pantone 386
6, 0, 56, 0
240, 255, 112
#F0FF70
Pantone 387
10, 0, 74, 0
230, 255, 66
#E6FF42
Pantone 388
14, 0, 79, 0
219, 255, 54
#DBFF36
Pantone 389
20, 0, 85, 0
204, 255, 38
#CCFF26
Pantone 390
22, 0, 100, 8
183, 235, 0
#B7EB00
Pantone 391
13, 0, 100, 33
149, 171, 0
#95AB00
Pantone 392
7, 0, 100, 49
121, 130, 0
#798200
Pantone 393
3, 0, 55, 0
247, 255, 115
#F7FF73
Pantone 3935
1, 0, 68, 0
252, 255, 82
#FCFF52
Pantone 394
6, 0, 76, 0
240, 255, 61
#F0FF3D
Pantone 3945
3, 0, 85, 0
247, 255, 38
#F7FF26
Pantone 395
8, 0, 85, 0
235, 255, 38
#EBFF26
Pantone 3955
6, 0, 100, 0
240, 255, 0
#F0FF00
Pantone 396
11, 0, 94, 0
227, 255, 15
#E3FF0F
Pantone 3965
8, 0, 100, 0
235, 255, 0
#EBFF00
Pantone 397
10, 0, 100, 11
204, 227, 0
#CCE300
Pantone 3975
0, 0, 100, 29
181, 181, 0
#B5B500
Pantone 398
7, 0, 100, 28
171, 184, 0
#ABB800
Pantone 3985
0, 3, 100, 41
150, 146, 0
#969200
Pantone 399
0, 0, 100, 43
145, 145, 0
#919100
Pantone 3995
0, 3, 100, 64
92, 89, 0
#5C5900
Pantone 400
0, 3, 6, 16
214, 208, 201
#D6D0C9
Pantone 401
0, 5, 11, 23
196, 187, 175
#C4BBAF
Pantone 402
0, 6, 14, 31
176, 165, 151
#B0A597
Pantone 403
0, 7, 17, 43
145, 135, 121
#918779
Pantone 404
0, 8, 22, 56
112, 103, 88
#706758
Pantone 405
0, 10, 33, 72
71, 64, 48
#474030
Pantone 406
0, 5, 6, 16
214, 203, 201
#D6CBC9
Pantone 407
0, 8, 9, 26
189, 174, 172
#BDAEAC
Pantone 408
0, 10, 11, 34
168, 151, 150
#A89796
Pantone 409
0, 13, 15, 45
140, 122, 119
#8C7A77
Pantone 410
0, 18, 21, 56
112, 92, 89
#705C59
Pantone 411
0, 27, 36, 72
71, 52, 46
#47342E
Pantone 412
0, 30, 66, 98
5, 4, 2
#050402
Pantone 413
0, 0, 9, 20
204, 204, 186
#CCCCBA
Pantone 414
0, 0, 10, 30
179, 179, 161
#B3B3A1
Pantone 415
0, 0, 12, 41
150, 150, 132
#969684
Pantone 416
0, 0, 16, 50
128, 128, 107
#80806B
Pantone 417
1, 0, 25, 65
88, 89, 67
#585943
Pantone 418
3, 0, 31, 75
62, 64, 44
#3E402C
Pantone 419
29, 0, 36, 100
0, 0, 0
#000000
Pantone 420
0, 0, 0, 15
217, 217, 217
#D9D9D9
Pantone 421
0, 0, 0, 26
189, 189, 189
#BDBDBD
Pantone 422
0, 0, 0, 33
171, 171, 171
#ABABAB
Pantone 423
0, 0, 0, 44
143, 143, 143
#8F8F8F
Pantone 424
0, 0, 0, 61
99, 99, 99
#636363
Pantone 425
0, 0, 0, 77
59, 59, 59
#3B3B3B
Pantone 426
0, 0, 0, 100
0, 0, 0
#000000
Pantone 427
0, 0, 0, 11
227, 227, 227
#E3E3E3
Pantone 428
2, 0, 0, 18
205, 209, 209
#CDD1D1
Pantone 429
3, 0, 0, 32
168, 173, 173
#A8ADAD
Pantone 430
5, 0, 0, 45
133, 140, 140
#858C8C
Pantone 431
11, 1, 0, 64
82, 91, 92
#525B5C
Pantone 432
23, 2, 0, 77
45, 57, 59
#2D393B
Pantone 433
33, 3, 0, 95
9, 12, 13
#090C0D
Pantone 434
7, 10, 9, 0
237, 230, 232
#EDE6E8
Pantone 435
13, 16, 14, 0
222, 214, 219
#DED6DB
Pantone 436
24, 25, 25, 0
194, 191, 191
#C2BFBF
Pantone 437
46, 45, 46, 0
138, 140, 138
#8A8C8A
Pantone 438
75, 70, 100, 10
57, 69, 0
#394500
Pantone 439
80, 75, 100, 20
41, 51, 0
#293300
Pantone 440
82, 78, 100, 30
32, 39, 0
#202700
Pantone 441
6, 0, 7, 9
218, 232, 216
#DAE8D8
Pantone 442
8, 0, 9, 19
190, 207, 188
#BECFBC
Pantone 443
12, 0, 12, 30
157, 179, 157
#9DB39D
Pantone 444
15, 0, 15, 42
126, 148, 126
#7E947E
Pantone 445
20, 0, 20, 65
71, 89, 71
#475947
Pantone 446
21, 0, 23, 75
50, 64, 49
#324031
Pantone 447
16, 0, 31, 82
39, 46, 32
#272E20
Pantone 448
75, 65, 100, 30
45, 62, 0
#2D3E00
Pantone 4485
0, 26, 100, 69
79, 58, 0
#4F3A00
Pantone 449
70, 60, 100, 20
61, 82, 0
#3D5200
Pantone 4495
0, 20, 95, 46
138, 110, 7
#8A6E07
Pantone 450
65, 55, 100, 10
80, 103, 0
#506700
Pantone 451
33, 29, 55, 0
171, 181, 115
#ABB573
Pantone 4515
0, 9, 50, 24
194, 176, 97
#C2B061
Pantone 452
24, 19, 39, 0
194, 207, 156
#C2CF9C
Pantone 4525
0, 7, 39, 17
212, 197, 129
#D4C581
Pantone 453
14, 11, 25, 0
219, 227, 191
#DBE3BF
Pantone 4535
0, 4, 30, 11
227, 218, 159
#E3DA9F
Pantone 454
9, 7, 16, 0
232, 237, 214
#E8EDD6
Pantone 4545
0, 3, 19, 6
240, 233, 194
#F0E9C2
Pantone 455
0, 17, 100, 65
89, 74, 0
#594A00
Pantone 456
0, 15, 100, 43
145, 124, 0
#917C00
Pantone 457
0, 15, 100, 28
184, 156, 0
#B89C00
Pantone 458
10, 10, 73, 0
230, 230, 69
#E6E645
Pantone 459
6, 7, 55, 0
240, 237, 115
#F0ED73
Pantone 460
4, 5, 44, 0
245, 242, 143
#F5F28F
Pantone 461
3, 3, 35, 0
247, 247, 166
#F7F7A6
Pantone 462
50, 70, 100, 50
64, 38, 0
#402600
Pantone 4625
0, 60, 100, 79
54, 21, 0
#361500
Pantone 463
30, 60, 100, 40
107, 61, 0
#6B3D00
Pantone 4635
0, 48, 96, 44
143, 74, 6
#8F4A06
Pantone 464
10, 50, 100, 35
149, 83, 0
#955300
Pantone 4645
0, 37, 68, 28
184, 116, 59
#B8743B
Pantone 465
20, 32, 58, 0
204, 173, 107
#CCAD6B
Pantone 4655
0, 26, 45, 18
209, 155, 115
#D19B73
Pantone 466
12, 22, 43, 0
224, 199, 145
#E0C791
Pantone 4665
0, 18, 32, 10
230, 188, 156
#E6BC9C
Pantone 467
9, 15, 34, 0
232, 217, 168
#E8D9A8
Pantone 4675
0, 11, 21, 6
240, 213, 189
#F0D5BD
Pantone 468
6, 9, 23, 0
240, 232, 196
#F0E8C4
Pantone 4685
0, 7, 14, 4
245, 228, 211
#F5E4D3
Pantone 469
0, 65, 100, 71
74, 26, 0
#4A1A00
Pantone 4695
0, 81, 100, 74
66, 13, 0
#420D00
Pantone 470
0, 58, 100, 33
171, 72, 0
#AB4800
Pantone 4705
0, 62, 71, 49
130, 49, 38
#823126
Pantone 471
0, 59, 100, 18
209, 86, 0
#D15600
Pantone 4715
0, 42, 45, 34
168, 98, 93
#A8625D
Pantone 472
0, 34, 52, 0
255, 168, 122
#FFA87A
Pantone 4725
0, 32, 35, 25
191, 130, 124
#BF827C
Pantone 473
0, 23, 36, 0
255, 196, 163
#FFC4A3
Pantone 4735
0, 22, 23, 15
217, 169, 167
#D9A9A7
Pantone 474
0, 15, 26, 0
255, 217, 189
#FFD9BD
Pantone 4745
0, 17, 18, 10
230, 190, 188
#E6BEBC
Pantone 475
0, 11, 20, 0
255, 227, 204
#FFE3CC
Pantone 4755
0, 10, 12, 6
240, 216, 211
#F0D8D3
Pantone 476
60, 80, 100, 45
56, 28, 0
#381C00
Pantone 477
50, 85, 100, 38
79, 24, 0
#4F1800
Pantone 478
40, 90, 100, 30
107, 18, 0
#6B1200
Pantone 479
31, 48, 55, 0
176, 133, 115
#B08573
Pantone 480
15, 29, 31, 0
217, 181, 176
#D9B5B0
Pantone 481
9, 19, 21, 0
232, 207, 201
#E8CFC9
Pantone 482
5, 12, 13, 0
242, 224, 222
#F2E0DE
Pantone 483
0, 93, 100, 60
102, 7, 0
#660700
Pantone 484
0, 95, 100, 29
181, 9, 0
#B50900
Pantone 485
0, 95, 100, 0
255, 13, 0
#FF0D00
Pantone 486
0, 47, 41, 0
255, 135, 150
#FF8796
Pantone 487
0, 35, 28, 0
255, 166, 184
#FFA6B8
Pantone 488
0, 26, 19, 0
255, 189, 207
#FFBDCF
Pantone 489
0, 15, 11, 0
255, 217, 227
#FFD9E3
Pantone 490
0, 74, 100, 72
71, 19, 0
#471300
Pantone 491
0, 79, 100, 52
122, 26, 0
#7A1A00
Pantone 492
0, 77, 100, 42
148, 34, 0
#942200
Pantone 493
0, 46, 23, 5
242, 131, 187
#F283BB
Pantone 494
0, 33, 13, 0
255, 171, 222
#FFABDE
Pantone 495
0, 24, 11, 0
255, 194, 227
#FFC2E3
Pantone 496
0, 16, 9, 0
255, 214, 232
#FFD6E8
Pantone 497
0, 70, 100, 78
56, 17, 0
#381100
Pantone 4975
0, 73, 100, 80
51, 14, 0
#330E00
Pantone 498
0, 64, 100, 60
102, 37, 0
#662500
Pantone 4985
0, 62, 51, 48
133, 50, 65
#853241
Pantone 499
0, 63, 100, 48
133, 49, 0
#853100
Pantone 4995
0, 48, 38, 34
168, 88, 104
#A85868
Pantone 500
0, 38, 21, 11
227, 141, 179
#E38DB3
Pantone 5005
0, 38, 27, 23
196, 122, 143
#C47A8F
Pantone 501
0, 27, 13, 3
247, 181, 215
#F7B5D7
Pantone 5015
0, 25, 15, 11
227, 170, 193
#E3AAC1
Pantone 502
0, 18, 10, 1
252, 207, 227
#FCCFE3
Pantone 5025
0, 18, 12, 7
237, 194, 209
#EDC2D1
Pantone 503
0, 11, 8, 0
255, 227, 235
#FFE3EB
Pantone 5035
0, 10, 9, 3
247, 223, 225
#F7DFE1
Pantone 504
70, 100, 100, 35
50, 0, 0
#320000
Pantone 505
50, 100, 100, 25
96, 0, 0
#600000
Pantone 506
45, 100, 100, 15
119, 0, 0
#770000
Pantone 507
13, 49, 23, 0
222, 130, 196
#DE82C4
Pantone 508
5, 36, 11, 0
242, 163, 227
#F2A3E3
Pantone 509
0, 24, 7, 0
255, 194, 237
#FFC2ED
Pantone 510
0, 17, 6, 0
255, 212, 240
#FFD4F0
Pantone 511
70, 100, 50, 20
61, 0, 102
#3D0066
Pantone 5115
80, 100, 70, 15
43, 0, 65
#2B0041
Pantone 512
60, 100, 15, 5
97, 0, 206
#6100CE
Pantone 5125
65, 86, 49, 0
89, 36, 130
#592482
Pantone 513
46, 88, 0, 0
138, 31, 255
#8A1FFF
Pantone 5135
49, 66, 28, 0
130, 87, 184
#8257B8
Pantone 514
15, 50, 0, 0
217, 128, 255
#D980FF
Pantone 5145
30, 45, 12, 0
179, 140, 224
#B38CE0
Pantone 515
7, 38, 0, 0
237, 158, 255
#ED9EFF
Pantone 5155
17, 30, 8, 0
212, 179, 235
#D4B3EB
Pantone 516
3, 27, 0, 0
247, 186, 255
#F7BAFF
Pantone 5165
9, 19, 5, 0
232, 207, 242
#E8CFF2
Pantone 517
0, 18, 0, 0
255, 209, 255
#FFD1FF
Pantone 5175
5, 12, 3, 0
242, 224, 247
#F2E0F7
Pantone 518
80, 100, 60, 10
46, 0, 92
#2E005C
Pantone 5185
88, 100, 85, 10
28, 0, 34
#1C0022
Pantone 519
72, 100, 35, 5
68, 0, 157
#44009D
Pantone 5195
75, 95, 68, 5
61, 12, 78
#3D0C4E
Pantone 520
64, 100, 12, 0
92, 0, 224
#5C00E0
Pantone 5205
52, 63, 48, 0
122, 94, 133
#7A5E85
Pantone 521
27, 47, 0, 0
186, 135, 255
#BA87FF
Pantone 5215
29, 38, 24, 0
181, 158, 194
#B59EC2
Pantone 522
17, 37, 0, 0
212, 161, 255
#D4A1FF
Pantone 5225
17, 27, 15, 0
212, 186, 217
#D4BAD9
Pantone 523
10, 26, 0, 0
230, 189, 255
#E6BDFF
Pantone 5235
10, 17, 10, 0
230, 212, 230
#E6D4E6
Pantone 524
6, 15, 0, 0
240, 217, 255
#F0D9FF
Pantone 5245
6, 10, 7, 0
240, 230, 237
#F0E6ED
Pantone 525
84, 100, 45, 5
39, 0, 133
#270085
Pantone 5255
83, 85, 0, 70
13, 11, 77
#0D0B4D
Pantone 526
77, 100, 7, 0
59, 0, 237
#3B00ED
Pantone 5265
77, 73, 0, 46
32, 37, 138
#20258A
Pantone 527
73, 100, 0, 0
69, 0, 255
#4500FF
Pantone 5275
67, 57, 0, 34
56, 72, 168
#3848A8
Pantone 528
41, 55, 0, 0
150, 115, 255
#9673FF
Pantone 5285
42, 35, 0, 23
114, 128, 196
#7280C4
Pantone 529
26, 40, 0, 0
189, 153, 255
#BD99FF
Pantone 5295
27, 22, 0, 10
168, 179, 230
#A8B3E6
Pantone 530
18, 31, 0, 0
209, 176, 255
#D1B0FF
Pantone 5305
16, 13, 0, 7
199, 206, 237
#C7CEED
Pantone 531
10, 20, 0, 0
230, 204, 255
#E6CCFF
Pantone 5315
6, 6, 0, 5
228, 228, 242
#E4E4F2
Pantone 532
100, 88, 70, 18
0, 25, 63
#00193F
Pantone 533
100, 85, 46, 11
0, 34, 123
#00227B
Pantone 534
100, 82, 30, 5
0, 44, 170
#002CAA
Pantone 535
42, 30, 7, 0
148, 179, 237
#94B3ED
Pantone 536
31, 22, 5, 0
176, 199, 242
#B0C7F2
Pantone 537
22, 14, 3, 0
199, 219, 247
#C7DBF7
Pantone 538
13, 9, 2, 0
222, 232, 250
#DEE8FA
Pantone 539
100, 49, 0, 70
0, 39, 77
#00274D
Pantone 5395
100, 44, 0, 76
0, 34, 61
#00223D
Pantone 540
100, 55, 0, 55
0, 52, 115
#003473
Pantone 5405
58, 17, 0, 46
58, 114, 138
#3A728A
Pantone 541
100, 57, 0, 38
0, 68, 158
#00449E
Pantone 5415
40, 8, 0, 41
90, 138, 150
#5A8A96
Pantone 542
62, 22, 0, 3
94, 193, 247
#5EC1F7
Pantone 5425
30, 4, 0, 31
121, 166, 173
#79A6AD
Pantone 543
41, 11, 0, 0
150, 227, 255
#96E3FF
Pantone 5435
13, 3, 0, 17
189, 219, 241
#BDDBF1
Pantone 544
30, 6, 0, 0
179, 240, 255
#B3F0FF
Pantone 5445
8, 1, 0, 13
216, 224, 229
#D8E0E5
Pantone 545
22, 3, 0, 0
208, 231, 246
#D0E7F6
Pantone 5455
6, 0, 0, 9
227, 234, 238
#E3EAEE
Pantone 546
95, 9, 0, 83
2, 39, 43
#02272B
Pantone 5463
100, 0, 18, 83
0, 43, 36
#002B24
Pantone 5467
100, 0, 30, 95
0, 13, 9
#000D09
Pantone 547
100, 19, 0, 75
0, 52, 64
#003440
Pantone 5473
82, 0, 28, 52
22, 122, 88
#167A58
Pantone 5477
56, 0, 27, 74
29, 66, 48
#1D4230
Pantone 548
100, 24, 0, 64
0, 70, 92
#00465C
Pantone 5483
62, 0, 21, 31
67, 176, 139
#43B08B
Pantone 5487
36, 0, 17, 56
72, 112, 93
#48705D
Pantone 549
54, 7, 0, 27
86, 173, 186
#56ADBA
Pantone 5493
43, 0, 14, 21
115, 201, 173
#73C9AD
Pantone 5497
18, 0, 9, 38
130, 158, 144
#829E90
Pantone 550
39, 4, 0, 21
123, 193, 201
#7BC1C9
Pantone 5503
29, 0, 10, 14
156, 219, 197
#9CDBC5
Pantone 5507
11, 0, 7, 29
161, 181, 168
#A1B5A8
Pantone 551
27, 3, 0, 13
162, 215, 222
#A2D7DE
Pantone 5513
18, 0, 7, 5
199, 242, 225
#C7F2E1
Pantone 5517
9, 0, 6, 18
190, 209, 197
#BED1C5
Pantone 552
15, 0, 0, 9
197, 232, 232
#C5E8E8
Pantone 5523
11, 0, 5, 3
220, 247, 235
#DCF7EB
Pantone 5527
6, 0, 4, 11
213, 227, 218
#D5E3DA
Pantone 553
60, 0, 51, 80
20, 51, 25
#143319
Pantone 5535
66, 0, 57, 82
16, 46, 20
#102E14
Pantone 554
80, 0, 60, 67
17, 84, 34
#115422
Pantone 5545
59, 0, 50, 52
50, 122, 61
#327A3D
Pantone 555
79, 0, 56, 56
24, 112, 49
#187031
Pantone 5555
43, 0, 34, 38
90, 158, 104
#5A9E68
Pantone 556
45, 0, 31, 27
102, 186, 128
#66BA80
Pantone 5565
30, 0, 24, 26
132, 189, 143
#84BD8F
Pantone 557
30, 0, 20, 15
152, 217, 173
#98D9AD
Pantone 5575
20, 0, 16, 17
169, 212, 178
#A9D4B2
Pantone 558
20, 0, 13, 9
186, 232, 202
#BAE8CA
Pantone 5585
12, 0, 11, 10
202, 230, 204
#CAE6CC
Pantone 559
14, 0, 10, 6
206, 240, 216
#CEF0D8
Pantone 5595
7, 0, 8, 7
221, 237, 218
#DDEDDA
Pantone 560
80, 0, 63, 75
13, 64, 24
#0D4018
Pantone 5605
65, 0, 56, 94
5, 15, 7
#050F07
Pantone 561
85, 0, 54, 52
18, 122, 56
#127A38
Pantone 5615
44, 0, 47, 68
46, 82, 43
#2E522B
Pantone 562
85, 0, 50, 31
26, 176, 88
#1AB058
Pantone 5625
28, 0, 30, 51
90, 125, 87
#5A7D57
Pantone 563
52, 0, 32, 1
121, 252, 172
#79FCAC
Pantone 5635
16, 0, 18, 36
137, 163, 134
#89A386
Pantone 564
37, 0, 20, 0
161, 255, 204
#A1FFCC
Pantone 5645
9, 0, 13, 25
174, 191, 166
#AEBFA6
Pantone 565
23, 0, 13, 0
196, 255, 222
#C4FFDE
Pantone 5655
6, 0, 9, 18
197, 209, 190
#C5D1BE
Pantone 566
14, 0, 9, 0
219, 255, 232
#DBFFE8
Pantone 5665
5, 0, 7, 10
218, 230, 213
#DAE6D5
Pantone 567
82, 0, 64, 70
14, 77, 28
#0E4D1C
Pantone 568
88, 0, 57, 36
20, 163, 70
#14A346
Pantone 569
98, 0, 57, 17
4, 212, 91
#04D45B
Pantone 570
48, 0, 29, 0
133, 255, 181
#85FFB5
Pantone 571
32, 0, 19, 0
173, 255, 207
#ADFFCF
Pantone 572
23, 0, 14, 0
196, 255, 219
#C4FFDB
Pantone 573
14, 0, 9, 0
219, 255, 232
#DBFFE8
Pantone 574
34, 0, 81, 71
49, 74, 14
#314A0E
Pantone 5743
33, 0, 85, 82
31, 46, 7
#1F2E07
Pantone 5747
32, 0, 100, 79
36, 54, 0
#243600
Pantone 575
48, 0, 100, 53
62, 120, 0
#3E7800
Pantone 5753
25, 0, 81, 67
63, 84, 16
#3F5410
Pantone 5757
27, 0, 95, 55
84, 115, 6
#547306
Pantone 576
49, 0, 100, 39
79, 156, 0
#4F9C00
Pantone 5763
16, 0, 74, 57
92, 110, 29
#5C6E1D
Pantone 5767
15, 0, 68, 39
132, 156, 50
#849C32
Pantone 577
24, 0, 46, 10
174, 230, 124
#AEE67C
Pantone 5773
9, 0, 43, 38
144, 158, 90
#909E5A
Pantone 5777
10, 0, 49, 28
165, 184, 94
#A5B85E
Pantone 578
20, 0, 40, 6
192, 240, 144
#C0F090
Pantone 5783
6, 0, 28, 27
175, 186, 134
#AFBA86
Pantone 5787
7, 0, 31, 13
206, 222, 153
#CEDE99
Pantone 579
17, 0, 34, 3
205, 247, 163
#CDF7A3
Pantone 5793
4, 0, 21, 18
201, 209, 165
#C9D1A5
Pantone 5797
5, 0, 24, 9
220, 232, 176
#DCE8B0
Pantone 580
12, 0, 26, 2
220, 250, 185
#DCFAB9
Pantone 5803
2, 0, 12, 11
222, 227, 200
#DEE3C8
Pantone 5807
3, 0, 14, 6
233, 240, 206
#E9F0CE
Pantone 581
2, 0, 100, 72
70, 71, 0
#464700
Pantone 5815
0, 0, 91, 79
54, 54, 5
#363605
Pantone 582
13, 0, 100, 46
120, 138, 0
#788A00
Pantone 5825
0, 2, 87, 59
105, 102, 14
#69660E
Pantone 583
23, 0, 100, 17
163, 212, 0
#A3D400
Pantone 5835
0, 2, 67, 40
153, 150, 50
#999632
Pantone 584
12, 0, 79, 6
211, 240, 50
#D3F032
Pantone 5845
0, 1, 47, 30
179, 177, 95
#B3B15F
Pantone 585
11, 0, 66, 2
222, 250, 85
#DEFA55
Pantone 5855
0, 0, 31, 18
209, 209, 144
#D1D190
Pantone 586
9, 0, 53, 0
232, 255, 120
#E8FF78
Pantone 5865
0, 0, 25, 13
222, 222, 166
#DEDEA6
Pantone 587
5, 0, 40, 0
242, 255, 153
#F2FF99
Pantone 5875
0, 0, 18, 8
235, 235, 192
#EBEBC0
Pantone 600
0, 0, 29, 0
255, 255, 181
#FFFFB5
Pantone 601
0, 0, 40, 0
255, 255, 153
#FFFF99
Pantone 602
0, 0, 51, 0
255, 255, 125
#FFFF7D
Pantone 603
0, 0, 69, 1
252, 252, 78
#FCFC4E
Pantone 604
0, 0, 88, 3
247, 247, 30
#F7F71E
Pantone 605
0, 2, 100, 7
237, 232, 0
#EDE800
Pantone 606
0, 4, 100, 12
224, 215, 0
#E0D700
Pantone 607
0, 0, 18, 1
252, 252, 207
#FCFCCF
Pantone 608
0, 0, 32, 2
250, 250, 170
#FAFAAA
Pantone 609
0, 0, 46, 4
245, 245, 132
#F5F584
Pantone 610
0, 0, 58, 6
240, 240, 101
#F0F065
Pantone 611
0, 1, 92, 11
227, 225, 18
#E3E112
Pantone 612
0, 2, 100, 20
204, 200, 0
#CCC800
Pantone 613
0, 4, 100, 30
179, 171, 0
#B3AB00
Pantone 614
0, 0, 20, 4
245, 245, 196
#F5F5C4
Pantone 615
0, 1, 27, 6
240, 237, 175
#F0EDAF
Pantone 616
0, 2, 35, 9
232, 227, 151
#E8E397
Pantone 617
0, 2, 48, 17
212, 207, 110
#D4CF6E
Pantone 618
0, 3, 87, 30
179, 173, 23
#B3AD17
Pantone 619
0, 4, 100, 43
145, 140, 0
#918C00
Pantone 620
0, 5, 100, 53
120, 114, 0
#787200
Pantone 621
13, 0, 10, 2
217, 250, 225
#D9FAE1
Pantone 622
24, 0, 19, 4
186, 245, 198
#BAF5C6
Pantone 623
32, 0, 24, 10
156, 230, 174
#9CE6AE
Pantone 624
44, 0, 35, 20
114, 204, 133
#72CC85
Pantone 625
56, 0, 44, 33
75, 171, 96
#4BAB60
Pantone 626
76, 0, 64, 63
23, 94, 34
#175E22
Pantone 627
90, 0, 76, 84
4, 41, 10
#04290A
Pantone 628
19, 0, 6, 0
207, 255, 240
#CFFFF0
Pantone 629
34, 0, 9, 0
168, 255, 232
#A8FFE8
Pantone 630
47, 0, 11, 0
135, 255, 227
#87FFE3
Pantone 631
67, 0, 12, 2
82, 250, 220
#52FADC
Pantone 632
92, 0, 15, 5
19, 242, 206
#13F2CE
Pantone 633
100, 0, 10, 25
0, 191, 172
#00BFAC
Pantone 634
100, 0, 9, 40
0, 153, 139
#00998B
Pantone 635
32, 0, 8, 0
173, 255, 235
#ADFFEB
Pantone 636
45, 0, 9, 0
140, 255, 232
#8CFFE8
Pantone 637
55, 0, 9, 0
115, 255, 232
#73FFE8
Pantone 638
83, 0, 10, 0
43, 255, 230
#2BFFE6
Pantone 639
100, 0, 5, 5
0, 242, 230
#00F2E6
Pantone 640
100, 0, 0, 22
0, 199, 199
#00C7C7
Pantone 641
100, 4, 0, 30
0, 171, 179
#00ABB3
Pantone 642
16, 4, 0, 2
210, 240, 250
#D2F0FA
Pantone 643
25, 7, 0, 4
184, 228, 245
#B8E4F5
Pantone 644
42, 15, 0, 6
139, 204, 240
#8BCCF0
Pantone 645
57, 28, 0, 9
100, 167, 232
#64A7E8
Pantone 646
69, 34, 0, 11
70, 150, 227
#4696E3
Pantone 647
100, 56, 0, 23
0, 86, 196
#0056C4
Pantone 648
100, 62, 0, 54
0, 45, 117
#002D75
Pantone 649
14, 6, 0, 1
217, 237, 252
#D9EDFC
Pantone 650
24, 9, 0, 2
190, 227, 250
#BEE3FA
Pantone 651
38, 18, 0, 6
149, 197, 240
#95C5F0
Pantone 652
60, 34, 0, 10
92, 151, 230
#5C97E6
Pantone 653
100, 62, 0, 20
0, 78, 204
#004ECC
Pantone 654
100, 64, 0, 38
0, 57, 158
#00399E
Pantone 655
100, 65, 0, 52
0, 43, 122
#002B7A
Pantone 656
14, 4, 0, 0
219, 245, 255
#DBF5FF
Pantone 657
24, 8, 0, 0
194, 235, 255
#C2EBFF
Pantone 658
41, 20, 0, 0
150, 204, 255
#96CCFF
Pantone 659
64, 35, 0, 0
92, 166, 255
#5CA6FF
Pantone 660
90, 57, 0, 0
26, 110, 255
#1A6EFF
Pantone 661
100, 69, 0, 9
0, 72, 232
#0048E8
Pantone 662
100, 72, 0, 18
0, 59, 209
#003BD1
Pantone 663
7, 6, 0, 0
237, 240, 255
#EDF0FF
Pantone 664
11, 9, 0, 0
227, 232, 255
#E3E8FF
Pantone 665
20, 17, 0, 2
200, 207, 250
#C8CFFA
Pantone 666
31, 30, 0, 7
164, 166, 237
#A4A6ED
Pantone 667
52, 49, 0, 14
105, 112, 219
#6970DB
Pantone 668
65, 64, 0, 30
62, 64, 179
#3E40B3
Pantone 669
76, 78, 0, 47
32, 30, 135
#201E87
Pantone 670
0, 13, 0, 0
255, 222, 255
#FFDEFF
Pantone 671
1, 20, 0, 0
252, 204, 255
#FCCCFF
Pantone 672
3, 34, 0, 0
247, 168, 255
#F7A8FF
Pantone 673
6, 49, 0, 0
240, 130, 255
#F082FF
Pantone 674
9, 67, 0, 0
232, 84, 255
#E854FF
Pantone 675
17, 100, 0, 3
205, 0, 247
#CD00F7
Pantone 676
6, 100, 0, 22
187, 0, 199
#BB00C7
Pantone 677
2, 13, 0, 0
250, 222, 255
#FADEFF
Pantone 678
3, 21, 0, 0
247, 201, 255
#F7C9FF
Pantone 679
5, 27, 0, 0
242, 186, 255
#F2BAFF
Pantone 680
10, 43, 0, 2
225, 142, 250
#E18EFA
Pantone 681
21, 61, 0, 4
193, 95, 245
#C15FF5
Pantone 682
25, 79, 0, 12
168, 47, 224
#A82FE0
Pantone 683
11, 100, 0, 43
129, 0, 145
#810091
Pantone 684
0, 17, 0, 2
250, 207, 250
#FACFFA
Pantone 685
0, 25, 0, 3
247, 186, 247
#F7BAF7
Pantone 686
0, 30, 0, 5
242, 170, 242
#F2AAF2
Pantone 687
2, 44, 0, 12
220, 126, 224
#DC7EE0
Pantone 688
5, 57, 0, 19
196, 89, 207
#C459CF
Pantone 689
7, 77, 0, 34
157, 39, 168
#9D27A8
Pantone 690
0, 97, 0, 59
105, 3, 105
#690369
Pantone 691
0, 15, 8, 1
252, 215, 232
#FCD7E8
Pantone 692
0, 23, 10, 2
250, 192, 225
#FAC0E1
Pantone 693
0, 30, 12, 6
240, 168, 211
#F0A8D3
Pantone 694
0, 43, 19, 10
230, 131, 186
#E683BA
Pantone 695
0, 58, 28, 25
191, 80, 138
#BF508A
Pantone 696
0, 84, 54, 40
153, 24, 70
#991846
Pantone 697
0, 93, 70, 51
125, 9, 37
#7D0925
Pantone 698
0, 16, 8, 0
255, 214, 235
#FFD6EB
Pantone 699
0, 24, 10, 0
255, 194, 230
#FFC2E6
Pantone 700
0, 36, 14, 0
255, 163, 219
#FFA3DB
Pantone 701
0, 53, 20, 0
255, 120, 204
#FF78CC
Pantone 702
0, 69, 34, 5
242, 75, 160
#F24BA0
Pantone 703
0, 83, 54, 16
214, 36, 99
#D62463
Pantone 704
0, 100, 80, 27
186, 0, 37
#BA0025
Pantone 705
0, 9, 5, 0
255, 232, 242
#FFE8F2
Pantone 706
0, 17, 10, 0
255, 212, 230
#FFD4E6
Pantone 707
0, 30, 14, 0
255, 179, 219
#FFB3DB
Pantone 708
0, 46, 22, 0
255, 138, 199
#FF8AC7
Pantone 709
0, 66, 38, 0
255, 87, 158
#FF579E
Pantone 710
0, 79, 58, 0
255, 54, 107
#FF366B
Pantone 711
0, 100, 80, 2
250, 0, 50
#FA0032
Pantone 712
0, 14, 31, 0
255, 219, 176
#FFDBB0
Pantone 713
0, 19, 41, 0
255, 207, 150
#FFCF96
Pantone 714
0, 28, 54, 0
255, 184, 117
#FFB875
Pantone 715
0, 37, 71, 0
255, 161, 74
#FFA14A
Pantone 716
0, 47, 91, 0
255, 135, 23
#FF8717
Pantone 717
0, 55, 100, 2
250, 112, 0
#FA7000
Pantone 718
0, 58, 100, 8
235, 99, 0
#EB6300
Pantone 719
0, 10, 25, 0
255, 230, 191
#FFE6BF
Pantone 720
0, 15, 34, 1
252, 215, 167
#FCD7A7
Pantone 721
0, 24, 52, 3
247, 188, 119
#F7BC77
Pantone 722
0, 36, 76, 9
232, 149, 56
#E89538
Pantone 723
0, 45, 95, 17
212, 116, 11
#D4740B
Pantone 724
0, 53, 100, 37
161, 76, 0
#A14C00
Pantone 725
0, 55, 100, 49
130, 59, 0
#823B00
Pantone 726
0, 8, 23, 2
250, 230, 192
#FAE6C0
Pantone 727
0, 15, 34, 5
242, 206, 160
#F2CEA0
Pantone 728
0, 21, 48, 10
230, 181, 119
#E6B577
Pantone 729
0, 31, 61, 18
209, 144, 82
#D19052
Pantone 730
0, 39, 76, 29
181, 110, 43
#B56E2B
Pantone 731
0, 53, 100, 54
117, 55, 0
#753700
Pantone 732
0, 56, 100, 64
92, 40, 0
#5C2800
Pantone 7401
0, 4, 18, 0
255, 245, 209
#FFF5D1
Pantone 7402
0, 6, 30, 0
255, 240, 179
#FFF0B3
Pantone 7403
0, 10, 50, 0
255, 230, 128
#FFE680
Pantone 7404
0, 9, 80, 0
255, 232, 51
#FFE833
Pantone 7405
0, 10, 100, 0
255, 230, 0
#FFE600
Pantone 7406
0, 18, 100, 0
255, 209, 0
#FFD100
Pantone 7407
0, 22, 85, 11
227, 177, 34
#E3B122
Pantone 7408
0, 25, 95, 0
255, 191, 13
#FFBF0D
Pantone 7409
0, 30, 95, 0
255, 179, 13
#FFB30D
Pantone 7410
0, 30, 55, 0
255, 179, 115
#FFB373
Pantone 7411
0, 35, 69, 0
255, 166, 79
#FFA64F
Pantone 7412
0, 42, 100, 7
237, 138, 0
#ED8A00
Pantone 7413
0, 53, 100, 4
245, 115, 0
#F57300
Pantone 7414
0, 46, 100, 11
227, 123, 0
#E37B00
Pantone 7415
0, 18, 15, 0
255, 209, 217
#FFD1D9
Pantone 7416
0, 60, 60, 0
255, 102, 102
#FF6666
Pantone 7417
0, 75, 75, 0
255, 64, 64
#FF4040
Pantone 7418
0, 70, 60, 5
242, 73, 97
#F24961
Pantone 7419
0, 60, 45, 18
209, 84, 115
#D15473
Pantone 7420
0, 80, 42, 20
204, 41, 118
#CC2976
Pantone 7421
0, 100, 30, 61
99, 0, 70
#630046
Pantone 7422
0, 9, 5, 0
255, 232, 242
#FFE8F2
Pantone 7423
0, 55, 22, 0
255, 115, 199
#FF73C7
Pantone 7424
0, 75, 30, 0
255, 64, 179
#FF40B3
Pantone 7425
0, 90, 30, 7
237, 24, 166
#ED18A6
Pantone 7426
0, 100, 45, 18
209, 0, 115
#D10073
Pantone 7427
0, 100, 65, 28
184, 0, 64
#B80040
Pantone 7428
0, 80, 45, 55
115, 23, 63
#73173F
Pantone 7429
0, 18, 3, 0
255, 209, 247
#FFD1F7
Pantone 7430
2, 31, 0, 0
250, 176, 255
#FAB0FF
Pantone 7431
0, 38, 2, 5
242, 150, 237
#F296ED
Pantone 7432
0, 55, 3, 10
230, 103, 223
#E667DF
Pantone 7433
0, 75, 15, 15
217, 54, 184
#D936B8
Pantone 7434
0, 80, 15, 20
204, 41, 173
#CC29AD
Pantone 7435
0, 100, 10, 35
166, 0, 149
#A60095
Pantone 7436
3, 8, 0, 0
247, 235, 255
#F7EBFF
Pantone 7437
6, 20, 0, 0
240, 204, 255
#F0CCFF
Pantone 7438
15, 35, 0, 0
217, 166, 255
#D9A6FF
Pantone 7439
20, 35, 0, 0
204, 166, 255
#CCA6FF
Pantone 7440
30, 40, 0, 0
179, 153, 255
#B399FF
Pantone 7441
36, 50, 0, 0
163, 128, 255
#A380FF
Pantone 7442
50, 70, 0, 0
128, 77, 255
#804DFF
Pantone 7443
6, 5, 0, 0
240, 242, 255
#F0F2FF
Pantone 7444
20, 17, 0, 0
204, 212, 255
#CCD4FF
Pantone 7445
30, 20, 0, 3
173, 198, 247
#ADC6F7
Pantone 7446
43, 38, 0, 0
145, 158, 255
#919EFF
Pantone 7447
60, 58, 0, 19
83, 87, 207
#5357CF
Pantone 7448
32, 42, 0, 55
78, 67, 115
#4E4373
Pantone 7449
72, 100, 77, 46
39, 0, 32
#270020
Pantone 7450
20, 10, 0, 0
204, 230, 255
#CCE6FF
Pantone 7451
40, 21, 0, 0
153, 201, 255
#99C9FF
Pantone 7452
50, 32, 0, 0
128, 173, 255
#80ADFF
Pantone 7453
50, 26, 0, 0
128, 189, 255
#80BDFF
Pantone 7454
50, 24, 0, 10
115, 174, 230
#73AEE6
Pantone 7455
80, 53, 0, 0
51, 120, 255
#3378FF
Pantone 7456
55, 35, 0, 7
107, 154, 237
#6B9AED
Pantone 7457
12, 0, 2, 0
224, 255, 250
#E0FFFA
Pantone 7458
40, 0, 5, 6
144, 240, 228
#90F0E4
Pantone 7459
57, 0, 6, 13
95, 222, 209
#5FDED1
Pantone 7460
100, 0, 0, 5
0, 242, 242
#00F2F2
Pantone 7461
78, 28, 0, 0
56, 184, 255
#38B8FF
Pantone 7462
100, 50, 0, 10
0, 115, 230
#0073E6
Pantone 7463
100, 43, 0, 65
0, 51, 89
#003359
Pantone 7464
25, 0, 10, 0
191, 255, 230
#BFFFE6
Pantone 7465
50, 0, 25, 0
128, 255, 191
#80FFBF
Pantone 7466
70, 0, 23, 0
77, 255, 196
#4DFFC4
Pantone 7467
95, 0, 25, 0
13, 255, 191
#0DFFBF
Pantone 7468
100, 10, 0, 28
0, 165, 184
#00A5B8
Pantone 7469
100, 20, 0, 40
0, 122, 153
#007A99
Pantone 7470
80, 15, 0, 45
28, 119, 140
#1C778C
Pantone 7471
28, 0, 14, 0
184, 255, 219
#B8FFDB
Pantone 7472
52, 0, 25, 0
122, 255, 191
#7AFFBF
Pantone 7473
70, 0, 38, 8
70, 235, 145
#46EB91
Pantone 7474
90, 0, 28, 22
20, 199, 143
#14C78F
Pantone 7475
50, 0, 25, 30
89, 179, 134
#59B386
Pantone 7476
100, 0, 43, 60
0, 102, 58
#00663A
Pantone 7477
80, 0, 10, 68
16, 82, 73
#105249
Pantone 7478
18, 0, 14, 0
209, 255, 219
#D1FFDB
Pantone 7479
55, 0, 50, 0
115, 255, 128
#73FF80
Pantone 7480
60, 0, 50, 0
102, 255, 128
#66FF80
Pantone 7481
60, 0, 55, 0
102, 255, 115
#66FF73
Pantone 7482
80, 0, 75, 0
51, 255, 64
#33FF40
Pantone 7483
85, 0, 100, 55
17, 115, 0
#117300
Pantone 7484
100, 0, 85, 50
0, 128, 19
#008013
Pantone 7485
6, 0, 10, 0
240, 255, 230
#F0FFE6
Pantone 7486
20, 0, 30, 0
204, 255, 179
#CCFFB3
Pantone 7487
30, 0, 45, 0
179, 255, 140
#B3FF8C
Pantone 7488
43, 0, 60, 0
145, 255, 102
#91FF66
Pantone 7489
60, 0, 80, 7
95, 237, 47
#5FED2F
Pantone 7490
45, 0, 80, 35
91, 166, 33
#5BA621
Pantone 7491
32, 0, 100, 40
104, 153, 0
#689900
Pantone 7492
12, 0, 50, 7
209, 237, 119
#D1ED77
Pantone 7493
14, 0, 36, 10
197, 230, 147
#C5E693
Pantone 7494
25, 0, 40, 15
163, 217, 130
#A3D982
Pantone 7495
25, 0, 80, 30
134, 179, 36
#86B324
Pantone 7496
40, 0, 100, 38
95, 158, 0
#5F9E00
Pantone 7497
40, 30, 70, 25
115, 134, 57
#738639
Pantone 7498
25, 0, 100, 80
38, 51, 0
#263300
Pantone 7499
0, 2, 15, 0
255, 250, 217
#FFFAD9
Pantone 7500
0, 2, 15, 3
247, 242, 210
#F7F2D2
Pantone 7501
0, 4, 20, 6
240, 230, 192
#F0E6C0
Pantone 7502
0, 8, 35, 10
230, 211, 149
#E6D395
Pantone 7503
0, 12, 35, 25
191, 168, 124
#BFA87C
Pantone 7504
0, 25, 45, 40
153, 115, 84
#997354
Pantone 7505
0, 30, 70, 55
115, 80, 34
#735022
Pantone 7506
0, 5, 15, 0
255, 242, 217
#FFF2D9
Pantone 7507
0, 10, 30, 0
255, 230, 179
#FFE6B3
Pantone 7508
0, 15, 40, 4
245, 208, 147
#F5D093
Pantone 7509
0, 20, 50, 5
242, 194, 121
#F2C279
Pantone 7510
0, 30, 72, 11
227, 159, 64
#E39F40
Pantone 7511
0, 45, 100, 25
191, 105, 0
#BF6900
Pantone 7512
0, 46, 100, 33
171, 92, 0
#AB5C00
Pantone 7513
0, 18, 28, 3
247, 203, 178
#F7CBB2
Pantone 7514
0, 24, 38, 5
242, 184, 150
#F2B896
Pantone 7515
0, 35, 50, 12
224, 146, 112
#E09270
Pantone 7516
0, 52, 100, 35
166, 80, 0
#A65000
Pantone 7517
0, 60, 100, 44
143, 57, 0
#8F3900
Pantone 7518
0, 40, 55, 60
102, 61, 46
#663D2E
Pantone 7519
50, 60, 100, 48
66, 53, 0
#423500
Pantone 7520
0, 16, 19, 0
255, 214, 207
#FFD6CF
Pantone 7521
0, 25, 20, 10
230, 172, 184
#E6ACB8
Pantone 7522
0, 40, 30, 16
214, 129, 150
#D68196
Pantone 7523
0, 40, 35, 20
204, 122, 133
#CC7A85
Pantone 7524
0, 55, 60, 27
186, 84, 74
#BA544A
Pantone 7525
0, 45, 50, 30
179, 98, 89
#B36259
Pantone 7526
0, 65, 100, 35
166, 58, 0
#A63A00
Pantone 7527
0, 2, 6, 7
237, 232, 223
#EDE8DF
Pantone 7528
0, 3, 10, 10
230, 223, 207
#E6DFCF
Pantone 7529
0, 4, 12, 17
212, 203, 186
#D4CBBA
Pantone 7530
0, 8, 21, 32
173, 160, 137
#ADA089
Pantone 7531
0, 10, 27, 50
128, 115, 93
#80735D
Pantone 7532
0, 17, 50, 65
89, 74, 45
#594A2D
Pantone 7533
0, 22, 85, 85
38, 30, 6
#261E06
Pantone 7534
0, 2, 8, 10
230, 225, 211
#E6E1D3
Pantone 7535
0, 3, 15, 20
204, 198, 173
#CCC6AD
Pantone 7536
0, 4, 22, 32
173, 166, 135
#ADA687
Pantone 7537
3, 0, 10, 20
198, 204, 184
#C6CCB8
Pantone 7538
9, 0, 13, 30
162, 179, 155
#A2B39B
Pantone 7539
2, 0, 9, 36
160, 163, 149
#A0A395
Pantone 7540
0, 0, 0, 72
71, 71, 71
#474747
Pantone 7541
2, 0, 0, 5
237, 242, 242
#EDF2F2
Pantone 7542
10, 0, 3, 16
193, 214, 208
#C1D6D0
Pantone 7543
7, 0, 0, 30
166, 179, 179
#A6B3B3
Pantone 7544
10, 1, 0, 40
138, 151, 153
#8A9799
Pantone 7545
23, 2, 0, 63
73, 92, 94
#495C5E
Pantone 7546
33, 4, 0, 72
48, 69, 71
#304547
Pantone 7547
35, 4, 0, 94
10, 15, 15
#0A0F0F
Pantone black
0, 13, 49, 98
5, 4, 3
#050403
Pantone
CMYK
RGB
HEX
gelb
0, 3, 100, 0
255, 247, 0
#FFF700
orange (021)
0, 53, 100, 0
255, 120, 0
#FF7800
rot (warm)
0, 75, 90, 0
255, 64, 26
#FF401A
rot (032)
0, 90, 86, 0
255, 26, 36
#FF1A24
Rub. rot
0, 100, 15, 4
245, 0, 208
#F500D0
Rhod. rot
3, 89, 0, 0
247, 28, 255
#F71CFF
grün
100, 0, 59, 0
0, 255, 105
#00FF69
purpur
38, 88, 0, 0
158, 31, 255
#9E1FFF
violet
98, 100, 0, 0
5, 0, 255
#0500FF
Ref. blau
100, 73, 0, 2
0, 67, 250
#0043FA
blau (072)
100, 88, 0, 5
0, 29, 242
#001DF2
Pantone
CMYK
RGB
HEX
Prc black
0, 0, 0, 100
0, 0, 0
#000000
Prc cyan
100, 0, 0, 0
0, 255, 255
#00FFFF
Prc magenta
0, 100, 0, 0
255, 0, 255
#FF00FF
Prc gelb
0, 0, 100, 0
255, 255, 0
#FFFF00
Prc blau
100, 10, 0, 10
0, 207, 230
#00CFE6
Pantone
CMYK
RGB
HEX
black 1
0, 13, 49, 98
5, 4, 3
#050403
black 2
0, 3, 55, 87
33, 32, 15
#21200F
black 3
60, 0, 60, 91
9, 23, 9
#091709
black 4
0, 22, 100, 89
28, 22, 0
#1C1600
black 5
0, 38, 20, 89
28, 17, 22
#1C1116
black 6
100, 35, 0, 100
0, 0, 0
#000000
black 7
0, 0, 15, 82
46, 46, 39
#2E2E27

-}