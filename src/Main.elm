module Main exposing (main)

import Color
import Color.Generator as ColGen
import Debug
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)
import Palette.Tango as Tango


col : Color.Color -> Color
col paletteColor =
    paletteColor
        |> Color.toRGB
        |> (\( red, green, blue ) ->
                rgb255 (round red) (round green) (round blue)
           )


googleFont : String -> String -> Attribute msg
googleFont fontName weight =
    let
        fontString =
            String.replace " " "+" fontName
    in
    Font.family
        [ Font.external
            { url =
                "https://fonts.googleapis.com/css?family="
                    ++ fontString
                    ++ ":"
                    ++ weight
            , name = fontName
            }
        ]



--     googleFont "Alegreya" "400i,400,700i"
--     googleFont "Alegreya SC" "400"


bookType =
    googleFont "Cormorant" "400i,400,700"


smallCaps =
    googleFont "Cormorant SC" "400"


scaleInt =
    scaleFloat
        >> round


scaleFloat =
    modular 36 1.618


xs =
    scaleInt -2


small =
    scaleInt -1


medium =
    scaleInt 1


large =
    scaleInt 2


xl =
    scaleInt 3


xxl =
    scaleInt 4


grey x =
    rgb x x x


zeroPad =
    { left = 0, right = 0, top = 0, bottom = 0 }


titleStyle =
    [ Font.bold
    , Font.size large
    , Font.letterSpacing <| scaleFloat -1
    ]


italics =
    [ Font.italic ]


type alias Skill =
    { name : String
    , when : String
    , roll : String
    , sep : String
    , and : String
    , alts : List String
    , after : Maybe String
    }


skillEl : Skill -> Element msg
skillEl skill =
    let
        bullet txt =
            paragraph
                [ onLeft <|
                    el
                        [ moveLeft 3
                        , moveDown 4
                        ]
                    <|
                        text "➤"
                ]
                [ text txt ]
    in
    textColumn
        []
        [ el titleStyle <| text skill.name
        , paragraph
            [ width <| px 720
            ]
            ([ el italics <| text skill.when
             , text skill.roll
             , el [ smallCaps ] <| text <| String.toLower skill.name
             , text skill.sep
             ]
                ++ smallCapsFormat skill.and
            )
        , textColumn [ paddingXY 0 xs ] <| List.map bullet skill.alts
        , case skill.after of
            Nothing ->
                none

            Just str ->
                paragraph [] <| smallCapsFormat str
        ]


smallCapsFormat : String -> List (Element msg)
smallCapsFormat txt =
    let
        allCaps : String -> Bool
        allCaps str =
            String.toUpper str == str

        boolPair : String -> ( Bool, String )
        boolPair str =
            ( allCaps str, str )

        toElement : ( Bool, String ) -> Element msg
        toElement pair =
            case pair of
                ( True, str ) ->
                    el [ smallCaps ] <| text <| String.toLower str

                ( False, str ) ->
                    text str
    in
    txt
        |> String.words
        |> List.map boolPair
        |> List.map toElement
        |> List.intersperse (text " ")


main : Html msg
main =
    layout
        [ padding xl
        , Background.color <| grey 0
        , Font.color <| grey 0.8
        , Font.size medium
        , Font.regular
        , Font.wordSpacing -1
        , bookType
        , Font.alignLeft
        ]
    <|
        column
            [ spacing medium ]
            [ skillEl kraft
            , skillEl finess
            , skillEl insikt
            ]


kraft : Skill
kraft =
    { name = "KRAFT"
    , when = "När du brukar kraft, "
    , roll = "slå+"
    , sep = " "
    , and = " och välj bland alternativen. På 12+, tre. På 10-11, två. På 7-9, ett."
    , alts =
        [ "Du vållar stor skada."
        , "Du tar liten skada i gengäld."
        , "Du driver dem tillbaka, griper något eller skapar en möjlighet."
        ]
    , after = Nothing
    }


finess : Skill
finess =
    { name = "FINESS"
    , when = "När du använder finess, "
    , roll = "slå+"
    , sep = " "
    , and = " och välj bland alternativen. På 12+, tre. På 10-11, två. På 7-9, ett."
    , alts =
        [ "Du gör det snabbt."
        , "Du undviker besvär, kompromisser eller kostnad."
        , "Du gör det oundvikligt, stilfullt eller med större effekt."
        ]
    , after = Nothing
    }


insikt : Skill
insikt =
    { name = "INSIKT"
    , when = "När du använder insikt, "
    , roll = "slå+"
    , sep = ". "
    , and = "På 12+, ta 3 HUM. På 10-11, ta 2 HUM. På 7-9, ta 1 HUM. Spendera HUM en-mot-en under scenen för att ställa frågor till SL från listan:"
    , alts =
        [ "Vad är det egentligen som pågår här?"
        , "Vad bör jag hålla utkik efter?"
        , "Vad är det bästa sättet att... ?"
        , "Vad är det de faktiskt känner? Vad vill de?"
        , "Hur kan jag få dem att... ?"
        ]
    , after = Just "På en miss får du inget HUM, men du kan ställa en fråga på en gång."
    }


allTxt : String
allTxt =
    """
KRAFT
När du brukar kraft, slå+KRAFT och välj bland alternativen.
På 12+, tre. På 10-11, två. På 7-9, ett.
Du vållar stor skada.
Du tar liten skada i gengäld.
Du driver dem tillbaka, griper något eller skapar en möjlighet.

FINESS
När du använder finess, slå+FINESS och välj bland alternativen.
På 12+, tre. På 10-11, två. På 7-9, ett.
Du gör det snabbt.
Du undviker besvär, kompromisser eller kostnad.
Du gör det oundvikligt, stilfullt eller med större effekt.

INSIKT
När du använder insikt, slå+INSIKT.
På 12+, ta 3 HUM. På 10-11, ta 2 HUM. På 7-9, ta 1 HUM. 
Spendera HUM en-mot-en under scenen för att ställa frågor till SL från listan: 
Vad är det egentligen som pågår här?
Vad bör jag hålla utkik efter?
Vad är det bästa sättet att ____?
Vad är det de faktiskt känner? Vad vill de?
Hur kan jag få dem att ____?
På en miss får du inget HUM, men du kan ställa en fråga på en gång.

HJÄLPA
För varje drag ovan (Kraft, Finess, Insikt), 
kan du också spendera ett av valen på dessa:
Du hjäper någon, de får +1 på sitt slag.
Du hindrar eller stör, de får -2 på sitt slag.

VILJA
När du stålsätter dig mot skada, tvång eller övernaturliga fasor, 
välj en reaktion nedan som du hoppas att du inte får och slå+VILJA.
På 10+, så gör du ingen av dem. På 7-9, gör du en av dem du inte valde, SL väljer. 
På en miss så reagerar på det sätt du inte ville.
Du fryser, stelnar till, lämnar dig själv vidöppen.
Panik, drar dig ur, flyr.
Kollapsar, släpper taget, ger upp.
Ursinne, tappar kontrollen, gör oavsiktlig skada.
Tar mer Chock eller Skada.


DÖDTID
När du har dödtid mellan arbetstillfällena, välj en:
Gå på krogen. Återhämta dig från Chock. Om du har mer Chock än VI, läk till VI.
Om har till VI eller mindre, läk all Chock.
Gå till doktorn. Läk Skada. Om du har mer Skada än IX, läk till IX. 
Om du har till IX eller mindre, läk all Skada.
Ta ett sidokneg. Välj: utkastare, kurir, hantverk, krematorium, brott, destilleri, 
hamn, smedja, vidunderjakt, slakthus, stall, handel. 
Inkassera 2 till din Gömma eller 1 Favör.
Du kan välja mer än ett val genom att spendera polletter en-mot-en. 
Du kan välja samma alternativ två gånger.

SKAPANDE AV ROLLPERSON
Välj ett emblem och ett namn från listorna nedan. Välj ett hemland och två begåvningar. Tilldela poäng (minst -1, högst +3) till Kraft, Finess, Insikt och Vilja så att summan är +2 . Du kan till exempel välja 
[+1, 0, 0, +1], [+2, 0, 0, 0] eller [0, +3, -1, 0].

NAMN
Ditt emblem är prytt med ett namn, valt från registret över De Renade, 
såsom traditionen påbjuder:
Agardh, Ancker, Bessel, Bureus, Cantor, Ehrensvärd, Fagel, Fahlcrantz, Faltzburg, Granith, Helmholtz., Horn, Järta, Karlfeldt, Klein, Klinga, Kreutz, Kuratowski, Kätting, Landau, Lewenstein, Markov, Morosova, Mörner, Nordensköld, Novikov, Nygaard, Odhner, Palmenbach, Reuterdahl, Richter, Rosenholm, Scheffer, Schenkel, Stenhammar, Stiernhielm, Ståhlberg, Sylow, Tarski, Wessel, Wetterstedt, Winter, Wolfram, Wrede, Wägner, Zelmanov, Ziegler.

Du har även ett personnamn:
Alberus, Albrekt, Alexius, Amandus, Amatias, Anatolius, Ansgarius, Anund, Arkadius, Arverus, Augustinus, Baltasar, Calixtus, Cardian, Cauntus, Cornelius, Eberhard, Edbert, Edvill, Egbertus, Engelbrekt, Enock, Erhard, Ernstfrid, Esaias, Eukarius, Eukasius, Eusebius, Evergistus, Florentin, Georgius, Gerion, Germanus, Gideon, Gillhof, Hedvigius, Hegosius, Helgard, Hildbrand, Hubertus, Hyginus, Idmar, Ignatius, Irenius, Isedor, Josias, Justinus, Lagus, Lambert, Laurentius, Leander, Leontius, Lisanian, Loritz, Mauritz, Maximilnus, Maxinus, Mortialis, Nestor, Nikanor, Nikiator, Nikodemus, Octalis, Ofian, Oswald, Rimhold, Salomon, Severin, Sixtus, Teotard, Theofilus, Titus, Viard, Viktornius, Villgott, Vindician, Vindirian, Virgiulius, Vitalis, Volerius, Voltner, Wikard, Willehard, Wolmar, Zakarias, Zefanius.

Adelhild, Adelina, Admalia, Akaria, Aktylia, Albertina, Aletty. Bernardina, Alexia, Alexina, Alfi, Alfilda, Alida, Allvingny, Amalia, Amintea, Anakordia, Andriettta, Anselina, Antoinetta, Appollonia, Aqvilina, Askaria, Autmia, Bedikta, Benedicta, Bertina, Concordia, Elfrid, Elova, Emelinda, Eufemia, Eulalia, Eurenia, Eusebia, Frosyne, Frysyne, Genette, Göthilda, Hilderan, Hortense, Idunia, Isodora, Jakobina, Josefina, Joselia, Juliana, Justina, Kolette, Leandra, Madeli, Madelina, Magnuetta, Malgunda, Melinda, Nikolada, Nikolovia, Notalia, Octavia, Olava, Olivia, Ottolina, Paulina, Rasida, Reinholdina, Romana, Rubertina, Sekunda, Selina, Serene, Serudia, Severina, Teolina, Theodora, Tiliana, Valdina, Valmina, Vanda, Visea, Welmina, Zipova.

MOT DET ÖVERNATURLIGA
När du leder dina oxar mot ett väsen, så ställer SL frågor till dig:
Har du avslutat ditt lärlingsskap? I så fall, ta +1.
Har du avslutat ditt gesällskap? I så fall, ta +1.
Är du en mästeroxe? I så fall, ta +1.
Har ni uppbackning från ett annat gäng? I så fall, ta +1.
Höll ni ett ankarlotteri? Om ni inte har något Ankare, ta -1.
Har ni en oxe i vardera av de tre andra rollerna? Om inte, ta -1.
Svär de andra oxarna att följa dina order utan att tveka? Om inte, ta -1.
Har ni jobbat på den här linjen förut? Om inte, ta -1.

Slå sedan och välj alternativ. På 12+, alla tre. På 10-11, två. På 7-9, ett.
Ni tar initiativet.
Ni bibehåller en ordningssam uppställning. 
Oxarna står där du vill ha dem och är redo att ingripa.
Ni griper en särskild möjlighet eller fördel; 
oxar får +1 tillsvidare medan de utnyttjar detta.

Dessutom: oxen som leder laget gör första draget mot spöket/spökena:
Ankare: fånga spökets intresse och dra in det i kontakt med dig (VILJA).
Spindel: använd ett blixtnät (FINESS) för att fjättra spöket till en gastflaska.
Torn: använd en blixtkrok (KRAFT) för att slita lös spöket från tåget 
och/eller försvaga det.
Uggla: studera spöket och situationen med dina spökbrillor (INSIKT) 
för att bestämma den bästa spelöppningen.

FÅ DIN VILJA IGENOM
När du försöker få som du vill, slå.
Om din VILJA är högre, ta +1.
Om din Nivå är högre, ta +1.
Om du är en ädling från Östmark, ta +1.
Om du hotar med kroppskada och din KRAFT är högre, ta +1.
På 12+, är de så överväldigade att de gör som du säger utan eftertanke. 
På 10-11, väljer de: antingen göra som du säger eller ta 2 i Chock. 
På 7-9, samma som 10-11, men 1 i Chock. 
Om du hotade med kroppsskada kan de tvinga dig att fullfölja och 
ta Skada istället för Chock.


SKADA OCH CHOCK
En skadas allvarlighetsgrad, enligt antal klocksektioner:
Knytnävsslag, brottning, indirekt elektricitet, kvävande dödslandsdimma.
Rejält kok stryk, blixtkroksstöt, manifestationsattack från spöke.
Dödliga vapen, blixtkroksflöde, omfattande manifestationsattack från spöke.
Elektriska stolen, explosion, långt fall, kraftig kollision.
Fall från tåg i full fart, brinna som en fackla.

En chocks allvarlighetsgrad, enligt antal klocksektioner:
Se en lagkamrat skadas, möte med spöke, misshandla en person.
Se en vän skadas, lagkamrat skadas allvarligt, mildare övernaturliga konstigheter, 
gå närmre ett spöke, misshandla en person med dödligt våld.
Direktkontakt med ett spöke, vän skadad allvarligt, lagkamrat dödad, utföra mord.
Psykiskt övergrepp från spöke, vän dödad.
Övergrepp från en betydande övernaturlig makt.
 
När du tar Skada till XII, så dör du. När du tar Chock till XII, går ditt förstånd i kras.
Du kan undvika en instans av Skada eller Chock genom att välja att markera ett Ärr eller ett Trauma istället. 
Ärr och Trauma är permanenta. Fyll i dem medurs.

NIVÅER
Du startar på nivå 0. När du går upp en nivå, välj en annan begåvning från ditt hemland eller 
från ett land där du arbetat en kombination av 4 linjer/sidokneg. 
När du blir en mästare, lägg till +1 på en egenskap (max +3).

RÖRANDE SPÖKLINJERNA
Det är år 891 för det imperium vilket enade kataklysmens splittrade öar under ett styre - 
all ära till hans majestät, den Odödlige Kejsaren.


Ni arbetar på spöklinjerna - den elektriska järnvägen som passerar genom de bläckmörka dödsländerna mellan städerna. De dödas andar, fria att vandra världen sedan Dödens Portar förstördes under kataklysmen, fastnar ofta i de kraftfulla elektriska fälten alstrade av tågen. Linjeoxar som ni går längs vagnarna, magnetiserade stövlar slamrande och andningsmasker väsande, för att rensa bort de överträdande andarna med era blixtkrokar innan de gör för mycket skada.


Varje stad i Imperiet är omgärdad av sprakande blixttorn som skapar ett elektriskt hölje vilket andarna inte kan tränga igenom. Lagen säger att alla lik ska förbrännas med åskolja (för att förstöra ande-essensen inuti) men välbärgade medborgare, andesekternas kättare eller kriminella element arrangerar ofta så att ett spöke ska kunna undgå förintelse i krematoriet. Dessa så kallade “överlöparandar” tas också om hand av oxar som ni. 
Mot ersättning, förstås.


När Imperiets spejare hittar en marbrunn i dödsländeras obygd kallar de ibland in ett erfaret gäng oxar för att hjälpa till att rensa ut den. Det här är det mest farliga arbetet - långt från den relativa säkerheten hos elrälsen och möjligheten till en snabb flykt längs linjen. Men betalningen är avsevärd och en oxe som rensar ut en marbrunn och överlever kan kanske precis skrapa ihop och stoppa undan en tillräcklig summa för att en dag kunna dra sig tillbaka med stil.

HÄNDELSER PÅ LINJEN
Tåget måste stanna. Reparationer behövs, skadade spår, elavbrott, 
krav från en viktig passagerare (en naturfilosof, en adelsman), kejserliga order, 
upptäckt marbrunn, väder.
Tåget kan inte stanna. Lokföraren lamslagen, gasreglaget skadat, 
krav från en viktig passagerare, kejserliga order, hotande väderomslag.
Övernaturlig händelse. Tiden saktar in/snabbas upp, landskapet skiftar, masshysteri/inbillning, minnesförlust, besynnerligt väder (eldregn, svart vind).
Spöke. Känner någon igen den?
Spöken, åtskilliga. Vems namn ropar de?
Spöke, mäktigt. Inte nödvändigtvis en mänsklig ande. Värd 5 rensningar.

SPEKTROLOGI
Själ. En levande kropp med sin egen ande.
Besatt. En levande kropp med två eller fler andar.
Skal. En levande kropp utan en ande.
Odöd. En död kropp med en ande.
Spöke. En ande utan kropp.
Marbrunn. En reva i verkligheten där spöken och andra övernaturliga väsen 
samlas för att dra energi.
Elektroplasm. Den energetiska lämning vilken blir kvar efter att ett spöke 
blivit “tystat” (förstört). Handhas med extrem försiktighet.
Häxa. En person som har en känslighet för andar. Kan ha förmågan att frammana och 
kommunicera med spöken, men folk i allmänhet tror inte på sådant.

POLLETTER, GÖMMOR, FAVÖRER
När du arbetar på en linje så får du betalt i Polletter: stämplade blyklumpar som du kan byta mot 
mat, logi och diverse hos Det Kejserliga Rälskontoret.
Linjens gradering x din Nivå = intjänade Polletter.
Du får också 1+Nivå i risktillägg om du är Ankaret och +1 Pollett för varje röjt spöke.
Du kan spendera dina polletter under dödtid för att återhämta dig från 
skada och chock, för att ta ett sidokneg, etc.
Din Gömma är byte du gömt undan för din pension. Ju större din gömma, 
desto bättre kommer du ha det. Din gömma är som din poängställning i spelet.
Gömma 0: Förlorad själ. Du dör ensam i rännstenen.
Gömma 1-10: Desperat trashank. Du dör på gatan, kall och glömd.
Gömma 11-20: Stackars människa. Du dör i ett stinkande fattighus, översköljd av sprit och elände.
Gömma 21-30: Magert. Du dör i ett pyttelitet (men varmt) kyffe som du kan kalla ditt eget.
Gömma 31-40: Blygsamt. Du dör i ett simpelt hus eller lägenhet, med några små bekvämligheter.
Gömma 41-50: Bra. Du dör i ett välförsett hus eller lägenhet, begagnande några få lyxartiklar.


Favörer kan intjänas genom att göra ett sidokneg. Du kan spendera favörer på följande:
1 Favör: +2 spöken rensade, tilldelas en annan tåglinje, begära specialutrustning.
2 Favörer: +1 arbetad tåglinje.
3 Favörer: +1 arbetad roll, begära skräddarsydd utrustning.
4 Favörer: Få tillgång till hemlig information, få kontakt med någon betydelsefull, 
göra en omfattande förfrågan.

Sidokneg
När du tar ett sidokneg, slå 1T6.
På 1, så uppstår en komplikation. SL berättar hur du skaffar dig en fiende, 
hamnar i en risig situation eller måste återbetala en tjänst.
På 2, 3 eller 4, utför du arbetet och hör dessutom ett rykte 
angående spöken (se tabellerna).
På 5, utför du arbetet och väljer dessutom en av: tjäna +2 Gömma eller 
få en tillförlitlig ledtråd till spökrelaterat arbete (se tabellerna).
På 6, utför du arbetet och väljer dessutom en av: tjäna +2 Gömma eller +1 Favör eller 
bli erbjuden spökrelaterat arbete av någon som betalar bra (SL ger dig detaljerna).

SL: När de gör sidokneg, ge deras arbetsgivare ett namn, hemland och utmärkande drag. 
Håll reda på dessa SLP och använd dem för att fylla i världen runt rollpersonerna. 

UTRUSTNING & OLJA
Spökröjningsutrustning går på olja från vidunder (så kallad åskolja). 
Ett bruk spänningssätter ett föremål. SL kan kräva ytterligare oljebruk beroende 
på omständigheter och tärningsutfall.
Specialutrustning:
Scharlakanselixir: en liten medicinflaska med rödaktig vätska. 
Helar 1 skada omedelbart vid inmundigande.
Violremedium: en liten medicinflaska med lila vätska. 
Återställer 1 chock omedelbart vid inmundigande.
Blixtbur: en uppsättning bärbara stänger, kablar och en generator som 
kan alstra en elektrisk barriär vilken andar inte kan passera.


RYKTEN & LEDTRÅDAR
Sidokneg, resultat 2-4. På 5, en tillförlitlig ledtråd, ge även ett namn och detaljer.
slå 1T66: 

11 - Någon skapar Skal för att tillhandahålla billig arbetskraft.
12 - Köttsliga Extasens Tempel köper bebodda gastflaskor.
13 - Nattmarknaden drivs av odöda.
14 - En omänsklig ande har setts runt hamnen.
15 - Någon på universitet köper elektroplasm.
16 - Det finns en marbrunn någonstans inne i staden.

21 - Någon säljer ett “botemedel” för Skal.
22 - Någon köper och säljer bebodda gastflaskor på en bar under kajen.
23 - En högt uppsatt kejserlig tjänsteman är besatt.
24 - Någon driver ett menageri med djurspöken.
25 - Någon raffinerar elektroplasm till en drog.
26 - Det finns en Spårare som tar mutor för att hålla andar gömda.

31 - Någon leder ett gäng av Skal.
32 - En upptäcktsresande påstår att han har en karta över alla marbrunnar i dödsländerna.
33 - Några välbeställda medborgare arrangerar “hemsökelsefester”.
34 - Folk lever på en ö utanför kusten utan någon elbarriär.
35 - En tatuerare har blandat sitt bläck med elektroplasm.
36 - Åtskilliga viktiga ämbetsmän inom Kyrkan är andekultister.

41 - En Oxe blev Urholkad för att han vågade vidröra en kunglighet under ett jobb.
42 - Den “övergivna” norra tåglinjen används för att skeppa gastflaskor… någonstans.
43 - Häxor har sina krafter på grund av att de härstammar från en demonisk blodslinje.
44 - En uppfinnare har byggt en “andefinnare” och behöver testare.
45 - Violremedium tillverkas av elektroplasm!
46 - En andesekt träffas i ett forntida tempel under staden.

51 - Någon såg Skal samlas vid den gamla reliken på torget.
52 - Det finns hemliga portar i staden som bara häxor/spöken/odöda/besatta kan se.
53 - Någon försöker organisera en fackförening för linjeoxar.
54 - Det finns en häxa som kan tillkalla spöken från din blodslinje.
55 - Ett “spökskepp” har siktats utanför kusten.
56 - Myternas demoner är verkliga och det är de som ligger bakom andesekterna.

61 - Någon erbjuder belöning för Skal som tas in levande.
62 - Det finns ett uråldrigt spöke i tornet i Svartdalen som är äldre än kataklysmen.
63 - Någon är villig att betala Oxar för att smuggla varor längs linjerna.
64 - Det finns en välbärgad odöd som erbjuder märkliga uppdrag.
65 - Kejsaren är ansvarig för att Dödens Portar förstördes.
66 - Andekultister smugglar spöken i besatta människor.

UPPDRAGSGIVARE (sidokneg, resultat 6)
11 - Samlare
12 - Naturfilosof
13 - Detektiv
14 - Vakt
15 - Kanslist
16 - Lönnmördare


21 - Bankir
22 - Upptäcksresande
23 - Ämbetsman
24 - Handelsman
25 - Skribent
26 - Tjuv


31 - Flykting
32 - Köpman
33 - Skriftlärd
34 - Domare
35 - Smugglare
36 - Häxa
41 - Spårare
42 - Sjöman
43 - Spion
44 - Sjökapten
45 - Revolutionär
46 - Hallick|Bordellmamma


51 - Langare
52 - Legosoldat
53 - Kurir
54 - Butiksinnehavare
55 - Präst
56 - Konstnär


61 - Ädling
62 - Gangsterledare
63 - Diplomat
64 - Soldat
65 - Demon
66 - Doktor


HÄNDELSER I STADEN
11 - Farsot
12 - Flyktingar
13 - Rivning
14 - Varubrist
15 - Parad
16 - Välgörenhet
21 - Festival
22 - Politiska oroligheter
23 - Valrörelse
24 - Utsvävningar
25 - Berömdhet
26 - Strejk


31 - Räd
32 - Förbud
33 - Skandal
34 - Upptäckt
35 - Högtid
36 - Rymning


41 - Revolution
42 - Byggnation
43 - Undantagstillstånd
44 - Paranoia
45 - Kravaller
46 - Diplomati


51 - Olycka
52 - Övernaturligt väder
53 - Mönstring
54 - Lönnmord
55 - Nedstängning
56 - Belägring


61 - Katastrof
62 - Brottsvåg
63 - Utvandring
64 - Häxjakt
65 - Hysteri
66 - Andesektsmöten


SPÖKENS KARAKTÄRSDRAG
+1 på slaget för varje decennium spöket existerat.
1 - Avundsjuk, desperat, våldsam, hysterisk, nyckfull, övergående.
2 - Nyfiken, beräknande, bedräglig, klipsk, utforskande, kunnig.
3 - Profetisk, insiktsfull, sanningsenlig, avslöjande, vägledande, lärorik.
4 - Reaktiv, revirtänkande, dominant, ihärdig, djärv, krävande.
5 - Arg, oförutsägbar, aggressiv, vild, primitiv, hämndlysten.
6 - Galen, förvirrad, hämndlysten, bisarr, destruktiv, sinnessjuk.

SPELLEDARPERSONERS KARAKTÄRSDRAG
1 - Tålmodig, vänlig, förlåtande, mild.
2 - Vild, ohövlig, primitiv, oborstad.
3 - Misstänksam, beräknande, slug.


4 - Hedervärd, rättfram, pålitlig.
5 - Konstig, kuslig, hemlighetsfull.
6 - Lojal, handfast, orubblig, kompromisslös.



GHOST LINES - version 1.5 | january 2013 | onesevendesign.com/ghost_lines.pdf
Författare: John Harper
Speltestare: Allison Arth, Suzanne Asprea, Keith Anderson, Jonathan Walton, Mike
Standish, Judd Karlman, Jim DelRosso, Charlotte Williams.
Inspirerad av: 
Apocalypse World av V. Baker; Dishonored av Arkane Studios; 
Ghostbusters av D. Aykroyd, H. Ramis, I. Reitman; 
Final Fantasy: The Spirits Within av A. Reinert & J. Vintar; 
Planarch Codex av J. Walton; MicroTraveller av C. Bennett.
Översättning: Ralf Northman (2017)

Licens: Creative Commons attribution non-commercial share-alike. (CC BY-NC-SA)
"""
