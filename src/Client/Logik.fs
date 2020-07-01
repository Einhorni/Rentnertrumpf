module Logik

open System
open Fulma
open Fable.React

type Spieler =
| Spieler1
| Gegner1
| Gegner2
| Gegner3
| Keiner


type Vergleichswert =
| Krankheit of int
| Alter of int
| Sonstiges of int
| Schamgefühl of int


type Karte = {
    Name: string
    Krankheit: string* int
    Alter: int
    Sonstiges: string * int
    Schamgefühl: int
    Spieler: Spieler
    }


type Decks = Karte list list

type KartenDerRunde = Karte list

type NiedrigOderhochGewinnt =
| Niedrig
| Hoch

let spieler =[Spieler1; Gegner1; Gegner2; Gegner3]

let fakeKarte = {
    Name = "ich hasse dich"
    Krankheit = ("yeaahhh", 0)
    Alter = 0
    Sonstiges = ("Depp", 0)
    Schamgefühl = -10 
    Spieler = Keiner} 




//########################################################################################
//############################### Erstellung der Karten ##################################
//########################################################################################


//sollen die auch nach Geschlecht gehen?? Wegen gewissen Eigenschaften?
let namen = ["Helga"; "Hilde"; "Herbert"; "Manfred"; "Harald"; "Doris"; "Heidrun"; "Adelheid"; "Werner"; "Joseph"; "Hans"; "Jacob"; "Franz"; "Barbara"; "Ursula"; "Agatha"; "Gertrud"; "Friedrich"; "Rosalinde"; "Walburga"; "Anton"; "Karl"; "Oskar"; "Gerda"; "Erna"; "Erwin"; "Wilhelm"; "Ludwig"; "Arnold"; "Alwin"; "Karla"; "Frieda"]


let krankheiten = [
    ("Krampfadern", 3)
    ("Inkontinenz", 1)
    ("Wasser in den Beinen", 2)
    ("Verstopfungen", 2)
    ("Rücken", 3)
    ("Hüfte", 4)
    ("Altersichtigkeit", 3)
    ("Schwerhörigkeit", 2)
    ("Blähbauch", 1)
    ("Haarausfall", 3)
    ("Altersflecken", 1)
    ("eingewachsene Zehennägel", 5)
    ("Hühneraugen", 2)
    ("Vergesslichkeit", 3)
    ]


let r = Random()

let alter =
    [for i in 1..32 do r.Next(65, 106)]

let alterFünferWert alterWert =
    [
        [65..72]
        [73..80]
        [81..88]
        [89..96]
        [97..105]
    ]
    |> List.findIndex (fun x ->
        x |> List.contains alterWert
        )

let sonstiges = [
    ("Veganer", 5)
    ("Vegetarier", 4)
    ("Spanner", 3)
    ("Katzenlady/-gentleman", 2)
    ("Taubenfütterer", 2)
    ("Schwer Beschäftiger", 4)
    ("Schwarzseher", 1)
    ("Aluhut", 4)
    ("\"Früher war alles...\"-Berichter", 3)
    ("Supermarkt-Belagerer", 5)
    ("Trinker", 2)
    ("Diva", 3)
    ]


let schamgefühl =
    [for i in 1..32 do r.Next(1,6)]


let kartenSet = 
    [for i in 1..32 do //Namen sollen nur einmal auftauchen 
            {
        Name = namen.[i-1]
        Krankheit = krankheiten.[r.Next(14)]
        Alter = alter.[r.Next(32)]
        Sonstiges = sonstiges.[r.Next(12)]
        Schamgefühl = schamgefühl.[r.Next(32)]
        Spieler = Spieler1
        }]





//########################################################################################
//############################### Spielfunktionen ########################################
//########################################################################################




let kartenDecks (initialSet:Karte list) anzahlSpieler : Decks = 
    let initialDeck1 = []
    let initialDeck2 = []
    let initialDeck3 = []

    let kurzesSet = 
        let zufällige = [for i in 1..2 do r.Next(1,33)]
        kartenSet
        |> List.indexed
        |> List.filter (fun (i,x) -> (i <> zufällige.[0]-1) && (i <> zufällige.[1]-1)) 
        |> List.map (fun (i,x) -> x)

    let rec createDecks (kartenSet:Karte list) deck1 deck2 deck3 =
        let längeDeck = 
            if anzahlSpieler = 2 then 16
            elif anzahlSpieler = 3 then 10
            else 8

        if kartenSet.Length > längeDeck then 

            let newCardAndSet (kartenSet:Karte list) = 
                let newCard1 = 
                    let index = r.Next(1, kartenSet.Length+1)-1
                    kartenSet.[index]
                let newSet1 = 
                    kartenSet
                    |> List.filter (fun x -> x <> newCard1)
                newSet1, newCard1

            let (newSet1, newCard1) = newCardAndSet kartenSet
            let (newSet2, newCard2) = newCardAndSet newSet1
            let (newSet3, newCard3) = newCardAndSet newSet2

            let newDeck1 = deck1 @ [newCard1]
            let newDeck2 = deck2 @ [newCard2]
            let newDeck3 = deck3 @ [newCard3]

            if anzahlSpieler = 2 then createDecks newSet1 newDeck1 []  []
            elif anzahlSpieler = 3 then createDecks newSet2 newDeck1 newDeck2  []
            else createDecks newSet3 newDeck1 newDeck2 newDeck3
        else [kartenSet; deck1 |> List.map (fun x -> {x with Spieler = Gegner1}); deck2 |> List.map (fun x -> {x with Spieler = Gegner2}); deck3 |> List.map (fun x -> {x with Spieler = Gegner3})]
    let set =
        if anzahlSpieler = 3 then kurzesSet
        else initialSet
    createDecks set initialDeck1 initialDeck2 initialDeck3


let kartenDerRunde (decks:Decks) anzahlAllerSpieler (indizesAktiverSpieler: int list) : KartenDerRunde =

    let indizesAllerSpieler =
                match anzahlAllerSpieler with
                | 2 -> [for i in 0..1 do i]
                | 3 -> [for i in 0..2 do i]
                | 4 -> [for i in 0..3 do i]
                | _ -> []

    indizesAllerSpieler 
    |> List.map (fun x ->
        indizesAktiverSpieler 
        |> List.map (fun y ->
            y=x
        ))
    |> List.indexed
    |> List.map (fun (i,x) ->
        if x |> List.contains true then (i,true)
        else (i,false)
        )
    //aktive spieler bekommen karten, andere nicht
    |> List.map (fun (i,x) ->
        if x = true then decks.[i] |> List.head
        else fakeKarte
        )


let werIstDran anzahlSpieler aktuellerSpielerIndex (indizesAktiverSpieler:int list) =

    if aktuellerSpielerIndex = 4 then
        r.Next(0,anzahlSpieler)
    else
        if aktuellerSpielerIndex = anzahlSpieler-1 then
            indizesAktiverSpieler.[0]
        else
            if indizesAktiverSpieler |> List.contains (aktuellerSpielerIndex+1) then
                aktuellerSpielerIndex+1
            else
                if aktuellerSpielerIndex = (indizesAktiverSpieler |> List.last) then
                    indizesAktiverSpieler.Head
                else
                    //nächstgrößerer verfügbarer index von aktive spieler
                    indizesAktiverSpieler
                    |> List.filter (fun x -> x > aktuellerSpielerIndex)
                    |> List.min


let vergleicheKarten (karten:KartenDerRunde) (vergleichswert:Vergleichswert) (rundenGewinner:Spieler list) niedrigOderhochGewinnt =
    let kartenDerGewinner =
        karten
        |> List.filter (fun x ->
        rundenGewinner |> List.contains x.Spieler)
    let werte =
        kartenDerGewinner
        |> List.map (fun x ->            
            match vergleichswert with
            | Krankheit _ ->
                let (_ , wert) = x.Krankheit
                wert, x.Spieler
            | Alter _ -> x.Alter, x.Spieler
            | Sonstiges _ ->
                let (_ , wert) = x.Sonstiges
                wert, x.Spieler
            | Schamgefühl _  -> x.Schamgefühl, x.Spieler
            )
    let niedrigste =
        werte
        |> List.sortBy (fun (x,y) -> x)
        |> List.map (fun (x,y) -> x)
        |> List.head

    let höchste =
        werte
        |> List.sortBy (fun (x,y) -> x)
        |> List.rev
        |> List.map (fun (x,y) -> x)
        |> List.head
        

    if niedrigOderhochGewinnt = Niedrig then
        werte
        |> List.filter (fun (x,y) -> x = niedrigste) // alle mit niedrigsten Werten behalten
        |> List.map (fun (x,y) -> y) //nur den/ die spieler zurückgeben
    else
        werte |> List.filter (fun (x,y) -> x = höchste)
        |> List.map (fun (x,y) -> y) 


let changeDecks (aktuelleDecks: Decks) (sieger:Spieler) (aktuelleKarten:Karte list) =

    //siegerDeck

    let siegerDeckIndex =
        [Spieler1; Gegner1; Gegner2; Gegner3]
        |> List.findIndex (fun x -> x = sieger)

    let siegerDeck =
        aktuelleDecks.[siegerDeckIndex]
        
        |> List.filter (fun x ->
            let gewinnerKarte =
                aktuelleKarten
                |> List.filter (fun y -> y.Spieler = sieger)
                |> List.exactlyOne
            x <> gewinnerKarte
            )


    let neuesSiegerDeck =
        aktuelleKarten
        |> List.filter (fun x -> x.Spieler <> Keiner)    //alle Fakekarten rausnehmen (werden gebraucht, um unabhängig Anzahl Spieler die gleiche Länge der KArtenliste zu haben) und auch die Karte des Siegers
        |> List.append siegerDeck //alle Karten der Runde dem Siegerdeck hinzufügen
        |> List.map (fun x -> {x with Spieler = sieger})

    //verliererDecks

    let (verliererDecksAnfang, rest) =
        aktuelleDecks
        |> List.splitAt siegerDeckIndex

    let (rest2, verliererDecksEnde) =
        aktuelleDecks
        |> List.splitAt (siegerDeckIndex + 1)

    let verliererDecks = 
        verliererDecksAnfang @ verliererDecksEnde

    let verlierKarten = //offene Karten außer Spieler = keiner oder Siegerkarte
        aktuelleKarten //alle offenen Karten
        |> List.filter (fun x -> x.Spieler <> Keiner && x.Spieler <> sieger)

    let neueVerliererDecks =
        verliererDecks
        |> List.map (fun verliererDeck ->
            //Browser.Dom.console.log(verliererDeck.ToString())
            if verliererDeck = [] then []
            else

                verliererDeck
                |> List.filter (fun karteImVerliererDeck ->
                    verlierKarten
                    |> List.exists (fun z ->
                        
                        karteImVerliererDeck.Name = z.Name)
                    |> not    
                )
        )


    //neue Decks

    let neueDecksAnfang, neueDecksEnde =
        neueVerliererDecks //3 elemente
        |> List.splitAt (siegerDeckIndex)
    
    neueDecksAnfang @ [neuesSiegerDeck] @ neueDecksEnde





//########################################################################################
//############################### Hilfsfunktionen ########################################
//########################################################################################




let niedrigOderHochGewinnt vergleichswert =
    match vergleichswert with
    | Krankheit _ -> Hoch
    | Alter _ -> Hoch
    | Sonstiges _ -> Hoch
    | Schamgefühl _ -> Niedrig


let wählenDerBestenKarte (vergleichsWerteGegner:Vergleichswert list) =

    let genormteVergleichswerte =
        vergleichsWerteGegner
        |> List.map (fun x ->
            match x with
            | Krankheit kpunkte -> kpunkte
            | Alter apunkte -> alterFünferWert apunkte
            | Sonstiges spunkte -> spunkte
            | Schamgefühl schpunkte ->
                match schpunkte with
                | 1 -> 5
                | 2 -> 4
                | 3 -> 3
                | 4 -> 2
                | 5 -> 1
                | _ -> 0
            )

    let indexVergleichswert = 
        genormteVergleichswerte // listeWerte //[1;3;6;2]
        |> List.indexed
        |> List.sortByDescending (fun (i, x) -> x)
        |> List.map (fun (i,x) -> i)
        |> List.head

    vergleichsWerteGegner.[indexVergleichswert]


let wessenKarte werIstDranIndex indizes =
    //weristdran ist zwischengewinner --> immer noch dran

    //let werIstDranIndex = 2
    //let indizes = [1;3]

    if indizes |> List.contains werIstDranIndex then
        werIstDranIndex
    //wer ist dran ist kein gewinner --> nächster gegner ist dran
    else
        if [0;1;2] |> List.contains werIstDranIndex  then

            
            if (indizes |> List.last) < werIstDranIndex then 
                indizes |> List.min
            else
            //der nächsthöhere Wert wird genommen
                indizes
                |> List.filter (fun x -> x > werIstDranIndex)
                |> List.min

            //der niedrigste Wert wird genommen
        else indizes
            |> List.min


//nur noch die WErte, die bisher noch nicht verglichen wurden vergleichen
let vergleichslisteGegner möglicheVergleichswerteGegner bereitsGespielteVergleichswerte =
    möglicheVergleichswerteGegner
    |> List.filter (fun x ->
        bereitsGespielteVergleichswerte
        |> List.exists (fun y ->
            match x, y with
            | Krankheit _ , Krankheit _ ->
                true
            | Alter _ , Alter _ ->
                true
            | Sonstiges _ , Sonstiges _ ->
                true
            | Schamgefühl _ , Schamgefühl _ ->
                true
            | _ -> false
            )
        |> not)



let indizesAktiverSpieler spielerListe aktiveSpieler =
    spielerListe
    |> List.indexed
    |> List.filter (fun (i,spieler) ->
        aktiveSpieler
        |> List.exists (fun aktiveSpieler ->
            //Fable /JS hat ein Problem mit Vergleich von Disc Unions, sporadisch: false bei gleichen DIngernCases, deswegen ein To String Vergleich
            aktiveSpieler.ToString() = spieler.ToString())
        )
    |> List.map (fun (i,x) -> i)

 