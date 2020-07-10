module Client

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Fulma
open Logik


//let mutable debugCounter = 0
//debugCounter <- debugCounter+1
//if debugCounter > 10 then failwith "loop"
//debugCounter <- 0


type Model = {

    Decks: Decks 
    AnzahlSpieler: int
    AktiveSpieler: Spieler list
    Spieler: Spieler list

    KartenDerRunde: KartenDerRunde
    RundenGewinner: Spieler list
    Vergleichswerte: Vergleichswert list
    MöglicheGegnerVergleichswerte: Vergleichswert list

    WerIstDranIndex: int option 
    ZwischenWerIstDran: int option 

    Weitersuchen: bool  //wird am Ende gelöscht
    }

type Msg =
    | Ziehen of int
    | GegnerErsterZug
    | NeuStart
    | Vergleich of Vergleichswert
    | NeueRunde
    | SucheFinalenRundenGewinner
    | WeitersuchenButton //wird am Ende gelöscht



let init () : Model * Cmd<Msg> =
    let initialModel = {
        Decks = []
        AnzahlSpieler = 0
        AktiveSpieler = []
        KartenDerRunde = []

        RundenGewinner = []
        Vergleichswerte = []
        MöglicheGegnerVergleichswerte = []
        
        Spieler = []
        WerIstDranIndex = None
        ZwischenWerIstDran = None

        Weitersuchen = false
        }
    
    initialModel, Cmd.none



let möglicheVergleichsWerteGegner (model:Model) (zwischenGewinner:Spieler list)  =

    let neuerWErIstDran =
        indizeesZwischenGewinner model.Spieler zwischenGewinner
        |> wessenKarte  (index model.WerIstDranIndex)

    let (kh, kpunkte) = model.KartenDerRunde.[neuerWErIstDran].Krankheit
    let (so, spunkte) = model.KartenDerRunde.[neuerWErIstDran].Sonstiges
    
    [Krankheit kpunkte; Alter model.KartenDerRunde.[neuerWErIstDran].Alter; Sonstiges spunkte; Schamgefühl model.KartenDerRunde.[neuerWErIstDran].Schamgefühl]


let gibtEsEinenVerlierer model neuesModelMitVerlierer (zwischenGewinner: Spieler list) vergleichswerte = 

    let decksMitNurEinerKarte:Decks = 
        model.Decks
        |> List.filter (fun x ->
            x.Length = 1)

    if decksMitNurEinerKarte.Length <> 0 then 

        let spielerMitNurEinerKarte =
            decksMitNurEinerKarte
            |> List.collect (fun x ->
                x
                |> List.map (fun  y -> y.Spieler)
                )

        let spielerMitEinerKarteEinVerlierer = 
                spielerMitNurEinerKarte
                |> List.filter (fun x ->
                    zwischenGewinner 
                    |> List.contains x
                    |> not //könnte anders evtl. PRobleme geben, weil nur ein Treffer evaluiert und dann abgebrochen wird, deswegen vorsichthalber not
                    )

        if 
            spielerMitEinerKarteEinVerlierer.Length = 0
        then 
            neuesModelMitVerlierer, Cmd.none
        else

            let neueAktiveSpielerOhneVerlierer =
                model.AktiveSpieler
                |> List.filter (fun x ->
                    spielerMitEinerKarteEinVerlierer
                    |> List.contains x
                    |> not
                    )

            let neuerWerIstDran =
                Some
                    (indizeesZwischenGewinner model.Spieler zwischenGewinner
                    |> wessenKarte (index model.WerIstDranIndex))

            let neuesModelOhneVerlierer = {
                model with
                    AktiveSpieler = neueAktiveSpielerOhneVerlierer
                    RundenGewinner = zwischenGewinner
                    Weitersuchen = false
                    MöglicheGegnerVergleichswerte = []
                    Vergleichswerte = vergleichswerte
                    WerIstDranIndex =  neuerWerIstDran}
            neuesModelOhneVerlierer, Cmd.none

    else neuesModelMitVerlierer, Cmd.none



let contentKarten wert idxSpieler krankheit  sonstiges (eigenschaften: int list) (vergleichsWerte:(int -> Vergleichswert) list) model =
    if model.KartenDerRunde.[idxSpieler].Spieler <> Keiner then
        let punkte = String.init eigenschaften.[wert] (fun x -> "+")
        let text =
            str
                (match wert with
                | 0 -> (sprintf "Krankheit: %s %s" krankheit punkte)
                | 1 -> (sprintf "Alter: %i" model.KartenDerRunde.[idxSpieler].Alter)
                | 2 -> (sprintf "Sonstiges: %s %s" sonstiges punkte)
                | 3 -> (sprintf "Schamgefühl: %i" model.KartenDerRunde.[idxSpieler].Schamgefühl)
                | _ -> "")
        if idxSpieler <> 0 then
            if schonVerglichen model.Vergleichswerte (vergleichsWerte.[wert] eigenschaften.[wert]) then
                text
            else str ""
        else
            text
    else str ""



let update (msg : Msg) (model : Model) : Model * Cmd<Msg> =
    match msg with
    | Ziehen anzahlSpieler ->

        let spielerListe = spieler.[0..anzahlSpieler-1 ]

        let decks =
            if model.KartenDerRunde = [] then
                kartenDecks (kartenSet()) anzahlSpieler
            else changeDecks model.Decks model.RundenGewinner.[0] model.KartenDerRunde

        let aktiveSpieler =
            if model.AktiveSpieler = [] then 
                spielerListe
            else model.AktiveSpieler

        let neuerWerIstDran =
            Some
                (indizesAktiverSpieler spielerListe aktiveSpieler
                |> werIstDran anzahlSpieler  (index model.WerIstDranIndex) )

        let neuerZwischenWerIstDran =
            Some
                (indizesAktiverSpieler spielerListe aktiveSpieler
                |> werIstDran anzahlSpieler (index model.WerIstDranIndex ))

        let newModel = {
            model with 
                Decks = decks
                AnzahlSpieler = anzahlSpieler
                AktiveSpieler = aktiveSpieler        
                KartenDerRunde = kartenDerRunde decks anzahlSpieler (indizesAktiverSpieler spielerListe aktiveSpieler)
                RundenGewinner = []
                Vergleichswerte = []
                MöglicheGegnerVergleichswerte = []
                Weitersuchen = false
                WerIstDranIndex = neuerWerIstDran
                ZwischenWerIstDran = neuerZwischenWerIstDran
                Spieler = spielerListe
            }
        newModel,
            if newModel.WerIstDranIndex = Some 0 then
                Cmd.none
            else Cmd.ofMsg GegnerErsterZug
    | GegnerErsterZug ->

        let gegnerKarte = model.KartenDerRunde.[index model.WerIstDranIndex]
        let (_, kpunkte) = gegnerKarte.Krankheit
        let (_, sopunkte) = gegnerKarte.Sonstiges
        let apunkte = gegnerKarte.Alter
        let spunkte = gegnerKarte.Schamgefühl

        let listeWerte = [Krankheit kpunkte; Alter apunkte; Sonstiges sopunkte; Schamgefühl spunkte]

        let vergleichsWert = wählenDerBestenKarte listeWerte

        model, Cmd.ofMsg (Vergleich vergleichsWert)
    | NeuStart ->
        let newModel = {
            model with 
                Decks = []
                AnzahlSpieler = 2
                KartenDerRunde = []
                RundenGewinner = []
                Vergleichswerte = []
                Spieler = []
                WerIstDranIndex = None
                AktiveSpieler = []
            }
        newModel, Cmd.none
    | Vergleich vergleichswert ->

        let vergleichswerte =
            [vergleichswert] @ model.Vergleichswerte

        let zuVergleichendeKarten =
            //wenn es Rundengewinner gibt, dann nur Gewinner vergleichen.. theoretisch würde denke ich auch model.Rundengewinner.Length > 1 funktionieren
            if model.RundenGewinner <> [] then 
                model.KartenDerRunde
                |> List.filter (fun x ->
                    model.RundenGewinner
                    |> List.exists (fun y -> y = x.Spieler))
            else model.KartenDerRunde

        let rundenGewinner =
            if model.RundenGewinner = [] then model.Spieler
            else model.RundenGewinner

        let zwischenGewinner =
            vergleicheKarten zuVergleichendeKarten vergleichswert rundenGewinner (niedrigOderHochGewinnt vergleichswert)

        let zwischenWerIstDran =
            let werIstDran = model.Spieler.[index model.WerIstDranIndex]
            if (model.RundenGewinner |> List.contains werIstDran) || zwischenGewinner.Length = 1 then
                model.WerIstDranIndex
            else
                Some
                    (indizeesZwischenGewinner model.Spieler zwischenGewinner
                    |> wessenKarte (index model.WerIstDranIndex))

        let neuerWerIstDran =
            Some
                (indizesAktiverSpieler model.Spieler model.AktiveSpieler
                |> wessenKarte  (index model.WerIstDranIndex))

        if  //wenn es einen Gewinner gibt oder Spieler am Zug, MOdell zurückgeben
            zwischenGewinner.Length = 1 ||
            zwischenWerIstDran = Some 0
        then
            let neuesModel ={
                model with
                    RundenGewinner = zwischenGewinner
                    Weitersuchen = false
                    Vergleichswerte = vergleichswerte
                    MöglicheGegnerVergleichswerte = vergleichslisteGegner model.MöglicheGegnerVergleichswerte vergleichswerte
                    WerIstDranIndex = neuerWerIstDran
                    ZwischenWerIstDran = zwischenWerIstDran}

            gibtEsEinenVerlierer model neuesModel zwischenGewinner vergleichswerte

        else //zwischenergebnis: mehrere gewinner, als nächstes Computergegner
                //hier eventuell eine Verzögerung 1-2 Sekunden einbauen anstsatt WeitersuchenButton
                    let newModel ={
                        model with
                            RundenGewinner = zwischenGewinner
                            MöglicheGegnerVergleichswerte = vergleichslisteGegner (möglicheVergleichsWerteGegner model zwischenGewinner) vergleichswerte
                            Vergleichswerte = vergleichswerte
                            Weitersuchen = true
                            ZwischenWerIstDran = zwischenWerIstDran
                            }

                    newModel, Cmd.ofMsg WeitersuchenButton

    | SucheFinalenRundenGewinner -> //unter Computergegnern

            let neueVergleichsWerte1 = [wählenDerBestenKarte model.MöglicheGegnerVergleichswerte] @ model.Vergleichswerte

            let neueRundenGewinner =
                vergleicheKarten model.KartenDerRunde neueVergleichsWerte1.[0] model.RundenGewinner (niedrigOderHochGewinnt neueVergleichsWerte1.[0]) //ddd

            let zwischenWerIstDran =
                if neueRundenGewinner.Length = 1 then
                    Some (indizesAktiverSpieler model.Spieler model.AktiveSpieler |> wessenKarte (index model.WerIstDranIndex))
                else Some (indizeesZwischenGewinner model.Spieler neueRundenGewinner |> wessenKarte (index model.WerIstDranIndex))

            let neuerWerIstDran =
                Some (indizesAktiverSpieler model.Spieler model.AktiveSpieler |> wessenKarte (index model.WerIstDranIndex))

            if neueRundenGewinner.Length = 1 then
            
                let neuesModel = {
                    model with
                        RundenGewinner = neueRundenGewinner
                        Vergleichswerte = neueVergleichsWerte1 //nötig?
                        Weitersuchen = false
                        MöglicheGegnerVergleichswerte = vergleichslisteGegner model.MöglicheGegnerVergleichswerte neueVergleichsWerte1 //nötig?
                        WerIstDranIndex = neuerWerIstDran
                        ZwischenWerIstDran = zwischenWerIstDran }
                        
                gibtEsEinenVerlierer model neuesModel neueRundenGewinner neueVergleichsWerte1

            else
                let neueVergleichsWerteGegner =  vergleichslisteGegner model.MöglicheGegnerVergleichswerte neueVergleichsWerte1 

                let neuesModel = {
                    model with
                        RundenGewinner = neueRundenGewinner
                        Vergleichswerte = neueVergleichsWerte1
                        ZwischenWerIstDran = zwischenWerIstDran
                        MöglicheGegnerVergleichswerte = neueVergleichsWerteGegner
                        Weitersuchen = false
                    }
                neuesModel, Cmd.ofMsg WeitersuchenButton
        
    | WeitersuchenButton ->
        let newModel = {
            model with
                Weitersuchen = true
                }
        newModel, Cmd.none
    | NeueRunde ->
        model, Cmd.ofMsg (Ziehen model.AnzahlSpieler)


let view (model : Model) (dispatch : Msg -> unit) =
    div []
        [ Navbar.navbar [ Navbar.Color IsPrimary ]
            [ Navbar.Item.div [ ]
                [ Heading.h2 [ ]
                    [ str "SAFE Template" ] ] ]

          Container.container [ ]
              [
                Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
                    [

                    ////########################################
                    ////############### LOG ####################
                    ////########################################


                        Heading.h3 [] [ str ("Choose number of players: " ) ]
                    //    p [] [str (sprintf "Wer ist dran: %s" (model.WerIstDranIndex.ToString()))]
                    //    let zwischenWerIstDran = 
                    //        match model.ZwischenWerIstDran with
                    //        |None -> 4
                    //        | Some wer -> wer
                    //    p [] [str (sprintf "Zwischen - Wer ist dran: %s" (zwischenWerIstDran.ToString()))]
                    //    p [] [str (sprintf "Aktive SPieler: %s" (model.AktiveSpieler.ToString()))]
                    //    p [] [str (sprintf "Rundengewinner: %s" (model.RundenGewinner.ToString()))]
                    //    p [] [str (sprintf "Mögliche Vergleichswerte: %s" (model.MöglicheGegnerVergleichswerte.ToString()))]
                    //    p [] [str (sprintf "Vergleichswerte %s" (model.Vergleichswerte.ToString()))]

                    //]

                //########################################
                //############### Anzahl der Spieler #####
                //########################################

                        Columns.columns []
                            [
                              for i in 2..4 do  
                              Column.column
                                [
                                    Column.Props [Style [ TextAlign TextAlignOptions.Center ]]
                                ]
                                [   
                                    Button.button
                                        [
                                            Button.OnClick (fun _ ->
                                                dispatch NeuStart//in zwei verschiedne Buttons teilen, ist nur so, damit ich schneller resetten kann
                                                dispatch (Ziehen i))
                                            Button.Props [Style [ FontSize "20px"; FontWeight "bold"; Width "200px"]]
                                    
                                        ]
                                        [str (sprintf "%i" i)]   
                                ]
                            ]


                        //########################################
                        //######## Infos #########################
                        //########################################

                        Columns.columns []
                            [
                                Column.column []
                                    [
                                        p []
                                            [//wer ist dran
                                                if model.ZwischenWerIstDran = Some 0 then //|| model.WerIstDranIndex  = Some 0 
                                                    str "Du bist an der Reihe"
                                                elif model.ZwischenWerIstDran = Some 1 then //|| model.WerIstDranIndex  = Some 1 
                                                    str "Gegner 1 ist an der Reihe"
                                                elif model.ZwischenWerIstDran = Some 2 then//|| model.WerIstDranIndex  = Some 2 
                                                    str "Gegner 2 ist an der Reihe"
                                                elif model.ZwischenWerIstDran = Some 3  then //|| model.WerIstDranIndex  = Some 3
                                                    str "Gegner 3 ist an der Reihe"
                                                else
                                                    str ""
                                            ]
                                        p []
                                            [//Infotexte
                                                //unbedingt nochmal Texte prüfen, ob so auch korrekt und ob, welche eingefügt werden müssen

                                                if model.Vergleichswerte =[] && model.KartenDerRunde = [] then
                                                    str "Wähle Anzahl der Spieler"
                                                elif model.Vergleichswerte = [] then
                                                    str "Wähle einen Wert zum Vergleich"
                                                elif model.RundenGewinner.Length > 1 && model.RundenGewinner |> List.contains Spieler1 && model.WerIstDranIndex = Some 0 then
                                                    str "Wähle einen anderen Wert zum Vergleich"
                                                elif model.AktiveSpieler.Length = 1 then
                                                    str (sprintf "%s hat das Spiel gewonnen" (model.AktiveSpieler.Head.ToString()))
                                                elif model.RundenGewinner.Length = 1 then
                                                    str
                                                        (
                                                            sprintf "%s ist Gewinner der Runde %s"
                                                                (model.RundenGewinner.Head.ToString())

                                                                (
                                                                    if model.Decks |> List.exists (fun x -> x.Length = 1)
                                                                    then
                                                                        let idxVerlierer = model.Decks |> List.indexed |> List.filter (fun (i,x) -> x.Length = 1) |> List.map (fun (i,x) -> i)
                                                                        let verlierer =
                                                                                    model.Spieler
                                                                                    |> List.indexed
                                                                                    |> List.filter (fun (i,x) -> idxVerlierer |> List.contains i)
                                                                                    |> List.map (fun (i,x) -> x)

                                                                        match idxVerlierer.Length with
                                                                        | 1 -> (sprintf "%s hat verloren" (model.Spieler.[idxVerlierer |> List.exactlyOne].ToString()))
                                                                        | 2 -> (sprintf "%s und %s hat verloren" (verlierer.[0].ToString()) (verlierer.[1].ToString()))
                                                                        | 3 -> (sprintf "%s, %s und %s hat verloren" (verlierer.[0].ToString()) (verlierer.[1].ToString()) (verlierer.[2].ToString()))
                                                                    
                                                                        | _ -> ""
                                                                
                                                                    else ""
                                                                )
                                                        )
                                                elif model.RundenGewinner |> List.contains Spieler1 && model.WerIstDranIndex = Some 3 then
                                                    str "Wähle einen anderen Wert zum Vergleich"
                                        
                                                else
                                                    str ""
                                            ]
                                    ]
                            ]

                        //########################################
                        //############### Buttons ################
                        //########################################


                        if model.RundenGewinner.Length = 1 && model.AktiveSpieler.Length > 1 then
                            Columns.columns []
                                [
                                    Column.column [Column.Props [Style [ TextAlign TextAlignOptions.Center ]]]
                                        [
                                            Button.button
                                                [
                                                    Button.OnClick (fun _ -> dispatch NeueRunde)
                                                    Button.Props [Style [ FontSize "30px"; FontWeight "bold"]]
                                                ]
                                                [str "Nächste Runde"] 
                                        ]
                                ]
                    

                        if model.Weitersuchen = true then// kommt später dann weg
                            Columns.columns []
                                [
                                    Column.column [Column.Props [Style [ TextAlign TextAlignOptions.Center ]]]
                                        [
                                            Button.button
                                                [
                                                    Button.OnClick (fun _ -> dispatch SucheFinalenRundenGewinner)
                                                    Button.Props [Style [ FontSize "30px"; FontWeight "bold"]]
                                                ]
                                                [str "Weitersuchen"] 
                                        ]
                                ]



                        //###########################################################################################
                        //################################ Spielerkarte #############################################
                        //###########################################################################################


                        if model.KartenDerRunde <> [] then

                            let istDran = model.ZwischenWerIstDran = Some 0 
                            //let istDran = (model.WerIstDranIndex = 0 && (model.ZwischenWerIstDran = None || model.ZwischenWerIstDran = Some 0 )) || (model.RundenGewinner.Length > 1 && model.ZwischenWerIstDran = Some 0)
                            containerSpieler model.RundenGewinner model.AktiveSpieler Spieler1 istDran
                                [
                                    nameStatusAnzahlKarten model.AktiveSpieler Spieler1 model.KartenDerRunde.[0].Name model.Decks.[0].Length 

                                    //Werte der Karten

                                    for j in 0..3 do
                                        Columns.columns []
                                            [
                                                Column.column []
                                                    [
                                                        let (sonstiges, krankheit, eigenschaften, vergleichsWerte) = helperVergleiche 0 model.KartenDerRunde      
                                                        a
                                                            [
                                                                if
                                                                    ((model.WerIstDranIndex = Some 0 && model.ZwischenWerIstDran = None) || model.ZwischenWerIstDran = Some 0) &&
                                                                    (
                                                                        (model.RundenGewinner |> List.contains Spieler1 && model.RundenGewinner.Length > 1) ||   
                                                                        (
                                                                            model.RundenGewinner.Length = 0 &&
                                                                            not (schonVerglichen model.Vergleichswerte (vergleichsWerte.[j] eigenschaften.[j]))
                                                                        )
                                                                    )
                                                                then
                                                                    OnClick (fun _ -> dispatch (Vergleich (vergleichsWerte.[j] eigenschaften.[j])))

                                                                Style
                                                                    [
                                                                        Color "#000000"

                                                                        if model.Vergleichswerte.Length >=1 then

                                                                            if aktuellerVergleichswert model.Vergleichswerte.[0] (vergleichsWerte.[j] eigenschaften.[j]) &&
                                                                                model.RundenGewinner |> List.contains model.KartenDerRunde.[0].Spieler
                                                                            then 
                                                                                BackgroundColor "#00ff00" //hellgrün = wert hat gewonnen
                                                                            elif schonVerglichen model.Vergleichswerte (vergleichsWerte.[j] eigenschaften.[j])
                                                                            then
                                                                                BackgroundColor "#A07748" //beige = schon verglichener Wert
                                   
                                                                            if
                                                                                model.RundenGewinner.Length = 1 ||
                                                                                schonVerglichen model.Vergleichswerte (vergleichsWerte.[j] eigenschaften.[j]) ||
                                                                                model.RundenGewinner |> List.contains Spieler1 |> not ||
                                                                                model.ZwischenWerIstDran <> Some 0
                                                                            then
                                                                                Cursor "default"
                                                                    ]
                                                            ]
                                                            [contentKarten j 0 krankheit sonstiges eigenschaften vergleichsWerte model]
                                                    ]
                                            ]
                                ]


                        //###########################################################################################
                        //################################ Gegnerkarten #############################################
                        //###########################################################################################

                        //nicht in der Mitte

                        if model.KartenDerRunde <> [] then
                            Columns.columns []
                                [
                                    for i in 1..model.AnzahlSpieler-1 do

                                        let istDran = model.ZwischenWerIstDran = Some i
                                        //let istDran = (model.WerIstDranIndex = i && (model.ZwischenWerIstDran = None || model.ZwischenWerIstDran = Some i )) || (model.RundenGewinner.Length > 1 && model.ZwischenWerIstDran = Some i)
                                        containerGegner i model.RundenGewinner model.AktiveSpieler model.KartenDerRunde.[i].Spieler istDran
                                            [
                                                nameStatusAnzahlKarten model.AktiveSpieler model.Spieler.[i] model.KartenDerRunde.[i].Name model.Decks.[i].Length 

                                                //Werte der Karten

                                                for j in 0..3 do
                                                    Columns.columns []
                                                        [
                                                            Column.column []
                                                                [
                                                                    let (sonstiges, krankheit, eigenschaften, vergleichsWerte) = helperVergleiche i model.KartenDerRunde
                                                                    p
                                                                        [  Style
                                                                            [
                                                                                if model.Vergleichswerte.Length >=1 then
                                                                                    if
                                                                                        aktuellerVergleichswert model.Vergleichswerte.[0] (vergleichsWerte.[j] eigenschaften.[j]) &&
                                                                                        model.RundenGewinner |> List.contains model.KartenDerRunde.[i].Spieler then
                                                                                        BackgroundColor "#00ff00"//hellgrün = wert hat gewonnen

                                                                                    elif schonVerglichen model.Vergleichswerte (vergleichsWerte.[j] eigenschaften.[j])  then
                                                                                        BackgroundColor "#A07748" //braun = schon verglichen
                                                                            ]
                                                                        ]
                                                                        [contentKarten j i krankheit sonstiges eigenschaften vergleichsWerte model]
                                                                ]
                                                        ]
                                            ]
                                    ]
                    ]
              ]

          ]




#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactBatched "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
