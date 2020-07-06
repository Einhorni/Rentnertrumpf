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

    WerIstDranIndex: int //sollte ich eigentlich auch als Option machen, anstatt Ausweichwert 4, wenn keiner dran ist
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
        WerIstDranIndex = 4
        ZwischenWerIstDran = None

        Weitersuchen = false
        }
    
    initialModel, Cmd.none


let indizeesZwischenGewinner model zwischenGewinner = 
    model.Spieler
    |> List.indexed
    |> List.filter (fun (i,x) ->
        zwischenGewinner
        |> List.exists (fun y -> y=x)
        )
    |> List.map (fun (i,x) -> i)


let möglicheVergleichsWerteGegner (model:Model) (zwischenGewinner:Spieler list)  =
    let neuerWErIstDran =
        indizeesZwischenGewinner model zwischenGewinner
        |> wessenKarte  model.WerIstDranIndex

    let (kh, kpunkte) = model.KartenDerRunde.[neuerWErIstDran].Krankheit
    let (so, spunkte) = model.KartenDerRunde.[neuerWErIstDran].Sonstiges
    
    [Krankheit kpunkte; Alter model.KartenDerRunde.[neuerWErIstDran].Alter; Sonstiges spunkte; Schamgefühl model.KartenDerRunde.[neuerWErIstDran].Schamgefühl]


let gibtEsEinenVerlierer model neuesModelMitVerlierer zwischenGewinner vergleichswerte = 

    let deckMitNurEinerKarteVorhanden:Decks = 
        model.Decks
        //|> List.truncate model.AnzahlSpieler //um die leeren FakeDecks auszusortieren // eigentlich unnötig... sind ja leer und nicht 1 lang
        |> List.filter (fun x ->
            x.Length = 1)

    if deckMitNurEinerKarteVorhanden.Length = 1 then

        let spielerMitNurEinerKarte =
            deckMitNurEinerKarteVorhanden
            |> List.collect (fun x ->
                x
                |> List.map (fun  y -> y.Spieler)
                )
            |> List.head

        if
            zwischenGewinner
            |> List.contains spielerMitNurEinerKarte
        then //Deck mit nur einer Karte hat gewonnen = keiner fliegt raus
            neuesModelMitVerlierer, Cmd.none
        else //es bleiben mehrere Spieler übrig = spiel geht weiter, ein SPieler weniger 
            let aktiveSpielerOhneVerlierer =
                model.AktiveSpieler
                |> List.filter (fun x -> x <> spielerMitNurEinerKarte)

            let neuesModelOhneVerlierer = {
                model with
                    AktiveSpieler = aktiveSpielerOhneVerlierer
                    RundenGewinner = zwischenGewinner
                    Weitersuchen = false
                    MöglicheGegnerVergleichswerte = []
                    Vergleichswerte = vergleichswerte
                    WerIstDranIndex = (indizeesZwischenGewinner model zwischenGewinner) |> wessenKarte  model.WerIstDranIndex }
            neuesModelOhneVerlierer, Cmd.none

    //keiner hat nur noch eine Karte = keiner fliegt raus
    else neuesModelMitVerlierer, Cmd.none



let update (msg : Msg) (model : Model) : Model * Cmd<Msg> =
    match msg with
    | Ziehen anzahlSpieler ->

        let spielerListe = spieler.[0..anzahlSpieler-1 ]

        let decks =
            if model.KartenDerRunde = [] then
                Logik.kartenDecks Logik.kartenSet anzahlSpieler
            else changeDecks model.Decks model.RundenGewinner.[0] model.KartenDerRunde


        let aktiveSpieler =
            if model.AktiveSpieler = [] then 
                spielerListe
            else model.AktiveSpieler

        let newModel = {
            model with 
                Decks = decks
                AnzahlSpieler = anzahlSpieler
                AktiveSpieler = aktiveSpieler        
                KartenDerRunde = Logik.kartenDerRunde decks anzahlSpieler (indizesAktiverSpieler spielerListe aktiveSpieler)
                RundenGewinner = []
                Vergleichswerte = []
                MöglicheGegnerVergleichswerte = []
                Weitersuchen = false
                WerIstDranIndex = werIstDran anzahlSpieler model.WerIstDranIndex (indizesAktiverSpieler spielerListe aktiveSpieler)
                ZwischenWerIstDran = None
                Spieler = spielerListe
            }
        newModel,
            if newModel.WerIstDranIndex = 0 then
                Cmd.none
            else Cmd.ofMsg GegnerErsterZug
    | GegnerErsterZug ->

        let gegnerKarte = model.KartenDerRunde.[model.WerIstDranIndex]
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
                WerIstDranIndex = 4
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
            let werIstDran = model.Spieler.[model.WerIstDranIndex]
            if model.RundenGewinner |> List.contains werIstDran then
                model.WerIstDranIndex
            else wessenKarte model.WerIstDranIndex (indizeesZwischenGewinner model zwischenGewinner)

        if  //wenn es einen Gewinner gibt oder Spieler am Zug, MOdell zurückgeben
            zwischenGewinner.Length = 1 ||
            zwischenWerIstDran = 0
        then
            let neuesModel ={
                model with
                    RundenGewinner = zwischenGewinner
                    Weitersuchen = false
                    Vergleichswerte = vergleichswerte
                    MöglicheGegnerVergleichswerte = vergleichslisteGegner model.MöglicheGegnerVergleichswerte vergleichswerte
                    WerIstDranIndex = indizesAktiverSpieler model.Spieler model.AktiveSpieler |> wessenKarte  model.WerIstDranIndex
                    ZwischenWerIstDran = Some zwischenWerIstDran}

            gibtEsEinenVerlierer model neuesModel zwischenGewinner vergleichswerte

        else //zwischenergebnis: mehrere gewinner, als nächstes Computergegner
                //hier eventuell eine Verzögerung 1-2 Sekunden einbauen anstsatt WeitersuchenButton
                    let newModel ={
                        model with
                            RundenGewinner = zwischenGewinner
                            MöglicheGegnerVergleichswerte = vergleichslisteGegner (möglicheVergleichsWerteGegner model zwischenGewinner) vergleichswerte
                            Vergleichswerte = vergleichswerte
                            Weitersuchen = true
                            ZwischenWerIstDran = Some zwischenWerIstDran
                            }

                    newModel, Cmd.ofMsg WeitersuchenButton

    | SucheFinalenRundenGewinner -> //unter Computergegnern

            let neueVergleichsWerte1 = [wählenDerBestenKarte model.MöglicheGegnerVergleichswerte] @ model.Vergleichswerte

            let neueRundenGewinner =
                vergleicheKarten model.KartenDerRunde neueVergleichsWerte1.[0] model.RundenGewinner (niedrigOderHochGewinnt neueVergleichsWerte1.[0]) //ddd

            if neueRundenGewinner.Length = 1 then
            
                let neuesModel = {
                    model with
                        RundenGewinner = neueRundenGewinner
                        Vergleichswerte = neueVergleichsWerte1 //nötig?
                        Weitersuchen = false
                        MöglicheGegnerVergleichswerte = vergleichslisteGegner model.MöglicheGegnerVergleichswerte neueVergleichsWerte1 //nötig?
                        WerIstDranIndex =  indizesAktiverSpieler model.Spieler model.AktiveSpieler |> wessenKarte model.WerIstDranIndex  
                        ZwischenWerIstDran = None}
                        
                gibtEsEinenVerlierer model neuesModel neueRundenGewinner neueVergleichsWerte1

            else
                let zwischenWerIstDran = wessenKarte model.WerIstDranIndex (indizeesZwischenGewinner model neueRundenGewinner)             
                let neueVergleichsWerteGegner =  vergleichslisteGegner model.MöglicheGegnerVergleichswerte neueVergleichsWerte1 

                let neuesModel = {
                    model with
                        RundenGewinner = neueRundenGewinner
                        Vergleichswerte = neueVergleichsWerte1
                        ZwischenWerIstDran = Some zwischenWerIstDran
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
              [ Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
                    [
                        Heading.h3 [] [ str ("Choose number of players: " ) ]
                        //p [] [str (sprintf "Wer ist dran: %s" (model.WerIstDranIndex.ToString()))]
                        //let zwischenWerIstDran = 
                        //    match model.ZwischenWerIstDran with
                        //    |None -> 4
                        //    | Some wer -> wer
                        //p [] [str (sprintf "Zwischen - Wer ist dran: %s" (zwischenWerIstDran.ToString()))]
                        //p [] [str (sprintf "Aktive SPieler: %s" (model.AktiveSpieler.ToString()))]
                        //p [] [str (sprintf "Rundengewinner: %s" (model.RundenGewinner.ToString()))]
                        //p [] [str (sprintf "Mögliche Vergleichswerte: %s" (model.MöglicheGegnerVergleichswerte.ToString()))]
                        //p [] [str (sprintf "Vergleichswerte %s" (model.Vergleichswerte.ToString()))]

                    ]
                    
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

                Columns.columns []
                    [
                        Column.column []
                            [
                                p []
                                    [//Anzeige, wer dran ist
                                        if model.ZwischenWerIstDran = Some 0 || model.WerIstDranIndex  = 0 then
                                            str "Du bist an der Reihe"
                                        elif model.ZwischenWerIstDran = Some 1 || model.WerIstDranIndex  = 1 then
                                            str "Gegner 1 ist an der Reihe"
                                        elif model.ZwischenWerIstDran = Some 2 || model.WerIstDranIndex  = 2 then
                                            str "Gegner 2 ist an der Reihe"
                                        elif model.ZwischenWerIstDran = Some 3 || model.WerIstDranIndex  = 3 then
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
                                        elif model.RundenGewinner.Length > 1 && model.RundenGewinner |> List.contains Spieler1 && model.WerIstDranIndex = 0 then
                                            str "Wähle einen anderen Wert zum Vergleich"
                                        elif model.AktiveSpieler.Length = 1 then
                                            str (sprintf "%s hat das Spiel gewonnen" (model.AktiveSpieler.Head.ToString()))
                                        elif model.RundenGewinner.Length = 1 then
                                            str (sprintf "%s ist Gewinner der Runde" (model.RundenGewinner.Head.ToString()))
                                        elif model.RundenGewinner |> List.contains Spieler1 && model.WerIstDranIndex = 3 then
                                            str "Wähle einen anderen Wert zum Vergleich"
                                        
                                        else
                                            str ""
                                    ]
                            ]
                    ]

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
                                        [str "Nächste Runde"] //sagen wer dran ist??
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
                    div [ Style [CSSProp.Width "100%" ]  ]
                        [
                            
                            Columns.columns []
                                [
                                    Column.column []
                                        [
                                        Box.box'
                                                [
                                                    Common.Props [Style
                                                        [

                                                            Width "500px"
                                                            Margin "Auto"
                                                            if model.RundenGewinner |> List.contains Spieler1 then
                                                                BackgroundColor "#2Fb660" //grün = spieler unter gewinnern
                                                            elif model.AktiveSpieler |> List.contains Spieler1 |> not then
                                                                BackgroundColor "#BDBDBD" //grau = spieler draußen
                                                            else BackgroundColor "#FFb660" //orange = spieler im Spiel
                                                        ]]
                                                ]
                                                [
                                                    //img [Src "images\Unbenannt.jpg"]
                                                    if model.AktiveSpieler |> List.contains Spieler1 |> not then
                                                        Columns.columns []
                                                            [
                                                                p [  Style [ CSSProp.FontSize "25px"] ][str (sprintf "Ausgeschieden")]
                                                            ]
                                                    elif model.AktiveSpieler.Length = 1 && model.AktiveSpieler.Head = Spieler1 then
                                                        Columns.columns []
                                                            [
                                                                p [  Style [ CSSProp.FontSize "25px"] ][str (sprintf "Gewinner")]
                                                            ]

                                                    else
                                                            

                                                        Columns.columns []
                                                            [
                                                                Column.column []
                                                                    [
                                                                        p [  Style [ CSSProp.FontSize "25px"] ][str (sprintf "%s" model.KartenDerRunde.[0].Name)]
                                                                    ]
                                                            ]


                                                        //Werte der Karten

                                                        for j in 0..3 do
                                                            Columns.columns []
                                                                [
                                                                    Column.column []
                                                                        [
                                                                            let schamPunkte = model.KartenDerRunde.[0].Schamgefühl
                                                                            let (sonstiges, sonstigesPunkte) = model.KartenDerRunde.[0].Sonstiges
                                                                            let alterPunkte = model.KartenDerRunde.[0].Alter
                                                                            let (krankheit, krankheitPunkte) = model.KartenDerRunde.[0].Krankheit
                                                                            let eigenschaften = [krankheitPunkte; alterPunkte; sonstigesPunkte; schamPunkte]
                                                                            let vergleichsWerte = [Krankheit; Alter; Sonstiges; Schamgefühl]
                                                                            Button.button
                                                                                [
                                                                                    Button.OnClick (fun _ -> dispatch (Vergleich (vergleichsWerte.[j] eigenschaften.[j])))
                                                                                    
                                                                                    if model.Vergleichswerte.Length >=1 then
                                                                                        if model.Vergleichswerte.[0] = (vergleichsWerte.[j] eigenschaften.[j]) && model.RundenGewinner |> List.contains model.KartenDerRunde.[0].Spieler then 
                                                                                            Button.Props [Style [BackgroundColor "#00ff00"]] //hellgrün = wert hat gewonnen
                                                                                        elif model.Vergleichswerte(**.Tail**) |> List.contains (vergleichsWerte.[j] eigenschaften.[j]) then // schon verglichene Werte
                                                                                            Button.Props [Style [ BackgroundColor "#EDB372"]] //orange / beige = schon verglichener Wert


                                                                                    let schonVerglichen =
                                                                                            

                                                                                        model.Vergleichswerte
                                                                                        |> List.exists (fun x ->
                                                                                            match x, (vergleichsWerte.[j] eigenschaften.[j]) with
                                                                                            | Krankheit _ , Krankheit _ -> true
                                                                                            | Alter _ , Alter _ -> true
                                                                                            | Sonstiges _ , Sonstiges _ -> true
                                                                                            | Schamgefühl _ , Schamgefühl _ -> true
                                                                                            | _ -> false
                                                                                            )

                                                                                    Button.Disabled
                                                                                        (
                                                                                        model.RundenGewinner.Length = 1 ||
                                                                                        model.KartenDerRunde.[0].Spieler <> Spieler1 ||
                                                                                        schonVerglichen ||
                                                                                        (model.WerIstDranIndex <> 0 && model.ZwischenWerIstDran <> Some 0)
                                                                                        )
                                                                                ]
                                                                                [
                                                                                    let punkte = String.init eigenschaften.[j] (fun x -> "+") 
                                                                                    str
                                                                                        (match j with
                                                                                        | 0 -> (sprintf "Krankheit: %s %s" krankheit punkte)
                                                                                        | 1 -> (sprintf "Alter: %i" model.KartenDerRunde.[0].Alter)
                                                                                        | 2 -> (sprintf "Sonstiges: %s %s" sonstiges punkte)
                                                                                        | 3 -> (sprintf "Schamgefühl: %i" model.KartenDerRunde.[0].Schamgefühl)
                                                                                        | _ -> "")
                                                                                            
                                                                                ]

                                                                        ]
                                                                ]

                                                                                                        
                                                ]

                                    ]
                                ]
                        ]
               

               

                //###########################################################################################
                //################################ Gegnerkarten #############################################
                //###########################################################################################


                Columns.columns []
                    [
                        for i in 2..model.AnzahlSpieler do
                        Column.column []
                            
                            [
                                Tile.ancestor []
                                    [ Tile.parent []
                                        [ Tile.child [ ]
                                            [
                                              Box.box' [ ]
                                                [
                                                    Heading.p []
                                                        [
                                                            if i = 1 then
                                                                str "Spieler"
                                                            else
                                                                str (sprintf "Gegner %i" (i-1))
                                                        ]
                                                ]




                                              //die vollständigen Karten der Gegner sind natürlich nicht einsehbar
                                              if model.KartenDerRunde <> [] then
                                                    Box.box'
                                                        [ 
                                                            if model.RundenGewinner |> List.contains model.KartenDerRunde.[i-1].Spieler then
                                                                Common.Props [Style [ BackgroundColor "#2Fb660"; Width "350px"]] //grün = spieler unter gewinnern
                                                            elif model.AktiveSpieler |> List.contains model.Spieler.[i-1] |> not then
                                                                Common.Props [Style [ BackgroundColor "#BDBDBD"; Width "350px"]] //grau = spieler draußen
                                                            else
                                                                Common.Props [Style [ BackgroundColor "#FFb660"; Width "350px"]] //orange = spieler im Spiel
                                                        ]
                                                        [
                                                            //img [Src "images\Unbenannt.jpg"]
                                                            if model.AktiveSpieler |> List.contains model.Spieler.[i-1] |> not then
                                                                Columns.columns []
                                                                    [
                                                                        p [  Style [ CSSProp.FontSize "25px"] ][str (sprintf "Ausgeschieden")]
                                                                    ]
                                                            elif model.AktiveSpieler.Length = 1 && model.AktiveSpieler.Head = model.Spieler.[i-1] then
                                                                Columns.columns []
                                                                    [
                                                                        p [  Style [ CSSProp.FontSize "25px"] ][str (sprintf "Gewinner")]
                                                                    ]

                                                            else
                                                            

                                                                Columns.columns []
                                                                    [
                                                                        Column.column []
                                                                            [
                                                                                p [  Style [ CSSProp.FontSize "25px"] ][str (sprintf "%s" model.KartenDerRunde.[i-1].Name)]
                                                                            ]
                                                                    ]


                                                                //Werte der Karten

                                                                for j in 0..3 do
                                                                    Columns.columns []
                                                                        [
                                                                            Column.column []
                                                                                [
                                                                                    let schamPunkte = model.KartenDerRunde.[i-1].Schamgefühl
                                                                                    let (sonstiges, sonstigesPunkte) = model.KartenDerRunde.[i-1].Sonstiges
                                                                                    let alterPunkte = model.KartenDerRunde.[i-1].Alter
                                                                                    let (krankheit, krankheitPunkte) = model.KartenDerRunde.[i-1].Krankheit
                                                                                    let eigenschaften = [krankheitPunkte; alterPunkte; sonstigesPunkte; schamPunkte]
                                                                                    let vergleichsWerte = [Krankheit; Alter; Sonstiges; Schamgefühl]
                                                                                    Button.button
                                                                                        [
                                                                                            Button.OnClick (fun _ -> dispatch (Vergleich (vergleichsWerte.[j] eigenschaften.[j])))
                                                                                    
                                                                                            if model.Vergleichswerte.Length >=1 then
                                                                                                if model.Vergleichswerte.[0] = (vergleichsWerte.[j] eigenschaften.[j]) && model.RundenGewinner |> List.contains model.KartenDerRunde.[i-1].Spieler then //Button = aktueller Vergleichswert & Spieler ein Rundengewinner
                                                                                                    Button.Props [Style [BackgroundColor "#00ff00"]] //hellgrün = wert hat gewonnen
                                                                                                elif model.Vergleichswerte(**.Tail**) |> List.contains (vergleichsWerte.[j] eigenschaften.[j]) then // schon verglichene Werte
                                                                                                    Button.Props [Style [ BackgroundColor "#EDB372"]] //orange / beige = schon verglichener Wert


                                                                                            let schonVerglichen =
                                                                                            

                                                                                                model.Vergleichswerte
                                                                                                |> List.exists (fun x ->
                                                                                                    match x, (vergleichsWerte.[j] eigenschaften.[j]) with
                                                                                                    | Krankheit _ , Krankheit _ -> true
                                                                                                    | Alter _ , Alter _ -> true
                                                                                                    | Sonstiges _ , Sonstiges _ -> true
                                                                                                    | Schamgefühl _ , Schamgefühl _ -> true
                                                                                                    | _ -> false
                                                                                                    )

                                                                                            Button.Disabled
                                                                                                (
                                                                                                model.RundenGewinner.Length = 1 ||
                                                                                                model.KartenDerRunde.[i-1].Spieler <> Spieler1 ||
                                                                                                schonVerglichen ||
                                                                                                (model.WerIstDranIndex <> 0 && model.ZwischenWerIstDran <> Some 0)
                                                                                                )
                                                                                        ]
                                                                                        [
                                                                                            let punkte = String.init eigenschaften.[j] (fun x -> "+") 
                                                                                            str
                                                                                                (match j with
                                                                                                | 0 -> (sprintf "Krankheit: %s %s" krankheit punkte)
                                                                                                | 1 -> (sprintf "Alter: %i" model.KartenDerRunde.[i-1].Alter)
                                                                                                | 2 -> (sprintf "Sonstiges: %s %s" sonstiges punkte)
                                                                                                | 3 -> (sprintf "Schamgefühl: %i" model.KartenDerRunde.[i-1].Schamgefühl)
                                                                                                | _ -> "")
                                                                                            
                                                                                        ]

                                                                                ]
                                                                        ]
                                                                    
                                                        ]


                                              if model.Decks <> [] then//kommt auch weg
                                                  yield! model.Decks.[i-1]
                                                  |> List.map (fun x -> 
                                                        
                                                    Box.box' []
                                                        [
                                                            p [][str (sprintf "Name: %s" x.Name)]
                                                            p [][
                                                                let (krankheit, kPunkte) = x.Krankheit
                                                                let punkte = ([for i in 1 .. kPunkte do "+"] |> List.reduce (fun x y -> x+y))
                                                                str (sprintf "Krankheit: %s %s" krankheit punkte)
                                                                ]
                                                            p [][
                                                                str (sprintf "Alter: %i" x.Alter)
                                                                ]
                                                            p [][
                                                                let (sonstiges, sPunkte) = x.Sonstiges
                                                                let punkte = ([for i in 1 .. sPunkte do "+"] |> List.reduce (fun x y -> x+y))
                                                                str (sprintf "Sonstiges: %s %s" sonstiges punkte)
                                                                ]
                                                            p [][str (sprintf "Schamgefühl: %i" x.Schamgefühl)]
                                                            p [][str (sprintf "Spieler: %s" (model.KartenDerRunde.[i-1].Spieler.ToString()))]
                                                        ]
                                                )
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
