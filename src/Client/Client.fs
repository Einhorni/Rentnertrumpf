module Client

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Fetch.Types
open Thoth.Fetch
open Fulma
open Thoth.Json
open Shared
open Logik


let mutable debugCounter = 0


type Model = {

    Decks: Decks 
    AnzahlSpieler: int

    AktiveSpieler: Spieler list

    KartenDerRunde: KartenDerRunde
    RundenGewinner: Spieler list
    Vergleichswerte: Vergleichswert list

    Spieler: Spieler list
    WerIstDranIndex: int
    ZwischenWerIstDran: int option

    ZwischengewinnerKarten: Karte list
    MöglicheGegnerVergleichswerte: Vergleichswert list
    test: int list list
    Weitersuchen: bool
    }

type Msg =
    | Ziehen of int
    | GegnerErsterZug
    | NeuStart
    | Vergleich of Vergleichswert
    | NeueRunde
    | SucheFinalenRundenGewinner
    | WeitersuchenButton


let init () : Model * Cmd<Msg> =
    let initialModel = {
        Decks = []
        AnzahlSpieler = 0
        AktiveSpieler = []
        KartenDerRunde = []
        RundenGewinner = []
        Vergleichswerte = []

        Spieler = []
        WerIstDranIndex = 4
        ZwischenWerIstDran = None

        ZwischengewinnerKarten = []
        MöglicheGegnerVergleichswerte = []
        test =[[]]
        Weitersuchen = false
        }
    
    initialModel, Cmd.none


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

let indizeesZwischenGewinner model zwischenGewinner = 
    model.Spieler
    |> List.indexed
    |> List.filter (fun (i,x) ->
        zwischenGewinner
        |> List.exists (fun y -> y=x)
        )
    |> List.map (fun (i,x) -> i)


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


let möglicheVergleichsWerteGegner (model:Model) (zwischenGewinner:Spieler list)  =
    let neuerWErIstDran =
        indizeesZwischenGewinner model zwischenGewinner
        |> wessenKarte  model.WerIstDranIndex


    let (kh, kpunkte) = model.KartenDerRunde.[neuerWErIstDran].Krankheit
    let (so, spunkte) = model.KartenDerRunde.[neuerWErIstDran].Sonstiges
    
    [Krankheit kpunkte; Alter model.KartenDerRunde.[neuerWErIstDran].Alter; Sonstiges spunkte; Schamgefühl model.KartenDerRunde.[neuerWErIstDran].Schamgefühl]

//nur noch die WErte, die bisher noch nicht verglichen wurden vergleichen
let vergleichslisteGegner1 möglicheVergleichswerteGegner bereitsGespielteVergleichswerte =
    möglicheVergleichswerteGegner
    |> List.filter (fun x ->
        bereitsGespielteVergleichswerte
        |> List.exists (fun y ->
            match x, y with
            | Krankheit _ , Krankheit _ ->
                //Browser.Dom.console.log(x.ToString() + "<>" + y.ToString() + "=" + (x <> y).ToString())
                true
            | Alter _ , Alter _ ->
                //Browser.Dom.console.log(x.ToString() + "<>" + y.ToString() + "=" + (x <> y).ToString())
                true
            | Sonstiges _ , Sonstiges _ ->
                //Browser.Dom.console.log(x.ToString() + "<>" + y.ToString() + "=" + (x <> y).ToString())
                true
            | Schamgefühl _ , Schamgefühl _ ->
                //Browser.Dom.console.log(x.ToString() + "<>" + y.ToString() + "=" + (x <> y).ToString())
                true
            | _ -> false
            )
        |> not
        )

let gibtEsEinenVerlierer model neuesModelMitVerlierer zwischenGewinner vergleichswerte = 

    let nurNochEineKarteImDeck =
        model.Decks
        |> List.truncate model.AnzahlSpieler //um die leeren FakeDecks auszusortieren // leere Decks, die einem Spieler gehören würden drin bleiben
        |> List.tryFindIndex (fun x ->
            x.Length = 1)

    match nurNochEineKarteImDeck with
    | Some idx ->
        if
            zwischenGewinner
            |> List.contains model.KartenDerRunde.[idx].Spieler
        then //Deck mit nur einer Karte hat gewonnen --> normal weitermachen
            neuesModelMitVerlierer, Cmd.none
        else

            let aktiveSpielerOhneVerlierer = //Deck mit nur einer KArte (idx) hat verloren --> wird aus aktiven SPielern herausgenommen
                model.AktiveSpieler
                |> List.indexed
                |> List.filter (fun (i,x) -> i <> idx)
                |> List.map (fun (i,x) -> x)

            let neuesModelOhneVerlierer = {
                model with
                    AktiveSpieler = aktiveSpielerOhneVerlierer
                    RundenGewinner = zwischenGewinner
                    Weitersuchen = false
                    MöglicheGegnerVergleichswerte = []
                    Vergleichswerte = vergleichswerte
                    WerIstDranIndex = (indizeesZwischenGewinner model zwischenGewinner) |> wessenKarte  model.WerIstDranIndex }

            neuesModelOhneVerlierer, Cmd.none
    | None -> //normal weitermachen
        neuesModelMitVerlierer, Cmd.none


let indizesAktiverSpieler spielerListe aktiveSpieler =
    spielerListe
    |> List.indexed
    |> List.filter (fun (i,spieler) ->
        aktiveSpieler
        |> List.exists (fun aktiveSpieler ->
            //Browser.Dom.console.log("spieler und aktive Spieler")
            //Browser.Dom.console.log(spieler.ToString())
            //Browser.Dom.console.log(aktiveSpieler.ToString())
            //Browser.Dom.console.log((aktiveSpieler.ToString() = spieler.ToString()))
            //Fable /JS hat ein Problem mit Vergleich von Disc Unions, sporadisch: false bei gleichen DIngernCases, deswegen ein To String Vergleich
            aktiveSpieler.ToString() = spieler.ToString())
        )
    |> List.map (fun (i,x) -> i)



let spieler =[Spieler1; Gegner1; Gegner2; Gegner3]

let update (msg : Msg) (model : Model) : Model * Cmd<Msg> =
    match msg with
    | Ziehen anzahlSpieler ->

        let spielerListe = spieler.[0..anzahlSpieler-1 ]

        Browser.Dom.console.log(model.KartenDerRunde.ToString())

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
                ZwischengewinnerKarten = []
                Weitersuchen = false
                WerIstDranIndex = werIstDran anzahlSpieler model.WerIstDranIndex (indizesAktiverSpieler spielerListe aktiveSpieler)
                ZwischenWerIstDran = None
                Spieler = spielerListe
                test = [[]]
            }


        newModel,
            if newModel.WerIstDranIndex = 0 then
                Cmd.none
            else Cmd.ofMsg GegnerErsterZug


        //wenn werIstDrann nicht 0, dann neue Msg: GegnerErsterZug
    | GegnerErsterZug ->

        let gegnerKarte = model.KartenDerRunde.[model.WerIstDranIndex]
        let (_, kpunkte) = gegnerKarte.Krankheit
        let (_, sopunkte) = gegnerKarte.Sonstiges
        let apunkte = gegnerKarte.Alter
        let spunkte = gegnerKarte.Schamgefühl

        let listeWerte = [Krankheit kpunkte; Alter apunkte; Sonstiges sopunkte; Schamgefühl spunkte]

        let vergleichsWert = wählenDerBestenKarte listeWerte

        //let neuesModel = {
        //    model with
        //        Vergleichswerte = [vergleichsWert]
        //    }


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
            if model.RundenGewinner <> [] then 
                model.KartenDerRunde
                |> List.filter (fun x ->
                    model.RundenGewinner
                    |> List.exists (fun y -> y = x.Spieler))
            else model.KartenDerRunde

        let aktuelleSpieler =
            if model.RundenGewinner = [] then model.Spieler
            else model.RundenGewinner

        let zwischenGewinner =
            vergleicheKarten zuVergleichendeKarten vergleichswert aktuelleSpieler (niedrigOderHochGewinnt vergleichswert)

        let zwischenWerIstDran =
            let werIstDran = model.Spieler.[model.WerIstDranIndex]
            if model.RundenGewinner |> List.contains werIstDran then
                model.WerIstDranIndex
            else wessenKarte model.WerIstDranIndex (indizeesZwischenGewinner model zwischenGewinner)

        if
            //wenn ein Gewinner, oder Spieler am Zug
            zwischenGewinner.Length = 1 ||
            (((indizeesZwischenGewinner model zwischenGewinner) |> wessenKarte  model.WerIstDranIndex = 0) && ((indizeesZwischenGewinner model zwischenGewinner) |> wessenKarte  zwischenWerIstDran = 0 ))
        then

            //eigentlich kommthier die prüfung rein, ob es einen verlierer gibt.

            //in update anzeigen, dass jeweiliger Spieler verloren

            let neuesModel ={
                model with
                    RundenGewinner = zwischenGewinner
                    Weitersuchen = false
                    Vergleichswerte = vergleichswerte
                    MöglicheGegnerVergleichswerte = vergleichslisteGegner1 model.MöglicheGegnerVergleichswerte vergleichswerte
                    WerIstDranIndex = indizesAktiverSpieler model.Spieler model.AktiveSpieler |> wessenKarte  model.WerIstDranIndex
                    ZwischenWerIstDran = Some zwischenWerIstDran}

            gibtEsEinenVerlierer model neuesModel zwischenGewinner vergleichswerte

        //zwischenergebnis: mehrere gewinner
        else
                let neueKartenDerRunde =
                    model.KartenDerRunde
                    |> List.collect (fun x ->
                        if zwischenGewinner |> List.contains x.Spieler then
                            [x]
                        else []
                        )

                ////spieler ist unter den Gewinnern --> ins model schreiben, damit spieler nächsten zug wählen kann
                //hier eventuell eine Verzögerung 1-2 Sekunden einbauen??

                if (indizeesZwischenGewinner model zwischenGewinner) |> wessenKarte  model.WerIstDranIndex = 0 || zwischenWerIstDran = 0 then
                    let newModel ={
                        model with
                            test = [indizeesZwischenGewinner model zwischenGewinner]
                            RundenGewinner = zwischenGewinner
                            MöglicheGegnerVergleichswerte =  vergleichslisteGegner1 (möglicheVergleichsWerteGegner model zwischenGewinner) vergleichswerte
                            Vergleichswerte = vergleichswerte
                            ZwischengewinnerKarten = neueKartenDerRunde
                            ZwischenWerIstDran = Some zwischenWerIstDran
                            }
                    newModel, Cmd.none

                //nächste Zwischengewinner darf einen Wert auswählen
                else
                    let newModel ={
                        model with
                            test = [indizeesZwischenGewinner model zwischenGewinner]
                            RundenGewinner = zwischenGewinner
                            MöglicheGegnerVergleichswerte = vergleichslisteGegner1 (möglicheVergleichsWerteGegner model zwischenGewinner) vergleichswerte
                            Vergleichswerte = vergleichswerte
                            Weitersuchen = true
                            ZwischengewinnerKarten = neueKartenDerRunde
                            ZwischenWerIstDran = Some zwischenWerIstDran
                            }

                    newModel, Cmd.ofMsg WeitersuchenButton
                    //finalerRundenGewinner model zwischenGewinner vergleichswerte (vergleichslisteGegner1 model vergleichswerte zwischenGewinner model.WerIstDranIndex) model.WerIstDranIndex

    | SucheFinalenRundenGewinner ->

        if model.RundenGewinner.Length > 1 then

            

            //debugCounter <- debugCounter+1
            //if debugCounter > 10 then failwith "loop"

            let neueVergleichsWerte1 = [wählenDerBestenKarte model.MöglicheGegnerVergleichswerte] @ model.Vergleichswerte


            let neueZwischengewinnerkarten =
                model.ZwischengewinnerKarten
                |> List.filter (fun x ->
                    model.RundenGewinner |> List.contains x.Spieler
                    )
            //zwischenGewinnerKarten sind falsch, hier sind 1-4 karten, es müssen aber immer 4 karten sein
            let neueRundenGewinner =
                vergleicheKarten model.KartenDerRunde neueVergleichsWerte1.[0] model.RundenGewinner (niedrigOderHochGewinnt neueVergleichsWerte1.[0]) //ddd

            //Browser.Dom.console.log("indizes")
            //Browser.Dom.console.log((indizeesZwischenGewinner model neuerRundenGewinner) |> List.toArray )
            //Browser.Dom.console.log("wer ist dran")
            //Browser.Dom.console.log(model.WerIstDranIndex)

            //ab hier kann es sein, dass es bereits einen Gewinner gibt, neue werte sind also obsolet

            if neueRundenGewinner.Length = 1 then

                //debugCounter <- 0
                let neuesModel = {
                    model with
                        RundenGewinner = neueRundenGewinner
                        ZwischengewinnerKarten = neueZwischengewinnerkarten
                        Vergleichswerte = neueVergleichsWerte1
                        Weitersuchen = false
                        MöglicheGegnerVergleichswerte = vergleichslisteGegner1 model.MöglicheGegnerVergleichswerte neueVergleichsWerte1
                        WerIstDranIndex =  indizesAktiverSpieler model.Spieler model.AktiveSpieler |> wessenKarte model.WerIstDranIndex  
                        ZwischenWerIstDran = None}
                        

                Browser.Dom.console.log("indizes")
                Browser.Dom.console.log((indizeesZwischenGewinner model neueRundenGewinner) |> List.toArray )
                Browser.Dom.console.log("wer ist dran")
                Browser.Dom.console.log(model.WerIstDranIndex)

                gibtEsEinenVerlierer model neuesModel neueRundenGewinner neueVergleichsWerte1

            else
                let zwischenWerIstDran = wessenKarte model.WerIstDranIndex (indizeesZwischenGewinner model neueRundenGewinner)             
                let neueVergleichsWerteGegner =  vergleichslisteGegner1 model.MöglicheGegnerVergleichswerte neueVergleichsWerte1 

                let neuesModel = {
                    model with
                        RundenGewinner = neueRundenGewinner
                        ZwischengewinnerKarten = neueZwischengewinnerkarten
                        Vergleichswerte = neueVergleichsWerte1
                        ZwischenWerIstDran = Some zwischenWerIstDran
                        MöglicheGegnerVergleichswerte = neueVergleichsWerteGegner
                        Weitersuchen = false
                    }
                neuesModel, Cmd.ofMsg WeitersuchenButton


        else

            //debugCounter <- 0
            let neuesModel = {
                model with
                    Weitersuchen = false
                    }
            
            neuesModel, Cmd.none
        
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

          Container.container []
              [ Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
                    [
                        Heading.h3 [] [ str ("Choose number of players: " ) ]
                        p [] [str (sprintf "Wer ist dran: %s" (model.WerIstDranIndex.ToString()))]
                        let zwischenWerIstDran = 
                            match model.ZwischenWerIstDran with
                            |None -> 4
                            | Some wer -> wer
                        p [] [str (sprintf "Zwischen - Wer ist dran: %s" (zwischenWerIstDran.ToString()))]
                        p [] [str (sprintf "Aktive SPieler: %s" (model.AktiveSpieler.ToString()))]
                        p [] [str (sprintf "Rundengewinner: %s" (model.RundenGewinner.ToString()))]
                        p [] [str (sprintf "Mögliche Vergleichswerte: %s" (model.MöglicheGegnerVergleichswerte.ToString()))]
                        p [] [str (sprintf "Vergleichswerte %s" (model.Vergleichswerte.ToString()))]
                        p [] [str (sprintf "Zwischengewinnerkarten %s" (model.ZwischengewinnerKarten.ToString()))]
                        p [] [str (sprintf "Indizees RUndengewinner: %s" (model.test.ToString()))]
                        

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
                                        dispatch NeuStart
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
                                    [
                                        if model.Vergleichswerte =[] && model.KartenDerRunde = [] then
                                            str "Wähle Anzahl der Spieler"
                                        elif model.Vergleichswerte = [] then
                                            str "Wähle einen Wert zum Vergleich"
                                        elif model.RundenGewinner.Length > 1 && model.RundenGewinner |> List.contains Spieler1 && model.WerIstDranIndex = 0 then
                                            str "Wähle einen anderen Wert zum Vergleich"
                                        elif model.RundenGewinner.Length = 1 then
                                            str (sprintf "%s ist Gewinner der Runde" (model.RundenGewinner.Head.ToString()))
                                        elif model.RundenGewinner |> List.contains Spieler1 && model.WerIstDranIndex = 3 then
                                            str "Wähle einen anderen Wert zum Vergleich"
                                        else
                                            str ""
                                    ]
                            ]
                    ]

                if model.RundenGewinner.Length = 1 then
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
                    

                if model.Weitersuchen = true then
                    Columns.columns []
                        [
                            Column.column [Column.Props [Style [ TextAlign TextAlignOptions.Center ]]]
                                [
                                    Button.button
                                        [
                                            Button.OnClick (fun _ -> dispatch SucheFinalenRundenGewinner)
                                            Button.Props [Style [ FontSize "30px"; FontWeight "bold"]]
                                        ]
                                        [str "Weitersuchen"] //sagen wer dran ist??
                                ]
                        ]
                   
                Columns.columns []
                    [
                        for i in 1..model.AnzahlSpieler do
                        Column.column []
                            
                            [
                                Tile.ancestor []
                                    [ Tile.parent []
                                        [ Tile.child []
                                            [
                                              Box.box' []
                                                [
                                                    Heading.p []
                                                        [
                                                            if i = 1 then
                                                                str "Spieler"
                                                            else
                                                                str (sprintf "Gegner %i" (i-1))
                                                        ]
                                                ]

                                              

                                              if model.KartenDerRunde <> [] then
                                                    Box.box'
                                                        [
                                                            if model.RundenGewinner |> List.contains model.KartenDerRunde.[i-1].Spieler then
                                                                Common.Props [Style [ BackgroundColor "#2Fb660"]]
                                                            elif model.AktiveSpieler |> List.contains model.Spieler.[i-1] |> not then
                                                                Common.Props [Style [ BackgroundColor "#BDBDBD"]]
                                                            else
                                                                Common.Props [Style [ BackgroundColor "#FFb660"]]
                                                        ]
                                                        [
                                                            if model.AktiveSpieler |> List.contains model.Spieler.[i-1] |> not then
                                                                Columns.columns []
                                                                    [
                                                                        p [  Style [ CSSProp.FontSize "25px"] ][str (sprintf "Ausgeschieden")]
                                                                    ]
                                                            else
                                                            

                                                                Columns.columns []
                                                                    [
                                                                        Column.column []
                                                                            [
                                                                                p [  Style [ CSSProp.FontSize "25px"] ][str (sprintf "%s" model.KartenDerRunde.[i-1].Name)]
                                                                            ]
                                                                    ]


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
                                                                                                    Button.Props [Style [BackgroundColor "#00ff00"]]
                                                                                                elif model.Vergleichswerte(**.Tail**) |> List.contains (vergleichsWerte.[j] eigenschaften.[j]) then // schon verglichene Werte
                                                                                                    Button.Props [Style [ BackgroundColor "#00c8f0"]]


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


                                              if model.Decks <> [] then
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
