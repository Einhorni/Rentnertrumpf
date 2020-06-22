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
    KartenDerRunde: KartenDerRunde
    RundenGewinner: Spieler list
    Vergleichswerte: Vergleichswert list

    Spieler: Spieler list
    WerIstDranIndex: int

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
        KartenDerRunde = []
        RundenGewinner = []
        Vergleichswerte = []

        Spieler = []
        WerIstDranIndex = 4

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


let rec finalerRundenGewinner model (rundenGewinner:Spieler list) (vergleichswerte:Vergleichswert list) (vergleichswerteGegner1:Vergleichswert list) = //dddd

    if rundenGewinner.Length > 1 then
        let indexDerVergleichsWerte = r.Next(0, vergleichswerteGegner1.Length) //eventuell höchsten bzw.niedrigsten wert wählen lassen
                            
        let neueKartenDerRunde =
            model.KartenDerRunde
            |> List.filter (fun x ->
                rundenGewinner |> List.contains x.Spieler
                )

        let neueVergleichsWerte = [vergleichswerteGegner1.[indexDerVergleichsWerte]] @ vergleichswerte

        let neueGegnerVergleichswerte = vergleichswerteGegner1.Tail

        let neuerRundenGewinner =
            vergleicheKarten neueKartenDerRunde neueVergleichsWerte.[0] rundenGewinner (niedrigOderHochGewinnt neueVergleichsWerte.[0]) //ddd

        finalerRundenGewinner model neuerRundenGewinner neueVergleichsWerte neueGegnerVergleichswerte

    else
        let newModel = {
            model with
                RundenGewinner = rundenGewinner
                Vergleichswerte = vergleichswerte}
        newModel, Cmd.none


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
    if indizes |> List.contains werIstDranIndex then
        werIstDranIndex
    //wer ist dran ist kein gewinner --> nächster gegner ist dran
    else
        if [0;1;2] |> List.contains werIstDranIndex  then

            //der nächsthöhere Wert wird genommen
            indizes
            |> List.filter (fun x -> x > werIstDranIndex)
            |> List.min

            //der niedrigste Wert wird genommen
        else indizes
            |> List.min


let vergleichswerteGegner1 (kartenDerRunde: Karte list) wessenKarte =
    let (kh, kpunkte) = kartenDerRunde.[wessenKarte].Krankheit
    let (so, spunkte) = kartenDerRunde.[wessenKarte].Sonstiges
    [Krankheit kpunkte; Alter kartenDerRunde.[wessenKarte].Alter; Sonstiges spunkte; Schamgefühl kartenDerRunde.[wessenKarte].Schamgefühl]

//nur noch die WErte, die bisher noch nicht verglichen wurden vergleichen
let vergleichslisteGegner1 model kartenDerRunde vergleichswerte zwischenGewinner werIstDranIndex =
    indizeesZwischenGewinner model zwischenGewinner
    |> wessenKarte  werIstDranIndex
    |> vergleichswerteGegner1 kartenDerRunde
    |> List.filter (fun x ->
        vergleichswerte
        |> List.exists (fun y ->
            match x, y with
            | Krankheit _ , Krankheit _ -> false
            | Alter _ , Alter _ -> false
            | Sonstiges _ , Sonstiges _ -> false
            | Schamgefühl _ , Schamgefühl _ -> false
            | _ -> true
            )
        )

let spieler =[Spieler1; Gegner1; Gegner2; Gegner3]

let update (msg : Msg) (model : Model) : Model * Cmd<Msg> =
    match msg with
    | Ziehen anzahlSpieler ->

        let spielerListe = spieler.[0..anzahlSpieler-1 ]

        let decks =
            if model.KartenDerRunde = [] then
                Logik.kartenDecks Logik.kartenSet anzahlSpieler
            else changeDecks model.Decks model.RundenGewinner.[0] model.KartenDerRunde
        let newModel = {
            model with 
                Decks = decks
                AnzahlSpieler = anzahlSpieler
                KartenDerRunde = Logik.kartenDerRunde decks anzahlSpieler
                RundenGewinner = []
                Vergleichswerte = []
                MöglicheGegnerVergleichswerte = []
                ZwischengewinnerKarten = []
                Weitersuchen = false
                WerIstDranIndex = werIstDran anzahlSpieler model.WerIstDranIndex
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
            }
        newModel, Cmd.none
    | Vergleich vergleichswert ->

        //hier muss jetzt noch  rein: wenn weristdran nicht 0, dann computerzug

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

        if zwischenGewinner.Length = 1 || model.WerIstDranIndex = 0 then 
            let newModel ={
                    model with
                        RundenGewinner = zwischenGewinner
                        Weitersuchen = false
                        MöglicheGegnerVergleichswerte = []
                        Vergleichswerte = vergleichswerte}
            newModel, Cmd.none

        //zeischenergebnis: mehrere gewinner
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
                if (indizeesZwischenGewinner model zwischenGewinner) |> wessenKarte  model.WerIstDranIndex = 0 then
                    let newModel ={
                        model with
                            test = [indizeesZwischenGewinner model zwischenGewinner]
                            RundenGewinner = zwischenGewinner
                            MöglicheGegnerVergleichswerte = vergleichslisteGegner1 model model.KartenDerRunde vergleichswerte zwischenGewinner model.WerIstDranIndex
                            Vergleichswerte = vergleichswerte
                            ZwischengewinnerKarten = neueKartenDerRunde
                            }
                    newModel, Cmd.none

                //nächste Zwischengewinner darf einen Wert auswählen
                else
                    let newModel ={
                        model with
                            test = [indizeesZwischenGewinner model zwischenGewinner]
                            RundenGewinner = zwischenGewinner
                            MöglicheGegnerVergleichswerte = vergleichslisteGegner1 model model.KartenDerRunde vergleichswerte zwischenGewinner model.WerIstDranIndex
                            Vergleichswerte = vergleichswerte
                            Weitersuchen = true
                            ZwischengewinnerKarten = neueKartenDerRunde
                            }



                    newModel, Cmd.ofMsg WeitersuchenButton
                    //finalerRundenGewinner model zwischenGewinner vergleichswerte (vergleichslisteGegner1 model vergleichswerte zwischenGewinner model.WerIstDranIndex) model.WerIstDranIndex

    | SucheFinalenRundenGewinner ->

        if model.RundenGewinner.Length > 1 then

            

            debugCounter <- debugCounter+1
            if debugCounter > 10 then failwith "loop"

            let neueVergleichsWerte1 = [wählenDerBestenKarte model.MöglicheGegnerVergleichswerte] @ model.Vergleichswerte


            let neueZwischengewinnerkarten =
                model.ZwischengewinnerKarten
                |> List.filter (fun x ->
                    model.RundenGewinner |> List.contains x.Spieler
                    )
            //zwischenGewinnerKarten sind falsch, hier sind 1-4 karten, es müssen aber immer 4 karten sein
            let neuerRundenGewinner =
                vergleicheKarten model.KartenDerRunde neueVergleichsWerte1.[0] model.RundenGewinner (niedrigOderHochGewinnt neueVergleichsWerte1.[0]) //ddd

            Browser.Dom.console.log("indizes")
            Browser.Dom.console.log((indizeesZwischenGewinner model neuerRundenGewinner) |> List.toArray )
            Browser.Dom.console.log("wer ist dran")
            Browser.Dom.console.log(model.WerIstDranIndex)

            //ab hier kann es sein, dass es bereits einen Gewinner gibt, neue werte sind also obsolet

            if neuerRundenGewinner.Length = 1 then
                debugCounter <- 0
                let neuesModel = {
                    model with
                        RundenGewinner = neuerRundenGewinner
                        ZwischengewinnerKarten = neueZwischengewinnerkarten
                        Vergleichswerte = neueVergleichsWerte1
                        Weitersuchen = false
                    }
                neuesModel, Cmd.none

            else
                let neuerWerIstDran = wessenKarte model.WerIstDranIndex (indizeesZwischenGewinner model neuerRundenGewinner)             
                let neueVergleichsWerteGegner = vergleichslisteGegner1 model model.KartenDerRunde neueVergleichsWerte1 neuerRundenGewinner model.WerIstDranIndex

                let neuesModel = {
                    model with
                        RundenGewinner = neuerRundenGewinner
                        ZwischengewinnerKarten = neueZwischengewinnerkarten
                        Vergleichswerte = neueVergleichsWerte1
                        WerIstDranIndex = neuerWerIstDran
                        MöglicheGegnerVergleichswerte = neueVergleichsWerteGegner
                        Weitersuchen = false
                    }
                neuesModel, Cmd.ofMsg WeitersuchenButton

            //let vergleichslisteGegner1 model kartenDerRunde vergleichswerte zwischenGewinner werIstDranIndex =
            //    indizeesZwischenGewinner model zwischenGewinner
            //    |> wessenKarte  werIstDranIndex
            //    |> vergleichswerteGegner1 kartenDerRunde
            //    |> List.filter (fun x ->
            //        vergleichswerte
            //        |> List.exists (fun y ->
            //            match x, y with
            //            | Krankheit _ , Krankheit _ -> false
            //            | Alter _ , Alter _ -> false
            //            | Sonstiges _ , Sonstiges _ -> false
            //            | Schamgefühl _ , Schamgefühl _ -> false
            //            | _ -> true
            //            )
            //        )


        else

            debugCounter <- 0
            let neuesModel = {
                model with
                    Weitersuchen = false}
            
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
                                                            else
                                                                Common.Props [Style [ BackgroundColor "#FFb660"]]
                                                        ]
                                                        [
                                                            
                                                            

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
                                                                                            model.WerIstDranIndex <> 0
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
