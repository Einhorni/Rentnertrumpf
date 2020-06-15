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


type Model = {

    Decks: Decks 
    AnzahlSpieler: int
    KartenDerRunde: KartenDerRunde
    RundenGewinner: Spieler list
    Vergleichswerte: Vergleichswert list

    Spieler: Spieler list
    WerIstDranIndex: int

    test: int list list
    }

type Msg =
    | Ziehen of int
    | GegnerErsterZug
    | NeuStart
    | Vergleich of Vergleichswert
    | NeueRunde


let init () : Model * Cmd<Msg> =
    let initialModel = {
        Decks = []
        AnzahlSpieler = 0
        KartenDerRunde = []
        RundenGewinner = []
        Vergleichswerte = []

        Spieler = []
        WerIstDranIndex = 4

        test =[[]]
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
            vergleicheKarten neueKartenDerRunde neueVergleichsWerte.[0] (niedrigOderHochGewinnt neueVergleichsWerte.[0]) //ddd

        finalerRundenGewinner model neuerRundenGewinner neueVergleichsWerte neueGegnerVergleichswerte

    else
        let newModel = {
            model with
                RundenGewinner = rundenGewinner
                Vergleichswerte = vergleichswerte}
        newModel, Cmd.none


let spieler =[Spieler1; Gegner1; Gegner2; Gegner3]

let update (msg : Msg) (model : Model) : Model * Cmd<Msg> =
    match msg with
    | Ziehen anzahlSpieler ->

        let spielerListe = spieler.[0..anzahlSpieler-1]

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
                WerIstDranIndex = werIstDran anzahlSpieler model.WerIstDranIndex
                Spieler = spielerListe
            }    
        newModel, Cmd.none


        //wenn werIstDrann nicht 0, dann neue Msg: GegnerErsterZug
    | GegnerErsterZug ->
        let gegnerKarte = model.KartenDerRunde.[model.WerIstDranIndex]
        let (_, kpunkte) = gegnerKarte.Krankheit
        let (_, sopunkte) = gegnerKarte.Sonstiges
        let apunkte = alterFünferWert gegnerKarte.Alter
        let spunkte =
            match gegnerKarte.Schamgefühl with
            | 1 -> 5
            | 2 -> 4
            | 3 -> 3
            | 4 -> 2
            | 5 -> 1
            | _ -> 0

        let listeWerte = [Krankheit kpunkte; Alter apunkte; Sonstiges sopunkte; Schamgefühl spunkte]

        let indexVergleichswert = 
            listeWerte //[1;3;6;2]
            |> List.indexed
            |> List.sortByDescending (fun (i, x) -> x)
            |> List.map (fun (i,x) -> i)
            |> List.head

        let vergleichsWert = listeWerte.[indexVergleichswert]

        //wenn unentschieden, dann darf derselbe Gegner nochmal wählen

        model, Cmd.none
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
        let vergleichswerte =
            [vergleichswert] @ model.Vergleichswerte
        
        let zwischenGewinner =
            vergleicheKarten model.KartenDerRunde vergleichswert (niedrigOderHochGewinnt vergleichswert)
            
        //if model.RundenGewinner = [] then

        //zwischenergebnis: ein gewinner
        if zwischenGewinner.Length = 1 then 
            let newModel ={
                    model with
                        RundenGewinner = zwischenGewinner
                        Vergleichswerte = vergleichswerte}
            newModel, Cmd.none


        //zeischenergebnis: mehrere gewinner
        else
                let indizeesZwischenGewinner model = 
                    model.Spieler
                    |> List.indexed
                    |> List.filter (fun (i,x) ->
                        zwischenGewinner
                        |> List.exists (fun y -> y=x)
                        )
                    |> List.map (fun (i,x) -> i)

                


                let wessenKarte model =
                    //weristdran ist zwischengewinner --> immer noch dran
                    if indizeesZwischenGewinner model |> List.contains model.WerIstDranIndex then
                        model.WerIstDranIndex
                    //wer ist dran ist kein gewinner --> nächster gegner ist dran
                    else
                        if [0;1;2] |> List.contains model.WerIstDranIndex  then

                            //der nächsthöhere Wert wird genommen
                            indizeesZwischenGewinner model
                            |> List.filter (fun x -> x > model.WerIstDranIndex)
                            |> List.min

                            //der niedrigste Wert wird genommen
                        else indizeesZwischenGewinner model
                            |> List.min


                
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
                            vergleicheKarten neueKartenDerRunde neueVergleichsWerte.[0] (niedrigOderHochGewinnt neueVergleichsWerte.[0]) //ddd

                        finalerRundenGewinner model neuerRundenGewinner neueVergleichsWerte neueGegnerVergleichswerte

                    else
                        let newModel = {
                            model with
                                RundenGewinner = rundenGewinner
                                Vergleichswerte = vergleichswerte}
                        newModel, Cmd.none




                ////spieler ist unter den Gewinnern --> ins model schreiben, damit spieler nächsten zug wählen kann
                //hier eventuell eine Verzögerung 1-2 Sekunden einbauen??
                if wessenKarte model = 0 then
                    let newModel ={
                        model with
                            test = [indizeesZwischenGewinner model]
                            RundenGewinner = zwischenGewinner
                            Vergleichswerte = vergleichswerte}
                    newModel, Cmd.none

                //nächste Zwischengewinner darf einen Wert auswählen
                else
                    
                    let vergleichswerteGegner1 =
                            let (kh, kpunkte) = model.KartenDerRunde.[wessenKarte model].Krankheit
                            let (so, spunkte) = model.KartenDerRunde.[wessenKarte model].Sonstiges
                            [Krankheit kpunkte; Alter model.KartenDerRunde.[wessenKarte model].Alter; Sonstiges spunkte; Schamgefühl model.KartenDerRunde.[wessenKarte model].Schamgefühl]


                    //nur noch die WErte, die bisher noch nicht verglichen wurden vergleichen
                    let vergleichslisteGegner1 =
                        vergleichswerteGegner1
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

                    finalerRundenGewinner model zwischenGewinner vergleichswerte vergleichslisteGegner1

        //schon Gewinner im Modell, da vorher SPieler ein Gewinner
        //else
        //    //wenn Spieler nicht unter den Gewinnern
        //    if not (zwischenGewinner |> List.contains Spieler1) then
        //        let neueKartenDerRunde =
        //            model.KartenDerRunde
        //            |> List.filter (fun x ->
        //                model.RundenGewinner |> List.contains x.Spieler
        //                )
        //        let neueGewinner =
        //            vergleicheKarten neueKartenDerRunde vergleichswert (niedrigOderHochGewinnt vergleichswert)
        //        let newModel = {
        //            model with
        //                RundenGewinner = neueGewinner
        //                Vergleichswerte = vergleichswerte}
        //        newModel, Cmd.none
        //    else

        //        let vergleichswerteGegner1 =
        //            let (kh, kpunkte) = model.KartenDerRunde.[1].Krankheit
        //            let (so, spunkte) = model.KartenDerRunde.[1].Sonstiges
        //            [Krankheit kpunkte; Alter model.KartenDerRunde.[1].Alter; Sonstiges spunkte; Schamgefühl model.KartenDerRunde.[1].Schamgefühl]

        //        let VergleichslisteGegner1 =
        //            vergleichswerteGegner1
        //            |> List.filter (fun x ->
        //                model.Vergleichswerte
        //                |> List.exists (fun y ->
        //                    match x, y with
        //                    | Krankheit _ , Krankheit _ -> false
        //                    | Alter _ , Alter _ -> false
        //                    | Sonstiges _ , Sonstiges _ -> false
        //                    | Schamgefühl _ , Schamgefühl _ -> false
        //                    | _ -> true
        //                    )
        //                )

        //        let indexDerVergleichsWerte = r.Next(0, VergleichslisteGegner1.Length)  //eventuell höchsten bzw.niedrigsten wert wählen lassen

        //        let neueKartenDerRunde =
        //            model.KartenDerRunde
        //            |> List.filter (fun x ->
        //                model.RundenGewinner |> List.contains x.Spieler
        //                )

        //        let neueVergleichsWerte = [VergleichslisteGegner1.[indexDerVergleichsWerte]] @ vergleichswerte

        //        let rundenGewinner =
        //            vergleicheKarten neueKartenDerRunde neueVergleichsWerte.[0] (niedrigOderHochGewinnt neueVergleichsWerte.[0])

                    
            
        //        let newModel ={
        //            model with
        //                RundenGewinner = rundenGewinner
        //                Vergleichswerte = neueVergleichsWerte}
        //        newModel, Cmd.none
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
                        p [] [str (sprintf "%s" (model.WerIstDranIndex.ToString()))]
                        p [] [str (sprintf "%s" (model.RundenGewinner.ToString()))]
                        p [] [str (sprintf "%s" (model.test.ToString()))]
                        p [] [str (sprintf "%s" (model.Vergleichswerte.ToString()))]
                        p [] [str (sprintf "%s" (model.KartenDerRunde.ToString()))]
                        

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
                                        elif model.RundenGewinner.Length > 1 && model.RundenGewinner |> List.contains Spieler1 then
                                            str "Wähle einen anderen Wert zum Vergleich"
                                        elif model.RundenGewinner.Length = 1 then
                                            str (sprintf "%s ist Gewinner der Runde" (model.RundenGewinner.Head.ToString()))
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
                                                                                            
                                                                                        Button.Disabled
                                                                                            (model.RundenGewinner.Length >= 1 &&
                                                                                            (not (model.RundenGewinner |> (List.contains model.KartenDerRunde.[i-1].Spieler)))|| //wenn kein Rundengewinner     --> notwendig?? Gegnerkarten werden eh unsichtbar
                                                                                            model.Vergleichswerte |> List.contains (vergleichsWerte.[j] eigenschaften.[j])  || //schon verglichene Werte
                                                                                            model.RundenGewinner.Length = 1) //endgültiger RUndengewinner steht fest
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
