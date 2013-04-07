// Ulteriori informazioni su F# all'indirizzo http://fsharp.net
// Per ulteriori informazioni, vedere il progetto 'Esercitazione su F#'.
module Program
open FakeDriver
open GestIT
open MyLeapFrame
open Leap
open System.Windows.Forms
open System.Drawing
open System.Numerics
open System.Collections.Generic


let speed (x:float32) (y:float32) = x/y

(* cerca un id all'interno della lista, che puo' essere una HandList, FingerList o ToolList.
let SearchId(list:Leap.Interface, id:int) =
    match list with
        | :? HandList as list -> (list |> Seq.exists (fun x -> x.Id = id), Some FakeDriver.Hand)
        | :? FingerList as list -> (list |> Seq.exists (fun x -> x.Id = id), Some FakeDriver.Finger)
        | :? ToolList as list-> (list |> Seq.exists (fun x -> x.Id = id), Some FakeDriver.Tool)
        | _ -> (false, None)
*)
let visibleObjects = new Dictionary<FakeId, LeapInfoType>()
let listFrame = new Queue<MyFrame>()
let mutable lastFrameInQueue = new MyFrame()
let s = new FakeDriver.LeapSensor()
let epsilon = 20.F
// PREDICATE: sempre true
let p = new Predicate<LeapEventArgs>(fun _ ->
    true
)
// PREDICATE: muovi un dito v/destra sull'asse x
let moveright = new Predicate<LeapEventArgs>(fun x ->
    let id = x.Id
    let visibleType = visibleObjects.[id]
    let exists =
        if listFrame |> Seq.exists (fun f -> not (f.PointableList.ContainsKey(id))) then
            false
        else
            listFrame |> Seq.pairwise |> Seq.forall (fun (f1,f2) ->
                let p1 = f1.PointableList.[id].Position
                let p2 = f2.PointableList.[id].Position
                let delta_s = System.Math.Abs(p2.x - p1.x)
                let delta_t = (float32)(f2.Timestamp - f1.Timestamp) * 1000.f
                let v_m = (delta_s / delta_t) * 1000000.f
//                if v_m >= 1.f then
//                    printfn "vel media: %f" v_m
                (p2.x >= p1.x) && (v_m >= 1.f)
        )
    exists && (System.Math.Abs(listFrame.Peek().PointableList.[id].Position.x - lastFrameInQueue.PointableList.[id].Position.x) > 150.f)
)
let moveon = new Predicate<LeapEventArgs>(fun x ->
    let id = x.Id
    let exists =
        if listFrame |> Seq.exists (fun f -> not (f.PointableList.ContainsKey(id))) then
            false
        else
            listFrame |> Seq.pairwise |> Seq.exists (fun (f1,f2) ->
            let p1 = f1.PointableList.[id].Position
            let p2 = f2.PointableList.[id].Position
            let delta_s = System.Math.Abs(p2.x - p1.x)
            let delta_t = (float32)(f2.Timestamp - f1.Timestamp) * 1000.f
            let v_m = (delta_s / delta_t) * 1000000.f
            //printfn "%s " (((p2.z <= p1.z) && (System.Math.Abs(p2.y - p1.y) < 40.f) && (System.Math.Abs(p2.x - p1.x) < 40.f) && (v_m >= 0.3f)).ToString())
            printfn "p2.z <= p1.z: %s" ((p2.z <= p1.z).ToString())
            printfn "System.Math.Abs(p2.y - p1.y) < 40.f: %s" ((System.Math.Abs(p2.y - p1.y) < 40.f).ToString())
            printfn "System.Math.Abs(p2.x - p1.x) < 40.f: %s" ((System.Math.Abs(p2.x - p1.x) < 40.f).ToString())
            printfn "v_m >= 0.3f: %s" ((v_m >= 0.3f).ToString())
            (p2.z <= p1.z) && (System.Math.Abs(p2.y - p1.y) < 40.f) && (System.Math.Abs(p2.x - p1.x) < 40.f) && (v_m >= 0.3f)
        )
    exists
)
let moveback = new Predicate<LeapEventArgs>(fun x ->
    let id = x.Id
    let exists =
        if listFrame |> Seq.exists (fun f -> not (f.PointableList.ContainsKey(id))) then
            false
        else
            listFrame |> Seq.pairwise |> Seq.exists (fun (f1,f2) ->
            let p1 = f1.PointableList.[id].Position
            let p2 = f2.PointableList.[id].Position
            let delta_s = System.Math.Abs(p2.x - p1.x)
            let delta_t = (float32)(f2.Timestamp - f1.Timestamp) / 1000.f // così passo da microsecondi a millisecondi
            let v_m = speed delta_s delta_t
            //let diff_y = System.Math.Abs(p2.y - p1.y)
            (p2.z > p1.z) && (System.Math.Abs(p2.y - p1.y) < 40.f) && (System.Math.Abs(p2.x - p1.x) < 40.f) && (v_m >= 0.3f)
        )
    exists
)
let vedodito = new GroundTerm<_,_>(LeapFeatureTypes.ActiveFinger, p)
let muovoditodx = new GroundTerm<_,_>(LeapFeatureTypes.MoveFinger, moveright)
let muovoditoindietro = new GroundTerm<_,_>(LeapFeatureTypes.MoveFinger, moveback)
let muovoditoavanti = new GroundTerm<_,_>(LeapFeatureTypes.MoveFinger, moveon)

let seq = new Sequence<_,_>(vedodito, muovoditodx)
seq.Gesture.Add(fun _ -> printfn "* MOVE RIGHT (parallel to x) *")
(*
let net = seq.ToGestureNet(s)
*)

//let seq1 = new Sequence<_,_>(vedodito, muovoditoavanti)
//seq1.Gesture.Add(fun _ -> printfn "AAAAVANTI")
let seq2 = new Sequence<_,_>(muovoditoavanti, muovoditoindietro)
seq2.Gesture.Add(fun _ -> printfn "* PICK! *")

let net2 = seq2.ToGestureNet(s)

//(s :> ISensor<_,_>).SensorEvents.Add(fun e ->
//    printfn "%A" (e.FeatureType.ToString())
//)

// mi attacco agli eventi del sensore Leap per aggiornare la lista di oggetti che vedo, e la lista di Frame su cui fare analisi temporale.
(s :> ISensor<_,_>).SensorEvents.Add(fun e ->
    // prendo l'ID dell'oggetto a cui si riferisce la feature dell'evento sollevato
    let id = e.Event.Id
    let f = e.FeatureType
    if not (listFrame.Contains(e.Event.Frame)) then
        lastFrameInQueue <- e.Event.Frame
        listFrame.Enqueue(e.Event.Frame)
    if not (visibleObjects.ContainsKey(id)) then
        // se non ce l'ho tra i visibili, devo aggiungerlo (e dargli un tipo che capisco a seconda della feature)
        match e.FeatureType with
            | LeapFeatureTypes.ActiveHand | LeapFeatureTypes.MoveHand -> visibleObjects.Add(id, LeapInfoType.Hand)
            | LeapFeatureTypes.ActiveFinger | LeapFeatureTypes.MoveFinger -> visibleObjects.Add(id, LeapInfoType.Finger)
            | LeapFeatureTypes.ActiveTool | LeapFeatureTypes.MoveTool -> visibleObjects.Add(id, LeapInfoType.Tool)
            | _ -> ()
    else
        match e.FeatureType with
            | LeapFeatureTypes.NotActiveHand | LeapFeatureTypes.NotActiveFinger | LeapFeatureTypes.NotActiveTool -> visibleObjects.Remove(id) |> ignore
            | _ -> ()
    // svuoto la coda di frame se sono lì da troppo tempo
    let t = e.Event.Frame.Timestamp
    while (listFrame.Count > 0 && (t - listFrame.Peek().Timestamp > (int64)500000)) do
        listFrame.Dequeue() |> ignore
    //printfn "Ci sono %d frame in lista" listFrame.Count 
)



System.Console.Read() |> ignore

(*
let f = new System.Windows.Forms.Form()
f.Controls.Add(s)
s.BackColor <- Color.Red;
s.ForeColor <- Color.Green;
f.Show()

System.Windows.Forms.Application.Run(f)

let s = new FakeDriver.LeapController() // mi attacco al Leap
let p = new Predicate(fun _ -> true) // predicato

// START >> MOVE >> END
let st = new GestIT.GroundTerm<_>(LeapFeatureTypes.ActivePalm, p)
let mo = new GestIT.GroundTerm<_>(LeapFeatureTypes.MovePalm, p)
let seq1 = new Sequence<_>(st, mo)

let en = new GestIT.GroundTerm<_>(LeapFeatureTypes.NotActivePalm, p)
let seq2 = new Sequence<_>(seq1, en)

seq2.Gesture.Add(fun _ -> while (true) do System.Console.WriteLine "YEEEEE!")

seq2.ToNet(s)

System.Console.ReadLine() |> ignore
(*
// Gesture:  S >> M* [> E
let s = new GroundTerm<_>(TouchFeatureTypes.Start, (fun () -> true), false)
let m = new GroundTerm<_>(TouchFeatureTypes.Move, (fun () -> true), true)
let seq = new Sequence<_>(s, m)

let e = new GroundTerm<_>(TouchFeatureTypes.End, (fun () -> true), false)
let dis = new Parallel<_>(seq, e)

PrepareNet(dis)
printfn "First net -  %d tokens" listToken.Length
*)

// Gesture a caso sul quaderno (riutilizzo anche roba di sopra)
let m1 = new GroundTerm<_>(TouchFeatureTypes.Move, (fun () -> true), false)
let p1 = new Parallel<_>(seq, m1)
let m2 = new GroundTerm<_>(TouchFeatureTypes.Move, (fun () -> true), false)
let p2 = new Parallel<_>(p1, m2)

let s1 = new GroundTerm<_>(TouchFeatureTypes.Start, (fun () -> true), false)
let s2 = new GroundTerm<_>(TouchFeatureTypes.Start, (fun () -> true), false)
let p3 = new Parallel<_>(s1, s2)

let seq1 = new Sequence<_>(p2, p3)
PrepareNet(seq1)
printfn "Second net -  %d tokens" listToken.Length
System.Console.Read() |> ignore

listToken <- []
*)

 