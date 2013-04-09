// Ulteriori informazioni su F# all'indirizzo http://fsharp.net
// Per ulteriori informazioni, vedere il progetto 'Esercitazione su F#'.
module LeapTrayApplication
    open System.Windows.Forms
    open System.Drawing
    open System.Collections.Generic
    open GestIT
    open FakeDriver
    open MyLeapFrame

    (* Structures *)
    let s = new FakeDriver.LeapSensor()
    let frameQueue = new Queue<MyFrame>()
    let mutable lastFrameInQueue = new MyFrame() // it represents the last enqueued frame

    (* Predicates *)
    let speed (x:float32) (y:float32) = x / y
    let p = new Predicate<LeapEventArgs>(fun x ->
        true
    )
    let moveright = new Predicate<LeapEventArgs>(fun x ->
//        printfn "Evaluating predicate moveright..."
        let id = x.Id
        let exists =
            if frameQueue |> Seq.exists (fun f -> not (f.PointableList.ContainsKey(id))) then
                false
            else
                let l =
                    frameQueue
                    |> Seq.pairwise
                    |> Seq.filter (fun (f1,f2) ->
                        let p1 = f1.PointableList.[id].Position
                        let p2 = f2.PointableList.[id].Position
                        let delta_s = System.Math.Abs(p2.x - p1.x)
                        let delta_t = (float32)(f2.Timestamp - f1.Timestamp) * 1000.f
                        let v_m = (delta_s / delta_t) * 1000000.f
                        let b1 = (p2.x >= p1.x) 
                        let b2 = (v_m >= 0.4f)
                        //printfn "p: %b %b" b1 b2
                        b1 && b2
                        )
                    |> Seq.length
                let thresh = int(float(frameQueue.Count) * 0.9)
//                printfn "moveright (l: %A thresh: %A)-> %A" l thresh ((l > thresh) && (System.Math.Abs(frameQueue.Peek().PointableList.[id].Position.x - lastFrameInQueue.PointableList.[id].Position.x) > 150.f))
                l > thresh
        exists && (lastFrameInQueue.PointableList.[id].Position.x - frameQueue.Peek().PointableList.[id].Position.x > 150.f)
    )
    let moveon = new Predicate<LeapEventArgs>(fun x ->
        let id = x.Id
        let exists =
            if frameQueue |> Seq.exists (fun f -> not (f.PointableList.ContainsKey(id))) then
                false
            else
                frameQueue |> Seq.pairwise |> Seq.exists (fun (f1,f2) ->
                let p1 = f1.PointableList.[id].Position
                let p2 = f2.PointableList.[id].Position
                let delta_s = System.Math.Abs(p2.x - p1.x)
                let delta_t = (float32)(f2.Timestamp - f1.Timestamp) * 1000.f
                let v_m = (delta_s / delta_t) * 1000000.f
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
            if frameQueue |> Seq.exists (fun f -> not (f.PointableList.ContainsKey(id))) then
                false
            else
                frameQueue |> Seq.pairwise |> Seq.exists (fun (f1,f2) ->
                let p1 = f1.PointableList.[id].Position
                let p2 = f2.PointableList.[id].Position
                let delta_s = System.Math.Abs(p2.x - p1.x)
                let delta_t = (float32)(f2.Timestamp - f1.Timestamp) / 1000.f // così passo da microsecondi a millisecondi
                let v_m = speed delta_s delta_t
                (p2.z > p1.z) && (System.Math.Abs(p2.y - p1.y) < 40.f) && (System.Math.Abs(p2.x - p1.x) < 40.f) && (v_m >= 0.3f)
            )
        exists
    )

    (* GroundTerms definitions *)
    let vedodito = new GroundTerm<_,_>(LeapFeatureTypes.ActiveFinger, p)
    let muovoditodx = new GroundTerm<_,_>(LeapFeatureTypes.MoveFinger, moveright)
    let muovoditoindietro = new GroundTerm<_,_>(LeapFeatureTypes.MoveFinger, moveback)
    let muovoditoavanti = new GroundTerm<_,_>(LeapFeatureTypes.MoveFinger, moveon)

    (* Gesture definition *)
    let seq = new Sequence<_,_>(vedodito, muovoditodx)
    //seq.Gesture.Add(fun _ -> printfn "%d * MOVE RIGHT (parallel to x) *" i)
    let net = muovoditodx.ToGestureNet(s)
    net.Completion.Add(fun _ -> SendKeys.SendWait("MARTA")
                                )

    (* Sensor *)
    let UpdateInformations (f:MyFrame, e:LeapFeatureTypes, id:FakeId) =
        (* Update informations in the last enqueued frame *)
        match e with
            | LeapFeatureTypes.ActiveHand -> lastFrameInQueue.HandList.Add(id, f.HandList.[id].Clone())
            | LeapFeatureTypes.ActiveFinger | LeapFeatureTypes.ActiveTool -> lastFrameInQueue.PointableList.Add(id, f.PointableList.[id].Clone())
            | LeapFeatureTypes.MoveHand -> lastFrameInQueue.HandList.[id] <- f.HandList.[id].Clone()
            | LeapFeatureTypes.MoveFinger | LeapFeatureTypes.MoveTool -> lastFrameInQueue.PointableList.[id] <- f.PointableList.[id].Clone()
            | LeapFeatureTypes.NotActiveHand -> lastFrameInQueue.HandList.Remove(id) |> ignore
            | LeapFeatureTypes.NotActiveFinger | LeapFeatureTypes.NotActiveTool -> lastFrameInQueue.PointableList.Remove(id) |> ignore
            | _ -> () 

    (s :> ISensor<_,_>).SensorEvents.Add(fun e ->
        (* Removing too old frames *)
        let t = e.Event.Frame.Timestamp
        while (frameQueue.Count > 0 && (t - frameQueue.Peek().Timestamp > (int64)250000)) do
            frameQueue.Dequeue() |> ignore

        (* Receiving updates from sensor *)
        let f = e.Event.Frame
        let id = e.Event.Id
        if lastFrameInQueue.Timestamp <> f.Timestamp then
            (* in this case, surely lastFrame.TS < f.TS, so it has to be added to the queue *)
            let newFrame = f.Clone()
            frameQueue.Enqueue(newFrame)
            lastFrameInQueue <- newFrame
        else
            (* update frame informations *)
            UpdateInformations(f, e.FeatureType, id)
    )
    System.Console.Read() |> ignore