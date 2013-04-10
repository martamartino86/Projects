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
                (* counting how many couples of frames respect the position and velocity condition *)
                let l =
                    frameQueue
                    |> Seq.pairwise
                    |> Seq.filter (fun (f1,f2) ->
                        let p1 = f1.PointableList.[id].Position
                        let p2 = f2.PointableList.[id].Position
                        let delta_s = System.Math.Abs(p2.x - p1.x)
                        let delta_t = (float32)(f2.Timestamp - f1.Timestamp) * 1000.f
                        let v_m = (delta_s / delta_t) * 1000000.f
                        (p2.x >= p1.x) && (v_m >= 0.4f)
                        )
                    |> Seq.length
                let thresh = int(float(frameQueue.Count) * 0.9)
                l > thresh
        exists && (lastFrameInQueue.PointableList.[id].Position.x - frameQueue.Peek().PointableList.[id].Position.x > 150.f)
    )
    let moveon = new Predicate<LeapEventArgs>(fun x ->
        let id = x.Id
        let exists =
            if frameQueue |> Seq.exists (fun f -> not (f.PointableList.ContainsKey(id))) then
                false
            else
                frameQueue
                |> Seq.pairwise
                |> Seq.exists (fun (f1,f2) ->
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
                frameQueue
                |> Seq.pairwise
                |> Seq.exists (fun (f1,f2) ->
                                let p1 = f1.PointableList.[id].Position
                                let p2 = f2.PointableList.[id].Position
                                let delta_s = System.Math.Abs(p2.x - p1.x)
                                let delta_t = (float32)(f2.Timestamp - f1.Timestamp) / 1000.f // così passo da microsecondi a millisecondi
                                let v_m = speed delta_s delta_t
                                (p2.z > p1.z) && (System.Math.Abs(p2.y - p1.y) < 40.f) && (System.Math.Abs(p2.x - p1.x) < 40.f) && (v_m >= 0.3f)
                )
        exists
    )
    (*
    let openhand = new Predicate<LeapEventArgs>(fun x ->
        printfn ("Evaluating OPENHAND...")
        let x =
            frameQueue
            |> Seq.pairwise
            |> Seq.takeWhile (fun (f1,f2) -> f1.PointableList.Count <= f2.PointableList.Count)
        if x |> Seq.length <> 0 then
            let zeroFingerFirst,zeroFingerSecond = (x |> Seq.head)
            zeroFingerSecond.
            //zer .PointableList.Count = 0)
        else
            ()
        //printfn "%A - %A - %A " zeroFingerFirst increasingPointables (lastFrameInQueue.PointableList.Count >= 4)
        //increasingPointables && lastFrameInQueue.PointableList.Count >= 4
    )
    *)
    let openhand = new Predicate<LeapEventArgs>(fun x -> false)
    let closehand = new Predicate<LeapEventArgs>(fun x ->
        //printfn ("Evaluating CLOSEHAND...")
        let decreasingPointables =
            frameQueue
            |> Seq.pairwise
            |> Seq.exists (fun (f1,f2) -> f1.HandList.Count = f2.HandList.Count && f1.PointableList.Count <= f2.PointableList.Count)
        decreasingPointables
    )
    let radiusSphereMin n =
        new Predicate<LeapEventArgs>(fun x -> x.Frame.HandList.Item(x.Id).SphereRadius <= n)
    let radiusSphereMax n =
        new Predicate<LeapEventArgs>(fun x -> x.Frame.HandList.Item(x.Id).SphereRadius >= n)

    let pointableCountIs n =
        new Predicate<LeapEventArgs>(fun x -> printfn "vedo %d dita e ne voglio %d" x.Frame.PointableList.Count n
                                              x.Frame.PointableList.Count = n)
    (* GroundTerms definitions *)
    let vedodito = new GroundTerm<_,_>(LeapFeatureTypes.ActiveFinger, p)
    
    let vedodito1 = new GroundTerm<_,_>(LeapFeatureTypes.ActiveFinger, pointableCountIs 1)
    let vedodito2 = new GroundTerm<_,_>(LeapFeatureTypes.ActiveFinger, pointableCountIs 2)
    let vedodito3 = new GroundTerm<_,_>(LeapFeatureTypes.ActiveFinger, pointableCountIs 3)
    let vedodito4 = new GroundTerm<_,_>(LeapFeatureTypes.ActiveFinger, pointableCountIs 4)
    let vedodito5 = new GroundTerm<_,_>(LeapFeatureTypes.ActiveFinger, pointableCountIs 5)
    let nonvedodito4 = new GroundTerm<_,_>(LeapFeatureTypes.NotActiveFinger, pointableCountIs 4)
    let nonvedodito3 = new GroundTerm<_,_>(LeapFeatureTypes.NotActiveFinger, pointableCountIs 3)
    let nonvedodito2 = new GroundTerm<_,_>(LeapFeatureTypes.NotActiveFinger, pointableCountIs 2)
    let nonvedodito1 = new GroundTerm<_,_>(LeapFeatureTypes.NotActiveFinger, pointableCountIs 1)

    let muovoditodx = new GroundTerm<_,_>(LeapFeatureTypes.MoveFinger, moveright)
    let muovoditoindietro = new GroundTerm<_,_>(LeapFeatureTypes.MoveFinger, moveback)
    let muovoditoavanti = new GroundTerm<_,_>(LeapFeatureTypes.MoveFinger, moveon)
    let apromano = new GroundTerm<_,_>(LeapFeatureTypes.MoveHand, openhand)
    let chiudomano = new GroundTerm<_,_>(LeapFeatureTypes.MoveHand, radiusSphereMax 80.F)
    let muovimano = new GroundTerm<_,_>(LeapFeatureTypes.MoveHand, radiusSphereMin 80.F)

    (* Gesture definition *)
    let s1 = new Sequence<_,_>(vedodito1, vedodito2)
    let s2 = new Sequence<_,_>(s1, vedodito3)
    let s3 = new Sequence<_,_>(s2, vedodito4)
    let s4 = new Sequence<_,_>(s3, vedodito5)
    let m1 = new Sequence<_,_>(s4, muovimano)
    let s5 = new Sequence<_,_>(m1, nonvedodito4)
    let s6 = new Sequence<_,_>(m1, nonvedodito3)
    let s7 = new Sequence<_,_>(m1, nonvedodito2)
    let s8 = new Sequence<_,_>(m1, chiudomano)
    m1.Gesture.Add(fun _ -> SendKeys.SendWait("^{ESC}"))
    s5.Gesture.Add(fun _ -> printfn "MA CI ARRIVI CAZZO??")
    s8.Gesture.Add(fun _ -> SendKeys.SendWait("{ESC}"))
    let netdestocazzo = m1.ToGestureNet(s)

    let seq = new Sequence<_,_>(vedodito, muovoditodx)
    muovoditodx.Gesture.Add(fun _ -> SendKeys.SendWait("MARTA"))
    let net = muovoditodx.ToGestureNet(s)

    let net1 = apromano.ToGestureNet(s)
    (*net1.Completion.Add(fun _ ->
                            SendKeys.SendWait("^{ESC}")
                            printfn ("[OPEN HAND]")) // dovrebbe aprirsi il menu grosso di win8
                            *)
    let net2 = chiudomano.ToGestureNet(s)
    net1.Completion.Add(fun _ ->
                            SendKeys.SendWait("^{F4}")
                            printfn ("[CLOSE HAND]")) // dovrebbe chiudere la finestra corrente
    
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
        printfn "NOT ACTIVE POINTABLE"
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