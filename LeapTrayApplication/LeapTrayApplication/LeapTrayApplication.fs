// Ulteriori informazioni su F# all'indirizzo http://fsharp.net
// Per ulteriori informazioni, vedere il progetto 'Esercitazione su F#'.
module LeapTrayApplication
    open System.Windows.Forms
    open System.Drawing
    open System.Collections.Generic
    open GestIT
    open ClonableLeapFrame
    open LeapDriver

    (* Structures *)
    let s = new LeapDriver.LeapSensor()
    let frameQueue = new Queue<ClonableFrame>()
    let mutable lastFrameInQueue = new ClonableFrame() // it represents the last enqueued frame
    let vectorX = new Leap.Vector((float32)1, (float32)0, (float32)0)
    let vectorY = new Leap.Vector((float32)0, (float32)(-1),(float32) 0)

    (* Timestamps *)
    let ts_openedhand = ref(-1L : TimeStamp)
    let ts_closedhand = ref(-1L : TimeStamp)
    let mutable lastEnter:TimeStamp = -1L

    (* Predicates *)
    let speed (x:float32) (y:float32) = x / y
    let p = new Predicate<LeapEventArgs>(fun x -> true)
    let movehandright (x:LeapEventArgs) =
        let f = x.Frame
        let id = x.Id
        let exists =
            if frameQueue |> Seq.exists (fun f -> not (f.HandList.ContainsKey(id))) then
                false
            else
                let l =
                    frameQueue
                    |> Seq.pairwise
                    |> Seq.filter (fun (f1,f2) ->
                        let p1 = f1.HandList.[id].Position
                        let p2 = f2.HandList.[id].Position
                        let delta_s = System.Math.Abs(p2.x - p1.x)
                        let delta_t = (float32)(f2.Timestamp - f1.Timestamp) * 1000.f
                        let v_m = (delta_s / delta_t) * 1000000.f
                        (p2.x >= p1.x) && (v_m >= 0.4f)
                    )
                    |> Seq.length
                let thresh = int(float(frameQueue.Count) * 0.7)
                l > thresh
        exists
    
    let movehandleft (x:LeapEventArgs) =
        let f = x.Frame
        let id = x.Id
        let exists =
            if frameQueue |> Seq.exists (fun f -> not (f.HandList.ContainsKey(id))) then
                false
            else
                let l =
                    frameQueue
                    |> Seq.pairwise
                    |> Seq.filter (fun (f1,f2) ->
                        let p1 = f1.HandList.[id].Position
                        let p2 = f2.HandList.[id].Position
                        let delta_s = System.Math.Abs(p2.x - p1.x)
                        let delta_t = (float32)(f2.Timestamp - f1.Timestamp) * 1000.f
                        let v_m = (delta_s / delta_t) * 1000000.f
                        (p2.x <= p1.x) && (v_m >= 0.4f)
                    )
                    |> Seq.length
                let thresh = int(float(frameQueue.Count) * 0.7)
                l > thresh
        exists
    
    let pushhanddown (x:LeapEventArgs) =
        let thresh = 50.f
        let f = x.Frame
        if (lastEnter >= f.Timestamp - 1000000L) || (x.Frame.PointableList.Count < 4) then
            false
        else
            let id = x.Id
            let o = x.Frame.HandList.[id].Position
            let coda =
                frameQueue
                |> Seq.filter (fun y -> y.HandList.ContainsKey(id) && y.Timestamp >= f.Timestamp - 100000L)
            if coda |> Seq.isEmpty then
                false
            else
                let maxY =
                    coda
                    |> Seq.maxBy (fun z -> z.HandList.[id].Position.y)
                if maxY.HandList.[id].Position.y - o.y > 100.f then
                    coda
                    |> Seq.filter (fun z -> z.Timestamp >= maxY.Timestamp)
                    |> Seq.forall (fun z ->
                                    let v = z.HandList.[id].Position
                                    let dx = v.x - o.x
                                    let dz = v.z - o.z
                                    (dx*dx + dz*dz) < thresh * thresh
                                    )
                else
                    false

    let movefingerright (x:LeapEventArgs) =
        let f = x.Frame
        let id = x.Id
        if f.PointableList.Count <> 2 then
            false
        else
            let finger =
                f.PointableList.Values
                |> Seq.maxBy (fun y -> y.Length)
            let angle_x = finger.Direction.AngleTo(vectorX) * (180.f / (float32)System.Math.PI)
            let angle_y = finger.Direction.AngleTo(vectorY) * (180.f / (float32)System.Math.PI)
            (50.f < angle_x) && (angle_x > 70.f) && (80.f < angle_y) && (angle_y > 100.f)

    let movefingerleft (x:LeapEventArgs) =
        let f = x.Frame
        let id = x.Id
        if f.PointableList.Count <> 2 then
            false
        else
            let finger =
                f.PointableList.Values
                |> Seq.maxBy (fun y -> y.Length)
            let angle_x = finger.Direction.AngleTo(vectorX) * (180.f / (float32)System.Math.PI)
            let angle_y = finger.Direction.AngleTo(vectorY) * (180.f / (float32)System.Math.PI)
            (105.f < angle_x) && (angle_x > 125.f) && (80.f < angle_y) && (angle_y > 100.f)

    let movefingerup1 (x:LeapEventArgs) =
        let f = x.Frame
        let id = x.Id
        if f.PointableList.Count <> 2 then
            false
        else
            let finger =
                f.PointableList.Values
                |> Seq.maxBy (fun y -> y.Length)
            let angle = finger.Direction.AngleTo(vectorY) * (180.f / (float32)System.Math.PI)
            (105.f < angle) && (angle > 125.f)

    let movefingerdown1 (x:LeapEventArgs) =
        let f = x.Frame
        let id = x.Id
        if f.PointableList.Count <> 2 then
            false
        else
            let finger =
                f.PointableList.Values
                |> Seq.maxBy (fun y -> y.Length)
            let angle = finger.Direction.AngleTo(vectorY) * (180.f / (float32)System.Math.PI)
            (50.f < angle) && (angle > 70.f)

    (*
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
    *)
    let openhand (x:LeapEventArgs) =
        let f = x.Frame
        f.HandList.Count = 1 && f.PointableList.Count >= 4

    let closehandframe (f:ClonableFrame) =
        f.HandList.Count = 1 && f.PointableList.Count <= 1

    let closehand (x:LeapEventArgs) =
        closehandframe (x.Frame)

    let keepclosed (x:LeapEventArgs) =
            let latestFrames =
                frameQueue
                |> Seq.filter (fun y -> y.Timestamp >= x.Frame.Timestamp - 100000L)
            if Seq.length latestFrames = 0 then
                false
            else
                latestFrames
                |> Seq.forall (fun y -> y.HandList.ContainsKey(x.Id) && (closehandframe y) )

//    let timedevent (p:Predicate<LeapEventArgs>) refts thresh (x:LeapEventArgs) =
//        (p.Invoke(x)) && x.Frame.Timestamp - !refts < thresh
    let timedevent p refts thresh (x:LeapEventArgs) =
        let f = x.Frame
        (p x) && x.Frame.Timestamp - !refts < thresh

    let closetimedhand = timedevent closehand ts_openedhand 150000L
    let opentimedhand = timedevent openhand ts_closedhand 150000L

    let pointableCountIs n =
        new Predicate<LeapEventArgs>(fun x -> x.Frame.PointableList.Count = n)
    (* Useless GroundTerms definitions *)
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
    let nonvedodito0 = new GroundTerm<_,_>(LeapFeatureTypes.NotActiveFinger, pointableCountIs 0)
    let movefinger1 = new GroundTerm<_,_>(LeapFeatureTypes.MoveFinger, p)
    (*  GroundTerms definitions *)
    let openedhand1 = new GroundTerm<_,_>(LeapFeatureTypes.MoveHand, openhand)
    let closedhand1 = new GroundTerm<_,_>(LeapFeatureTypes.MoveHand, closetimedhand)
    let keepclosedhand = new GroundTerm<_,_>(LeapFeatureTypes.MoveHand, keepclosed)

    let closedhand2 = new GroundTerm<_,_>(LeapFeatureTypes.MoveHand, closehand)
    let openedhand2 = new GroundTerm<_,_>(LeapFeatureTypes.MoveHand, opentimedhand)
    
    let movefingerup (x:LeapEventArgs) = false
    let movefingerdown (x:LeapEventArgs) = false
    let movedfingerright = new GroundTerm<_,LeapEventArgs>(LeapFeatureTypes.MoveFinger, movefingerright)
    let movedfingerleft = new GroundTerm<_,LeapEventArgs>(LeapFeatureTypes.MoveFinger, movefingerleft)
    let movedfingerup = new GroundTerm<_,LeapEventArgs>(LeapFeatureTypes.MoveFinger, movefingerup)
    let movedfingerdown = new GroundTerm<_,LeapEventArgs>(LeapFeatureTypes.MoveFinger, movefingerdown)

//    let movedhandright = new GroundTerm<_,_>(LeapFeatureTypes.MoveHand, movehandright)
//    let movedhandleft = new GroundTerm<_,_>(LeapFeatureTypes.MoveHand, movehandleft)
    let pushedhanddown = new GroundTerm<_,_>(LeapFeatureTypes.MoveHand, pushhanddown)

    let s1 = new Sequence<_,_>(openedhand1, closedhand1, keepclosedhand)
    openedhand1.Gesture.Add(fun (sender,e) -> ts_openedhand := e.Event.Frame.Timestamp)
    let net1 = s1.ToGestureNet(s)
    s1.Gesture.Add(fun _ -> printfn "chiudi menu"; SendKeys.SendWait("^{ESC}"))

    let s2 = new Sequence<_,_>(closedhand2, openedhand2)
    closedhand2.Gesture.Add(fun (sender,e) -> ts_closedhand := e.Event.Frame.Timestamp)
    let iterr = new Iter<_,_>(movedfingerright)
    let iterl = new Iter<_,_>(movedfingerleft)
    let iteru = new Iter<_,_>(movedfingerup)
    let iterd = new Iter<_,_>(movedfingerdown)
    let ch1 = new Choice<_,_>(iterr, iterl, iteru, iterd)
    
    let s22 = new Sequence<_,_>(s2, ch1)
    let s222 = new Choice<_,_>(s22, pushedhanddown)
    let net222 = s222.ToGestureNet(s)
    s2.Gesture.Add(fun _ -> printfn "apri menu"; SendKeys.SendWait("^{ESC}"))
    iterr.Gesture.Add(fun _ -> printfn "vai a dx";  SendKeys.SendWait("{RIGHT 1}"))
    iterl.Gesture.Add(fun _ -> printfn "vai a sx";  SendKeys.SendWait("{LEFT 1}"))
    iteru.Gesture.Add(fun _ -> printfn "vai su";  SendKeys.SendWait("{UP 1}"))
    iterd.Gesture.Add(fun _ -> printfn "vai giù";  SendKeys.SendWait("{DOWN 1}"))
    s222.Gesture.Add(fun (sender,e) -> printfn "select"; 
                                       lastEnter <- e.Event.Frame.Timestamp
                                       SendKeys.SendWait("{ENTER}")
                    )
    
    (* Gesture definition 
    let s1 = new Sequence<_,_>(vedodito1, vedodito2)
    let s2 = new Sequence<_,_>(s1, vedodito3)
    let s3 = new Sequence<_,_>(s2, vedodito4)
    let s4 = new Sequence<_,_>(s3, vedodito5)
    let m1 = new Sequence<_,_>(s4, apromano)
    m1.Gesture.Add(fun _ -> SendKeys.SendWait("^{ESC}"))
    let net1 = m1.ToGestureNet(s)
    let seq = new Sequence<_,_>(m1, muovoditodx)
    seq.Gesture.Add(fun _ -> SendKeys.SendWait("{RIGHT 10}")) *)
(*
    let s6 = new Sequence<_,_>(nonvedodito4, nonvedodito3)
    let s7 = new Sequence<_,_>(s6, nonvedodito2)
    s7.Gesture.Add(fun _ -> SendKeys.SendWait("{ESC}"))
    let netdestocazzo2 = s7.ToGestureNet(s)
*)
    (*let net1 = apromano.ToGestureNet(s)
    net1.Completion.Add(fun _ ->
                            SendKeys.SendWait("^{ESC}")
                            printfn ("[OPEN HAND]")) // dovrebbe aprirsi il menu grosso di win8
                            
    let net2 = chiudomano.ToGestureNet(s)
    net1.Completion.Add(fun _ ->
                            SendKeys.SendWait("^{F4}")
                            printfn ("[CLOSE HAND]")) // dovrebbe chiudere la finestra corrente
    *)

    (* Sensor *)
    let UpdateInformations (f:ClonableFrame, e:LeapFeatureTypes, id:FakeId) =
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