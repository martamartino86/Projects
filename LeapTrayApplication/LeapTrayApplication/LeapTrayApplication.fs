// Ulteriori informazioni su F# all'indirizzo http://fsharp.net
// Per ulteriori informazioni, vedere il progetto 'Esercitazione su F#'.
module LeapTrayApplication
    open System.Windows.Forms
    open System.Drawing
    open System.Collections.Generic
    open System.Diagnostics
    open GestIT
    open ClonableLeapFrame
    open LeapDriver
    
    type TrayApplication () =
        inherit Form()

        let mutable trayMenu = null
        let mutable trayIcon = null
        (* Structures *)
        let s = new LeapDriver.LeapSensor()
        let frameQueue = new Queue<ClonableFrame>()
        let mutable lastFrameInQueue = new ClonableFrame() // it represents the last enqueued frame
        let vectorX = new Leap.Vector((float32)1, (float32)0, (float32)0)
        let vectorY = new Leap.Vector((float32)0, (float32)(-1),(float32) 0)
        let vectorZ = new Leap.Vector((float32)0, (float32)0, (float32)(-1))
        (* Timestamps *)
        let ts_openedhand = ref(-1L : TimeStamp)
        let ts_closedhand = ref(-1L : TimeStamp)
        let mutable lastEnter:TimeStamp = -1L
        let mutable lastFingerLeft:TimeStamp = -1L
        let mutable lastFingerRight:TimeStamp = -1L
        let mutable lastFingerUp:TimeStamp = -1L
        let mutable lastFingerDown:TimeStamp = -1L
        let threshpointfinger:TimeStamp = 300000L
        let mutable minX = new ClonableFrame()
        let mutable maxY = new ClonableFrame()

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

        let movefistdownright (x:LeapEventArgs) =
            let f = x.Frame
            let id = x.Id
            if f.PointableList.Count > 2 || f.HandList.Count <> 1 then
                Debug.WriteLine("{0} dita, {1} mani", f.PointableList.Count, f.HandList.Count)
                false
            else
                // studio il movimento su quasi tutta la coda: in 1/10 di secondo devo fare il movimento "scendi col pugno"
                let coda =
                    frameQueue
                    |> Seq.filter (fun y -> y.HandList.ContainsKey(id) && y.Timestamp >= f.Timestamp - 150000L)
                //Debug.WriteLine("ci sono {0} frame in coda", Seq.length coda)
                if coda |> Seq.isEmpty then
                    Debug.WriteLine("coda empty")
                    false
                else
                    let maxY =
                        coda
                        |> Seq.maxBy (fun y -> y.HandList.[id].Position.y)
                    minX <-
                        coda
                        |> Seq.minBy (fun q -> q.HandList.[id].Position.x)
                    let c1 =
                        coda
                        |> Seq.filter (fun z -> z.Timestamp >= maxY.Timestamp)
                    if Seq.length c1 < 2 then
                        false
                    else
                        let c2 =
                            c1
                            |> Seq.pairwise
                            |> Seq.forall (fun (u1,u2) ->
                                                          let p1 = u1.HandList.[id].Position
                                                          let p2 = u2.HandList.[id].Position
                                                          //Debug.WriteLine ("Y1: {0} {2} Y2: {1} {3}", p1.y, p2.y, u1.Timestamp, u2.Timestamp)
                                                          Debug.Flush()
                                                          p1.x < p2.x && p1.y > p2.y
                            )
                        let passedbyorigin =
                            c1
                            |> Seq.exists (fun v -> let xx = v.HandList.[id].Position
                                                    xx.x >= -5.f && xx.x <= 5.f
                            )
                        //Debug.WriteLine("ERGO: {0} {1}", c2, passedbyorigin)
                        c2 && passedbyorigin

        let movefistupright (x:LeapEventArgs) =
            let thresh = 50.f
            let f = x.Frame
            let id = x.Id
            if f.PointableList.Count > 2 || f.HandList.Count <> 1 then
                Debug.WriteLine("{0} dita, {1} mani", f.PointableList.Count, f.HandList.Count)
                false
            else
                let coda =
                    frameQueue
                    |> Seq.filter (fun y -> y.HandList.ContainsKey(id) && y.Timestamp >= f.Timestamp - 150000L)
                if not (minX.HandList.ContainsKey(id)) || f.HandList.[id].Position.x < minX.HandList.[id].Position.x then
                    Debug.WriteLine("nuovo min x: {0}", f.HandList.[id].Position.x)
                    minX <- f
                    false
                else
                    if coda |> Seq.isEmpty then
                        Debug.WriteLine("coda empty")
                        false
                    else
                        maxY <- 
                            coda
                            |> Seq.maxBy (fun y -> y.HandList.[id].Position.y)
                        coda
                        |> Seq.filter (fun z -> z.Timestamp >= minX.Timestamp)
                        |> Seq.pairwise
                        |> Seq.forall (fun (u1,u2) -> let p1 = u1.HandList.[id].Position
                                                      let p2 = u2.HandList.[id].Position
                                                      //Debug.WriteLine ("{0} {1} - {2}", p1, p2, (p1.x > p2.x && p1.y < p2.y))
                                                      p1.x < p2.x && p1.y < p2.y
                        )

        let pushhanddown (x:LeapEventArgs) =
            let thresh = 50.f
            let f = x.Frame
            if (lastEnter >= f.Timestamp - 1000000L) || (f.PointableList.Count < 4) then
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

        let movefingerleft (x:LeapEventArgs) =
            let f = x.Frame
            let id = x.Id
            if f.PointableList.Count > 2 || f.PointableList.Count = 0 || f.Timestamp - lastFingerLeft < threshpointfinger then
                false
            else
                let finger =
                    f.PointableList.Values
                    |> Seq.maxBy (fun y -> y.Length)
                finger.Position.x <= -60.f
                   
        let movefingerright (x:LeapEventArgs) =
            let f = x.Frame
            let id = x.Id
            if f.PointableList.Count > 2 || f.PointableList.Count = 0 || f.Timestamp - lastFingerRight < threshpointfinger then
                false
            else
                let finger =
                    f.PointableList.Values
                    |> Seq.maxBy (fun y -> y.Length)
                finger.Position.x >= 50.f

        let movefingerup (x:LeapEventArgs) =
            let f = x.Frame
            let id = x.Id
            if f.PointableList.Count > 2 || f.PointableList.Count = 0 || f.Timestamp - lastFingerUp < threshpointfinger then
                false
            else
                let finger =
                    f.PointableList.Values
                    |> Seq.maxBy (fun y -> y.Length)
                finger.Position.y >= 210.f

        let movefingerdown (x:LeapEventArgs) =
            let f = x.Frame
            let id = x.Id
            if f.PointableList.Count > 2 || f.PointableList.Count = 0 || f.Timestamp - lastFingerDown < threshpointfinger then
                false
            else
                let finger =
                    f.PointableList.Values
                    |> Seq.maxBy (fun y -> y.Length)
                finger.Position.y <= 170.f

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
    
        let movedfingerright = new GroundTerm<_,LeapEventArgs>(LeapFeatureTypes.MoveFinger, movefingerright)
        let movedfingerleft = new GroundTerm<_,LeapEventArgs>(LeapFeatureTypes.MoveFinger, movefingerleft)
        let movedfingerup = new GroundTerm<_,LeapEventArgs>(LeapFeatureTypes.MoveFinger, movefingerup)
        let movedfingerdown = new GroundTerm<_,LeapEventArgs>(LeapFeatureTypes.MoveFinger, movefingerdown)
        let pushedhanddown = new GroundTerm<_,_>(LeapFeatureTypes.MoveHand, pushhanddown)
        
        let movedfistdownright = new GroundTerm<_,_>(LeapFeatureTypes.MoveHand, movefistdownright)
        let movedfistupright = new GroundTerm<_,_>(LeapFeatureTypes.MoveHand, movefistupright)
        let s1 = new Sequence<_,_>(movedfistdownright, movedfistupright)
        let net = s1.ToGestureNet(s)
        (*
        let s1 = new Sequence<_,_>(openedhand1, closedhand1, keepclosedhand)
        let net1 = s1.ToGestureNet(s)

        let s2 = new Sequence<_,_>(closedhand2, openedhand2)
        let iterr = new Iter<_,_>(movedfingerright)
        let iterl = new Iter<_,_>(movedfingerleft)
        let iteru = new Iter<_,_>(movedfingerup)
        let iterd = new Iter<_,_>(movedfingerdown)
        let ch1 = new Choice<_,_>(iterr, iterl, iteru, iterd)
    
        let s22 = new Sequence<_,_>(s2, ch1)
        let s222 = new Choice<_,_>(s22, pushedhanddown)
        let net222 = s222.ToGestureNet(s)
        *)
        do
            trayMenu <- new ContextMenu()
            trayIcon <- new NotifyIcon()
            trayIcon.Text <- "MyTrayApp";
            trayIcon.Icon <- new Icon(SystemIcons.Application, 40, 40);
            trayIcon.ContextMenu <- trayMenu;
            trayIcon.Visible <- true;

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

        override x.OnLoad(e:System.EventArgs) =
            x.Visible <- false
            trayIcon.Visible <- true
            x.ShowInTaskbar <- false; // Remove from taskbar.
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
                    openedhand1.Gesture.Add(fun (sender,e) -> ts_openedhand := e.Event.Frame.Timestamp)
            )
            movedfistdownright.Gesture.Add(fun _ -> Debug.WriteLine "*** RECOGNIZED DOWN ***")
            movedfistupright.Gesture.Add(fun _ -> Debug.WriteLine "*** RECOGNIZED UP ***")

            (*
            s1.Gesture.Add(fun _ -> printfn "chiudi menu"; SendKeys.SendWait("{ESC}"))
            closedhand2.Gesture.Add(fun (sender,e) -> ts_closedhand := e.Event.Frame.Timestamp)
            s2.Gesture.Add(fun (sender,e) -> printfn "apri menu"; SendKeys.SendWait("^{ESC}"))
            iterr.Gesture.Add(fun (sender,e) -> printfn "***************RIGHT"
                                                lastFingerRight <- e.Event.Frame.Timestamp
                                                SendKeys.SendWait("{RIGHT 1}"))
            iterl.Gesture.Add(fun (sender,e) -> printfn "***************LEFT";
                                                lastFingerLeft <- e.Event.Frame.Timestamp
                                                SendKeys.SendWait("{LEFT 1}"))
            iteru.Gesture.Add(fun (sender,e) -> printfn "***************UP";
                                                lastFingerUp <- e.Event.Frame.Timestamp
                                                SendKeys.SendWait("{UP 1}"))
            iterd.Gesture.Add(fun (sender,e) -> printfn "***************DOWN";
                                                lastFingerDown <- e.Event.Frame.Timestamp

                                                SendKeys.SendWait("{DOWN 1}"))
            s222.Gesture.Add(fun (sender,e) -> printfn "select"; 
                                               lastEnter <- e.Event.Frame.Timestamp
                                               SendKeys.SendWait("{ENTER}"))
            *)
            trayIcon.DoubleClick.Add(fun _ ->
                                            if x.Visible = true then
                                                x.Visible <- false
                                            else
                                                x.Visible <- true
                                            x.Invalidate()
                                    )

        override x.OnClosing(e:System.ComponentModel.CancelEventArgs) =
            trayIcon.Dispose()
            Application.Exit()

    [<EntryPoint>](*; System.STAThread>*)
    let main argv = 
        let a = new TrayApplication()
        Application.Run(a)
        System.Console.ReadLine() |> ignore
        0
