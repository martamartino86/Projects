// Ulteriori informazioni su F# all'indirizzo http://fsharp.net
// Per ulteriori informazioni, vedere il progetto 'Esercitazione su F#'.
module Program
    open System.Collections.Generic
    open System.Diagnostics
    open GestIT
    open ClonableLeapFrame
    open LeapDriver
    open System.IO

    type Puppa (fileName:string) =
      let f = (File.Open(fileName, FileMode.Create, FileAccess.Write))
      let formatter = new System.Runtime.Serialization.Formatters.Binary.BinaryFormatter()
      
      member x.Write(e:SensorEventArgs<LeapFeatureTypes, LeapEventArgs>) =
        if f.CanWrite then formatter.Serialize(f, (System.DateTime.Now, e.FeatureType, e.Event))
        
      interface System.IDisposable with
        member x.Dispose () = f.Close()

    
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
    let mutable lastHandRight:TimeStamp = -1L
    let mutable lastHandLeft:TimeStamp = -1L
    let threshpointfinger:TimeStamp = 300000L
    let mutable minX_glob = new ClonableFrame()
    let mutable maxY_glob = new ClonableFrame()

    let queueIDs = new List<FakeId>()
    
    (* Predicates *)
    let speed (x:float32) (y:float32) = x / y
    let p = new Predicate<LeapEventArgs>(fun x -> true)
    
    let movehand (dir:int) (i:int) (x:LeapEventArgs) = // dir = 0 per DOWN, dir = 1 per UP
        let f = x.Frame
        if queueIDs.Count <= i then false
        else
            let id = queueIDs.[i] //x.Id
            if f.HandList.Count <> 2 then
                false
            else
                let o = x.Frame.HandList.[id].Position
                let coda =
                    frameQueue
                    |> Seq.filter (fun y -> y.HandList.ContainsKey(id) && y.Timestamp >= f.Timestamp - 100000L)
                if coda |> Seq.isEmpty then
                    false
                else
                    if dir = 0 then // DOWN
                        let maxY =
                            coda
                            |> Seq.maxBy (fun z -> z.HandList.[id].Position.y)
                        if maxY.HandList.[id].Position.y - o.y > 20.f then
                            coda
                            |> Seq.filter (fun z -> z.Timestamp >= maxY.Timestamp)
                            |> Seq.forall (fun z -> z.HandList.[id].Position.y <= maxY.HandList.[id].Position.y)
                        else
                            false
                    else if dir = 1 then // UP
                        let minY =
                            coda
                            |> Seq.minBy (fun z -> z.HandList.[id].Position.y)
                        if o.y - minY.HandList.[id].Position.y > 20.f then
                            coda
                            |> Seq.filter (fun z -> z.Timestamp <= minY.Timestamp)
                            |> Seq.forall (fun z -> z.HandList.[id].Position.y >= minY.HandList.[id].Position.y)
                        else
                            false
                    else false

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
                if maxY.HandList.[id].Position.y - o.y > 80.f then
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

    let pointableCountIs n =
        new Predicate<LeapEventArgs>(fun x -> x.Frame.PointableList.Count = n)

    let vedomani (n:int) (x:LeapEventArgs) =
        x.Frame.HandList.Count = n
    let nomano (x:LeapEventArgs) =
        true

    (*  GroundTerms definitions *)
    let vedomani1 = new GroundTerm<_,_>(LeapFeatureTypes.ActiveHand, vedomani 2)
    let perdomano = new GroundTerm<_,_>(LeapFeatureTypes.NotActiveHand, nomano)

    let muovosu1 = new GroundTerm<_,LeapEventArgs>(LeapFeatureTypes.MoveHand, movehand 1 0)     // primo arg: UP - secondo arg: ID in queue
    let muovogiu2 = new GroundTerm<_,LeapEventArgs>(LeapFeatureTypes.MoveHand, movehand 0 1)    // primo arg: DOWN - secondo arg: ID in queue
    let muovosu2 = new GroundTerm<_,LeapEventArgs>(LeapFeatureTypes.MoveHand, movehand 1 1)     // idem
    let muovogiu1 = new GroundTerm<_,LeapEventArgs>(LeapFeatureTypes.MoveHand, movehand 0 0)    // idem
    let pp1 = new Parallel<_,_>(muovosu1, muovogiu2)
    let pp2 = new Parallel<_,_>(muovosu2, muovogiu1)
    let c1 = new Choice<_,_>(pp1, pp2)
//    let it = new Iter<_,_>(c1)

    let seq = new Sequence<_,_>(vedomani1, c1)//it)
    let pp3 = new Parallel<_,LeapEventArgs>(muovogiu1, muovogiu1)

    let ch = new Choice<_,_>(seq, perdomano)

    let ff = new Puppa(@"C:\Users\Pc\Desktop\output.ser")
    //let f = (File.OpenWrite(@"C:\Users\Pc\Documents\Visual Studio 2012\Projects\LeapTrayApplication - Rock Paper Scissor\LeapTrayApplication\output.ser"))
    let net = ch.ToGestureNet(s)
    //net.Stream <- f
    printfn "NET HASHCODE: %A" (net.GetHashCode())

    vedomani1.Gesture.Add(fun (sender,e) -> printfn ("vedo 2 mani"); queueIDs.Add(e.Event.Id))
    c1.Gesture.Add(fun (sender,e) -> printfn("~ ciao ciao! "))
    //ch.Gesture.Add(fun (sender,e) -> printfn("ahi ahi ahi... scazzato!"))
    perdomano.Gesture.Add(fun (sender,e) -> if queueIDs.Remove(e.Event.Id) then printfn "persa mano - fine male gesture" )
    (* Sensor *)
    let UpdateInformations (f:ClonableFrame, e:LeapFeatureTypes, id:FakeId) =
        (* Update informations in the last enqueued frame *)
        match e with
            | LeapFeatureTypes.ActiveHand -> lastFrameInQueue.HandList.Add(id, f.HandList.[id].Clone())
            | LeapFeatureTypes.ActiveFinger | LeapFeatureTypes.ActiveTool -> lastFrameInQueue.PointableList.Add(id, f.PointableList.[id].Clone())
            | LeapFeatureTypes.MoveHand -> lastFrameInQueue.HandList.[id] <- f.HandList.[id].Clone()
            | LeapFeatureTypes.MoveFinger | LeapFeatureTypes.MoveTool -> lastFrameInQueue.PointableList.[id] <- f.PointableList.[id].Clone()
            | LeapFeatureTypes.NotActiveHand -> lastFrameInQueue.HandList.Remove(id) |> ignore; queueIDs.Clear()
            | LeapFeatureTypes.NotActiveFinger | LeapFeatureTypes.NotActiveTool -> lastFrameInQueue.PointableList.Remove(id) |> ignore
            | _ -> ()

    let formatter = new System.Runtime.Serialization.Formatters.Binary.BinaryFormatter()

    (s :> ISensor<_,_>).SensorEvents.Add(fun e ->
        
        ff.Write(e)

        Debug.WriteLine("ricevo dati; net = {0}", net)
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

    System.Console.ReadLine() |> ignore