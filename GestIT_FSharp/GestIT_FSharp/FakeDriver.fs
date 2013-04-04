module FakeDriver

    open System.Windows.Forms
    open System.Drawing
    open System.Collections.Generic
    open Microsoft.FSharp.Reflection
    open MyFrame
    open GestIT
    open Leap
    open System.IO

    type TimeStamp = int64

    type IStateCleaner<'T,'U> =
        (* Clears the history preceding a timestamp, returns the elements that are being dropped *)
        abstract member TruncateHistory: TimeStamp -> 'T list

        (* Predicts the model state at the given timestamp, as an evolution of the currently known information *)
        abstract member Predict: TimeStamp -> unit

        (*
         * Corrects the current state using the new information, returns:
         * (true, x) if the model was extended with new information
         * (false, x) if the model was simply updated with the new information
         * x is always the model representation of the new information
         *)
        abstract member Correct: 'U -> bool * 'T

    type MyHandCleaner() =
        interface IStateCleaner<MyHand,Hand> with
            member x.TruncateHistory(t) = []
            member x.Predict(t) = ()
            member x.Correct(f) = (true, new MyHand(f))

    type MyPointableCleaner() =
        interface IStateCleaner<MyPointable,Pointable> with
            member x.TruncateHistory(t) = []
            member x.Predict(t) = ()
            member x.Correct(f) = (true, new MyPointable(f))

    // KEYBOARD features that have to be notified from the sensor //
    type KeyFeatureTypes =
        | KeyDown = 0
        | KeyUp = 1
    
    type KeySensor () =
        inherit UserControl()
        let sensorEvent = new Event<SensorEventArgs<KeyFeatureTypes,KeyEventArgs>>()
        interface ISensor<KeyFeatureTypes,KeyEventArgs> with
            member x. SensorEvents = sensorEvent.Publish

        override x.OnKeyDown(e) =
            printfn "SPACE DOWN"
            sensorEvent.Trigger(new SensorEventArgs<_,_>(KeyFeatureTypes.KeyDown, e))

        override x.OnKeyUp(e) =
            printfn "SPACE UP"
            sensorEvent.Trigger(new SensorEventArgs<_,_>(KeyFeatureTypes.KeyUp, e))

    // MOUSE features that have to be notified from the sensor //
    type MouseFeatureTypes =
        | MouseDown = 0
        | MouseUp = 1
        | MouseMove = 2

    type MouseSensor (debug) =
        inherit UserControl()
        let mutable down = false
        let sensorEvent = new Event<SensorEventArgs<MouseFeatureTypes,MouseEventArgs>>()
        interface GestIT.ISensor<MouseFeatureTypes,MouseEventArgs> with
            member x.SensorEvents = sensorEvent.Publish

        override x.OnMouseDown(e) =
            if debug then printfn "MOUSE DOWN"
            sensorEvent.Trigger(new SensorEventArgs<_,_>(MouseFeatureTypes.MouseDown, e))
            down <- true

        override x.OnMouseUp(e) =
            if debug then printfn "MOUSE UP"
            sensorEvent.Trigger(new SensorEventArgs<_,_>(MouseFeatureTypes.MouseUp, e))
            down <- false

        override x.OnMouseMove(e) =
            if down then
                if debug then printfn "MOUSE MOVE"
                sensorEvent.Trigger(new SensorEventArgs<_,_>(MouseFeatureTypes.MouseMove, e))

    type TouchFeatureTypes =
        | Start = 0
        | Move = 1
        | End = 2


        
// *** LEAP *** //
 
    
    let outfile = File.CreateText("dataleap.txt")
  
    // devo tenermi traccia di cosa ho visto nel frame (precedente): tengo dunque un dizionario in cui la Key e' l'ID
    // (NB: ID e' un campo unico per ogni frame) e il valore è il Frame stesso.
    // In tal modo, quando ricevo un nuovo frame, controllo che gli ID:
    // * non siano presenti -> mando Active
    // * siano già presenti -> mando Move
    // * non siano più presenti -> mando NotActive
    type LeapInfoType =
        | Hand
        | HandZombie
        | Finger
        | FingerZombie
        | Tool
        | ToolZombie
    
    let mutable lastFrame:MyFrame = new MyFrame(Frame.Invalid)
    let activeHands = new Dictionary<FakeId, int64 (* timestamp *)>()
    let activePointables = new Dictionary<FakeId, int64 (* timestamp *)>()
    let leapToFakeMap = new Dictionary<LeapId, FakeId>()
    let fakeToLeapMap = new Dictionary<FakeId, LeapId>()

    let delta = 0.25F
    let epsilon = 1000.0f* 1.5F

    let checkDistanceHands (h1:MyHand) (h2:MyHand) =
//        let delta_s = System.Math.Abs (h1.Position - h2.Position).Magnitude
//        let delta_t = (float32)(currenttimestamp - activeHands.[leapToFakeMap.[h1.Id]]) / 1000.F
        (h1.Position - h2.Position).Magnitude < 100.F

    let checkDistanceFingers (p1:MyPointable) (p2:MyPointable) =
        printfn "distanza: %s " (((p1.Position - p2.Position).Magnitude).ToString())
        //printfn "%A %A" p1.Position (p2.Position)
        (p1.Position - p2.Position).Magnitude < 50.F
    
    let isTheSameHand (h1:MyHand) (h2:MyHand) =
        checkDistanceHands h1 h2

    let isTheSameFinger (f1:MyPointable) (f2:MyPointable) =
        //printfn "diff length: %f " (System.Math.Abs(f1.Length - f2.Length))
        //printfn "diff width: %f " (System.Math.Abs(f1.Width - f2.Width))
        (System.Math.Abs(f1.Length - f2.Length) < epsilon) && (System.Math.Abs(f1.Width - f2.Width) < epsilon) && (checkDistanceFingers f1 f2)

    type LeapFeatureTypes =
        | ActiveHand = 0
        | ActiveFinger = 1
        | ActiveTool = 2
        | MoveHand = 3
        | MoveFinger = 4
        | MoveTool = 5
        | NotActiveHand = 6
        | NotActiveFinger = 7
        | NotActiveTool = 8

    // Evento contenente il frame corrente e l'ID dell'oggetto a cui si riferisce la feature.
    type LeapEventArgs(f:MyFrame, id:int) =
        inherit System.EventArgs()
        // Oggetto Frame corrente.
        member this.Frame = f
        // ID dell'oggetto a cui si riferisce la feature (es. ID di un Hand / Finger / Tool).
        member this.Id = id

    type LeapSensor () as this =
        inherit Leap.Listener()
        let handCleaner = new MyHandCleaner() :> IStateCleaner<_,_>
        let pointableCleaner = new MyPointableCleaner() :> IStateCleaner<_,_>
        let counterBase = 10000000
        let ctrl = new Controller()
        let mutable fakeIdCounter = counterBase
        let sensorEvent = new Event<SensorEventArgs<LeapFeatureTypes, LeapEventArgs>>()
        do
            ctrl.AddListener(this) |> ignore
        member this.allocateNewFakeId() =
            fakeIdCounter <- fakeIdCounter + 1
            fakeIdCounter
        member this.Controller = ctrl
        interface ISensor<LeapFeatureTypes,LeapEventArgs> with
            member x.SensorEvents = sensorEvent.Publish
        override this.OnInit(c:Controller) =
            System.Console.WriteLine "OnInit"
        override this.OnConnect(c:Controller) =
            System.Console.WriteLine "OnConnect"
        override this.OnFrame(c:Controller) =
            let frame = c.Frame()
            let currenttimestamp = frame.Timestamp

            let removedHands = handCleaner.TruncateHistory(currenttimestamp)
            let removedPointables = pointableCleaner.TruncateHistory(currenttimestamp)
            // TODO: notify removed

            handCleaner.Predict(currenttimestamp)
            pointableCleaner.Predict(currenttimestamp)

            for h in frame.Hands do
                let isNew,hand = handCleaner.Correct(h)
                // TODO: notify new ...
                ()

            for p in frame.Pointables do
                let isNew,pointable = pointableCleaner.Correct(p)
                // TODO: notify new ...
                ()


//            outfile.Write("{0}, ", currenttimestamp)
//            outfile.Flush()

            lastFrame.Timestamp <- currenttimestamp
            // Rimuovo gli oggetti piu' vecchi del limite (cosi' non rischio di trovare un match con loro)
            let minAcceptableTimeStamp = currenttimestamp - 2000000L

            this.Check()

            let remove =
                activePointables
                |> Seq.filter (fun x -> x.Value < minAcceptableTimeStamp)
                |> Seq.map (fun x -> x.Key)
                |> Seq.toArray

            for fakeId in remove do
                printfn "remove id: %d " fakeId
                let isFinger = lastFrame.PointableList.[fakeId].IsFinger
                let evtType = if isFinger then LeapFeatureTypes.NotActiveFinger else LeapFeatureTypes.NotActiveTool
                activePointables.Remove(fakeId) |> ignore
                lastFrame.PointableList.Remove(fakeId) |> ignore
                leapToFakeMap.Remove(fakeToLeapMap.[fakeId]) |> ignore
                fakeToLeapMap.Remove(fakeId) |> ignore
                let e = new LeapEventArgs(lastFrame, fakeId)
                sensorEvent.Trigger(new SensorEventArgs<_,_>(evtType, e))

            this.Check()

            let remove =
                activeHands
                |> Seq.filter (fun x -> x.Value < minAcceptableTimeStamp)
                |> Seq.map (fun x -> x.Key)
                |> Seq.toArray

            for fakeId in remove do
                // imposto a -1 gli IdHand dei Pointables che si tengono questa hand come mano
                let pointables =
                    lastFrame.PointableList
                    |> Seq.filter ( fun x -> x.Value.IdHand = fakeId )
                    |> Seq.toArray
                for p in pointables do
                    lastFrame.PointableList.[p.Key] <- p.Value.Rename(p.Key, -1)
                // rimozione hand
                activeHands.Remove(fakeId) |> ignore
                lastFrame.HandList.Remove(fakeId) |> ignore
                leapToFakeMap.Remove(fakeToLeapMap.[fakeId]) |> ignore
                fakeToLeapMap.Remove(fakeId) |> ignore
                let e = new LeapEventArgs(lastFrame, fakeId)
                sensorEvent.Trigger(new SensorEventArgs<_,_>(LeapFeatureTypes.NotActiveHand, e))

            this.Check()

            // faccio le STIME delle NUOVE POSIZIONI e le salvo in lastFrame
            for h in lastFrame.HandList do
                let ts = activeHands.[h.Key] // hand timestamp
                let hand = lastFrame.HandList.[h.Key]
                hand.Position <- hand.Position (* mm *) + hand.Velocity (* mm/s *) * ((float32)(currenttimestamp (* us *) - activeHands.[h.Key]) * (1.e-6f))
            for p in lastFrame.PointableList do
                let ts = activePointables.[p.Key] // pointable timestamp
                let pntb = p.Value
                pntb.Position <- pntb.Position + pntb.Velocity * ((float32)(currenttimestamp - ts) * (1.e-6f))

            this.Check()

            // mi salvo il nuovo frame
            let newFrame = new MyFrame(frame)

            // controllo che le mani del nuovo frame siano già presenti, ed aggiorno le loro informazioni (id, timestamp, lastframe.handbla) + triggero l'evento .....
            for h in newFrame.HandList.Values do
//                outfile.Write("{0}, ", h.Position.x.ToString().Replace(",", "."))
                if leapToFakeMap.ContainsKey(h.Id) then
                    // se l'ho trovato, mando l'evento sopra (con l'ID fake!) e salvo lo stato in lastFrame aggiungendoci la mano (con id fake)
                    let fakeId = leapToFakeMap.[h.Id]
                    if isTheSameHand h lastFrame.HandList.[fakeId] then
                        activeHands.Item(fakeId) <- currenttimestamp
                        lastFrame.HandList.Item(fakeId) <- h.Rename(fakeId)
                        let e = new LeapEventArgs(lastFrame, fakeId)
                        sensorEvent.Trigger(new SensorEventArgs<_,_>(LeapFeatureTypes.MoveHand, e))
                   else
                        printfn "Stesso ID ma non la vede come stessa mano -__-"

            this.Check()

            // ..... ora invece controllo quelle che NON sono presenti nell'activeHands, ma che ho trovato nel frame appena giunto
            for h in newFrame.HandList.Values do
                if not (leapToFakeMap.ContainsKey(h.Id)) then
                    // arriva un nuovo ID: controlla che non sia uno zombie!
                    let zombielist =
                        activeHands
                        |> Seq.filter (fun x -> x.Value < currenttimestamp)
                        |> Seq.map (fun x -> x.Key)
                        |> Seq.toArray
                    try
                        let zombie =
                            zombielist
                            |> Seq.filter (fun x -> isTheSameHand lastFrame.HandList.[x] h (*lastFrame.HandList.[x].Position h.Position*))
                            |> Seq.head
                        // (se non solleva eccezione, ha trovato uno zombie che matcha con la nuova mano!)
                        // cambio l'associazione nella leapToFakeMap, mantenendo come value l'ID dello zombie e come nuova chiave l'ID che arriva dal Leap
                        printfn "HO TROVATO LO ZOMBO! %d" zombie
                        leapToFakeMap.Remove(fakeToLeapMap.[zombie]) |> ignore
                        leapToFakeMap.Add(h.Id, zombie)
                        fakeToLeapMap.Item(zombie) <- h.Id
                        // le informazioni che gli mando (posizione, ecc) rimangono le stesse di prima
                        activeHands.Item(zombie) <- currenttimestamp
                        lastFrame.HandList.Item(zombie) <- h.Rename(zombie)
                        let e = new LeapEventArgs(lastFrame, zombie)
                        sensorEvent.Trigger(new SensorEventArgs<_,_>(LeapFeatureTypes.MoveHand, e))
                    with
                        // non ho trovato zombie che potrebbero corrispondere al nuovo ID, quindi lo aggiungo a leapToFakeMap e sollevo evento per nuova mano
                        | _ ->  let fakeId = this.allocateNewFakeId()
                                printfn "NON HO TROVATO LO ZOMBO... NEW HAND: %d" fakeId
                                leapToFakeMap.Add(h.Id, fakeId)
                                fakeToLeapMap.Add(fakeId, h.Id)
                                activeHands.Item(fakeId) <- currenttimestamp
                                lastFrame.HandList.Add(fakeId, h.Rename(fakeId))
                                let e = new LeapEventArgs(lastFrame, fakeId)
                                sensorEvent.Trigger(new SensorEventArgs<_,_>(LeapFeatureTypes.ActiveHand, e))
          
            this.Check()

            // ora pero' devo vedere quali tra le activeHands NON sono nel nuovo frame, ovvero sono scomparse ma devo continuare a farle vedere sopra
            // per 2/10 sec! (e ora posso farlo perché quelle troppo vecchie le ho già eliminate in cima)
            for a in activeHands do
                // l'id in activeHands (a.Key) e' fake
                if not (newFrame.HandList.ContainsKey(fakeToLeapMap.[a.Key])) then
                    // NON aggiorno il timestamp ovviamente. continuo a mandare il frame con le informazioni precedenti
                    let e = new LeapEventArgs(lastFrame, a.Key)
                    sensorEvent.Trigger(new SensorEventArgs<_,_>(LeapFeatureTypes.MoveHand, e))

            this.Check()

            // e adesso la parte Pointables!
            for p in newFrame.PointableList.Values do
                if leapToFakeMap.ContainsKey(p.Id) then
                    let fakeId = leapToFakeMap.[p.Id]
                    if isTheSameFinger p lastFrame.PointableList.[fakeId] then
                        activePointables.[fakeId] <- currenttimestamp
                        let fakeIdHand = if p.IdHand <> -1 then
                                            leapToFakeMap.[p.IdHand]
                                         else
                                            -1
                        lastFrame.PointableList.[fakeId] <- p.Rename(fakeId, fakeIdHand)
                        let e = new LeapEventArgs(lastFrame, fakeId)
                        let evttype = if p.IsFinger then LeapFeatureTypes.MoveFinger else LeapFeatureTypes.MoveTool
                        sensorEvent.Trigger(new SensorEventArgs<_,_>(evttype, e))
                    else
                        printfn "Stesso ID ma <>: %A %A %A" p.Position lastFrame.PointableList.[fakeId].Position (p.Position - lastFrame.PointableList.[fakeId].Position)

            this.Check()

            for p in newFrame.PointableList.Values do
                if not (leapToFakeMap.ContainsKey(p.Id)) then
                    let zombielist =
                        activePointables
                        |> Seq.filter (fun x -> x.Value < currenttimestamp)
                        |> Seq.map (fun x -> x.Key)
                        |> Seq.toArray
                    printfn "zombie list: %A" zombielist
                    let evttype = if p.IsFinger then LeapFeatureTypes.MoveFinger else LeapFeatureTypes.MoveTool
                    try
                        let zombie =
                            zombielist
                            |> Seq.filter (fun x -> isTheSameFinger (lastFrame.PointableList.[x]) p )
                            |> Seq.head
                        printfn "zombie id: %d " zombie
                        leapToFakeMap.Remove(fakeToLeapMap.[zombie]) |> ignore
                        leapToFakeMap.Add(p.Id, zombie)
                        fakeToLeapMap.Item(zombie) <- p.Id

                        activePointables.Item(zombie) <- currenttimestamp
                        let fakeIdHand = if p.IdHand <> -1 then
                                            leapToFakeMap.[p.IdHand]
                                         else
                                            -1
                        lastFrame.PointableList.Item(zombie) <- p.Rename(zombie, fakeIdHand)
                        let e = new LeapEventArgs(lastFrame, zombie)
                        sensorEvent.Trigger(new SensorEventArgs<_,_>(evttype, e))
                    with
                        | _ ->
                               let fakeId = this.allocateNewFakeId()
                               printfn "NEW POINTABLE: %d" fakeId
                               leapToFakeMap.Add(p.Id, fakeId)
                               fakeToLeapMap.Add(fakeId, p.Id)
                               activePointables.Item(fakeId) <- currenttimestamp
                               let fakeIdHand = if p.IdHand <> -1 then
                                                    leapToFakeMap.[p.IdHand]
                                                else
                                                    -1
                               lastFrame.PointableList.Add(fakeId, p.Rename(fakeId, fakeIdHand))
                               let evttype = if p.IsFinger then LeapFeatureTypes.ActiveFinger else LeapFeatureTypes.ActiveTool
                               let e = new LeapEventArgs(lastFrame, fakeId)
                               sensorEvent.Trigger(new SensorEventArgs<_,_>(evttype, e))

            this.Check()

            for p in activePointables do
                if not (newFrame.PointableList.ContainsKey(fakeToLeapMap.[p.Key])) then
                    // NON aggiorno il timestamp ovviamente. continuo a mandare il frame con le informazioni precedenti
                    let e = new LeapEventArgs(lastFrame, p.Key)
                    let evttype = if lastFrame.PointableList.[p.Key].IsFinger then LeapFeatureTypes.MoveFinger else LeapFeatureTypes.MoveTool
                    sensorEvent.Trigger(new SensorEventArgs<_,_>(evttype, e))
            this.Check()

        member this.Check() =
            for k in lastFrame.PointableList.Keys do
                assert (activePointables.ContainsKey(k))

            for k in lastFrame.HandList.Keys do
                assert (activeHands.ContainsKey(k))

            for k in activePointables.Keys do
                assert (lastFrame.PointableList.ContainsKey(k))

            for k in activeHands.Keys do
                assert (lastFrame.HandList.ContainsKey(k))

            for k in leapToFakeMap do
                assert (fakeToLeapMap.[k.Value] = k.Key)

            for k in fakeToLeapMap do
                assert (leapToFakeMap.[k.Value] = k.Key)
                assert (k.Key > counterBase)
                assert (activeHands.ContainsKey(k.Key) || activePointables.ContainsKey(k.Key))

            for k in activeHands do
                assert fakeToLeapMap.ContainsKey(k.Key)
                assert (not (activePointables.ContainsKey(k.Key)))

            for k in activePointables do
                assert fakeToLeapMap.ContainsKey(k.Key)
                assert (not (activeHands.ContainsKey(k.Key)))
                
        override this.OnDisconnect(c:Controller) =
            System.Console.WriteLine "OnDisconnect"
        override this.OnExit (c:Controller) =
            System.Console.WriteLine "OnExit"
