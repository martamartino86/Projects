namespace FakeDriver

    open System.Windows.Forms
    open System.Drawing
    open System.Collections.Generic
    open Microsoft.FSharp.Reflection
    open MyLeapFrame
    open GestIT
    open Leap
    open System.IO

    // KEYBOARD features that have to be notified from the sensor //
    type KeyFeatureTypes =
        | KeyDown = 0
        | KeyUp = 1
    
    type KeySensor () =
        inherit UserControl()
        let sensorEvent = new Event<SensorEventArgs<KeyFeatureTypes,KeyEventArgs>>()
        interface ISensor<KeyFeatureTypes,KeyEventArgs> with
            [<CLIEvent>]
            member x.SensorEvents = sensorEvent.Publish

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
            [<CLIEvent>]
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
 
    
    //let outfile = File.CreateText("dataleap.txt")

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

    type IStateCleaner<'T,'U> =
        (* Clears the history preceding a timestamp, returns the elements that are being dropped *)
        abstract member TruncateHistory: TimeStamp -> 'T list

        (* Predicts the model state at the given timestamp, as an evolution of the currently known information *)
        abstract member Predict: TimeStamp -> unit

        (*
         * Updates the current state using the new information (assuming matching ids);
         * returns the model representation of the new information if the update succeeded
         *)
        abstract member TryUpdate: 'U -> 'T option

        (*
         * Tries to corrects the current state using the new information;
         * returns the model representation of the new information if the correction succeeded
         *)
        abstract member Correct: 'U -> 'T option

        (*
         * Extends the current state using the new information;
         * returns the model representation of the new information
         *)
        abstract member Extend: 'U -> 'T

    type LeapId = int

    type MyHandCleaner(s:MyFrame) =

        let handDistance (h1:MyHand) (h2:Hand) =
//        let delta_s = System.Math.Abs (h1.Position - h2.Position).Magnitude
//        let delta_t = (float32)(currenttimestamp - activeHands.[leapToFakeMap.[h1.Id]]) / 1000.F
            (h1.Position - h2.PalmPosition).Magnitude

        let checkDistanceHands (h1:MyHand) (h2:Hand) =
    //        let delta_s = System.Math.Abs (h1.Position - h2.Position).Magnitude
    //        let delta_t = (float32)(currenttimestamp - activeHands.[leapToFakeMap.[h1.Id]]) / 1000.F
            (handDistance h1 h2) < 100.F

        let isTheSameHand (h1:MyHand) (h2:Hand) =
            checkDistanceHands h1 h2

        let state = s
        let handTimestamps = new Dictionary<LeapId, TimeStamp>()
        let leapToFake = new Dictionary<LeapId, FakeId>()
        let mutable lastTimestamp:TimeStamp = -1L
        
        member x.GetFakeId(leapId:LeapId) =
            if leapId = Leap.Hand.Invalid.Id then
                null
            else
                leapToFake.[leapId]

        interface IStateCleaner<MyHand,Hand> with
            member x.TruncateHistory(t) =
                let removedIds =
                    handTimestamps
                    |> Seq.filter (fun x -> x.Value < t)
                    |> Seq.map (fun x -> x.Key)
                    |> Seq.toArray
                let removedHands =
                    removedIds
                    |> Seq.map (fun x -> state.HandList.[leapToFake.[x]])
                    |> Seq.toList
                for leapId in removedIds do
                    let fakeId = leapToFake.[leapId]
                    for p in state.PointableList.Values do
                        if p.IdHand = fakeId then
                            p.IdHand <- null
                    handTimestamps.Remove(leapId) |> ignore
                    state.HandList.Remove(fakeId) |> ignore
                    leapToFake.Remove(leapId) |> ignore
                removedHands

            member x.Predict(t) =
                for h in state.HandList do
                    let hand = h.Value
                    hand.Position <- hand.Position (* mm *) + hand.Velocity (* mm/s *) * ((float32)(t (* us *) - lastTimestamp) * (1.e-6f))
                lastTimestamp <- t

            member x.TryUpdate(h) =
                let leapId = h.Id
                if leapToFake.ContainsKey(leapId) then
                    let fakeId = leapToFake.[leapId]
                    let hand = new MyHand(fakeId, h.Direction, h.PalmPosition, h.PalmVelocity, h.PalmNormal, h.SphereCenter, h.SphereRadius)
                    state.HandList.[fakeId] <- hand
                    handTimestamps.[leapId] <- h.Frame.Timestamp
                    Some hand
                else
                    None

            member x.Correct(h) =
                (* controllo se questa mano e' gia' contenuta nello stato *)
                let leapId = h.Id
                try
                    let oldLeapId,hand =
                        handTimestamps
                        |> Seq.filter ( fun x -> x.Value < h.Frame.Timestamp )
                        |> Seq.map ( fun x -> x.Key,state.HandList.[leapToFake.[x.Key]] )
                        |> Seq.filter ( fun (oldLeapId,x) -> isTheSameHand x h )
                        |> Seq.sortBy ( fun (oldLeapId,x) -> handDistance x h )
                        |> Seq.head
                    let fakeId = hand.Id
                    let hand = new MyHand(fakeId, h.Direction, h.PalmPosition, h.PalmVelocity, h.PalmNormal, h.SphereCenter, h.SphereRadius)
                    handTimestamps.Remove(oldLeapId) |> ignore
                    leapToFake.Remove(oldLeapId) |> ignore
                    state.HandList.[fakeId] <- hand
                    handTimestamps.Add(leapId, h.Frame.Timestamp)
                    leapToFake.Add(leapId, fakeId)
                    Some hand
                with
                | _ -> 
                    None

            member x.Extend(h) = 
                let leapId = h.Id
                let fakeId = new FakeId()
                let hand = new MyHand(fakeId, h.Direction, h.PalmPosition, h.PalmVelocity, h.PalmNormal, h.SphereCenter, h.SphereRadius)
                state.HandList.Add(fakeId, hand)
                handTimestamps.Add(leapId, h.Frame.Timestamp)
                leapToFake.Add(leapId, fakeId)
                hand

    type MyPointableCleaner(s:MyFrame,hc:MyHandCleaner) =
        let epsilon = 1000.0f* 1.5F

        let checkDistanceFingers (p1:MyPointable) (p2:Pointable) =
            printfn "distanza: %s " (((p1.Position - p2.TipPosition).Magnitude).ToString())
            //printfn "%A %A" p1.Position (p2.Position)
            (p1.Position - p2.TipPosition).Magnitude < 50.F
    
        let isTheSameFinger (f1:MyPointable) (f2:Pointable) =
            //printfn "diff length: %f " (System.Math.Abs(f1.Length - f2.Length))
            //printfn "diff width: %f " (System.Math.Abs(f1.Width - f2.Width))
            (System.Math.Abs(f1.Length - f2.Length) < epsilon) && (System.Math.Abs(f1.Width - f2.Width) < epsilon) && (checkDistanceFingers f1 f2)


        let state = s
        let handCleaner = hc
        let pointableTimestamps = new Dictionary<LeapId, TimeStamp>()
        let leapToFake = new Dictionary<LeapId, FakeId>()
        let mutable lastTimestamp:TimeStamp = -1L

        interface IStateCleaner<MyPointable,Pointable> with
            member x.TruncateHistory(t) =
                let removedIds =
                    pointableTimestamps
                    |> Seq.filter (fun x -> x.Value < t)
                    |> Seq.map (fun x -> x.Key)
                    |> Seq.toArray
                let removedPointables =
                    removedIds
                    |> Seq.map (fun x -> state.PointableList.[leapToFake.[x]])
                    |> Seq.toList
                for leapId in removedIds do
                    pointableTimestamps.Remove(leapId) |> ignore
                    state.PointableList.Remove(leapToFake.[leapId]) |> ignore
                    leapToFake.Remove(leapId) |> ignore
                    printfn "RIMOSSO: %A" leapId
                removedPointables
            
            member x.TryUpdate(p) =
                let leapId = p.Id
                if leapToFake.ContainsKey(leapId) then
                    let fakeId = leapToFake.[leapId]
                    let pointable = new MyPointable(fakeId, handCleaner.GetFakeId(p.Hand.Id), p.Direction, p.TipPosition, p.TipVelocity, p.IsFinger, p.IsTool, p.Length, p.Width)
                    state.PointableList.[fakeId] <- pointable
                    pointableTimestamps.[leapId] <- p.Frame.Timestamp
                    Some pointable
                else
                    None

            member x.Predict(t) =
                for p in state.PointableList do
                    let ptb = p.Value
                    ptb.Position <- ptb.Position + ptb.Velocity * ((float32)(t - lastTimestamp) * (1.e-6f))
                lastTimestamp <- t

            member x.Correct(p) =
                (* controllo se questo pointable e' gia' contenuto nello stato *)
                let leapId = p.Id
                try
                    let oldLeapId,pointable =
                        pointableTimestamps
                        |> Seq.filter ( fun x -> x.Value < p.Frame.Timestamp )
                        |> Seq.map ( fun x -> x.Key,state.PointableList.[leapToFake.[x.Key]] )
                        |> Seq.filter ( fun (oldLeapId,x) -> isTheSameFinger x p )
                        |> Seq.sortBy ( fun (oldLeapId,x) -> checkDistanceFingers x p )
                        |> Seq.head
                    let fakeId = pointable.Id
                    let pointable = new MyPointable(fakeId, handCleaner.GetFakeId(p.Hand.Id), p.Direction, p.TipPosition, p.TipVelocity, p.IsFinger, p.IsTool, p.Length, p.Width)
                    pointableTimestamps.Remove(oldLeapId) |> ignore
                    leapToFake.Remove(oldLeapId) |> ignore
                    state.PointableList.[fakeId] <- pointable
                    pointableTimestamps.Add(leapId, p.Frame.Timestamp)
                    leapToFake.Add(leapId, fakeId)
                    printfn "NUOVO ID %A, TROVATO ZOMBIE: %A" leapId oldLeapId
                    Some pointable
                with
                | _ -> 
                    None

            member x.Extend(p) =
                let leapId = p.Id
                let fakeId = new FakeId()
                let pointable = new MyPointable(fakeId, handCleaner.GetFakeId(p.Hand.Id), p.Direction, p.TipPosition, p.TipVelocity, p.IsFinger, p.IsTool, p.Length, p.Width)
                state.PointableList.Add(fakeId, pointable)
                pointableTimestamps.Add(leapId, p.Frame.Timestamp)
                leapToFake.Add(leapId, fakeId)
                printfn "NUOVO ID %A, L'HO AGGIUNTO" leapId 
                pointable

    // Evento contenente il frame corrente e l'ID dell'oggetto a cui si riferisce la feature.
    type LeapEventArgs(f:MyFrame, id:FakeId) =
        inherit System.EventArgs()
        // Oggetto Frame corrente.
        member this.Frame = f
        // ID dell'oggetto a cui si riferisce la feature (es. ID di un Hand / Finger / Tool).
        member this.Id = id

    type LeapSensor () as this =
        inherit Leap.Listener()
        let ctrl = new Controller()

        let zombieWindow = 200000L
        let state = new MyFrame()
        let handCleaner = new MyHandCleaner(state)
        let pointableCleaner = new MyPointableCleaner(state, handCleaner) :> IStateCleaner<_,_>
        let handCleaner = handCleaner :> IStateCleaner<_,_>

        let sensorEvent = new Event<SensorEventArgs<LeapFeatureTypes, LeapEventArgs>>()

        do
            ctrl.AddListener(this) |> ignore

        member this.Controller = ctrl
        interface ISensor<LeapFeatureTypes,LeapEventArgs> with
            [<CLIEvent>]
            member x.SensorEvents = sensorEvent.Publish
        override this.OnInit(c:Controller) =
            System.Console.WriteLine "OnInit"
        override this.OnConnect(c:Controller) =
            System.Console.WriteLine "OnConnect"
        override this.OnFrame(c:Controller) =
            let frame = c.Frame()
            let currenttimestamp = frame.Timestamp

            state.Timestamp <- currenttimestamp

            let removedPointables = pointableCleaner.TruncateHistory(currenttimestamp - zombieWindow)
            for pointable in removedPointables do
                let e = new LeapEventArgs(state, pointable.Id)
                let t = if pointable.IsFinger then LeapFeatureTypes.NotActiveFinger else LeapFeatureTypes.NotActiveTool
                sensorEvent.Trigger(new SensorEventArgs<_,_>(t, e))

            let removedHands = handCleaner.TruncateHistory(currenttimestamp - zombieWindow)
            for hand in removedHands do
                let e = new LeapEventArgs(state, hand.Id)
                sensorEvent.Trigger(new SensorEventArgs<_,_>(LeapFeatureTypes.NotActiveHand, e))

            handCleaner.Predict(currenttimestamp)
            pointableCleaner.Predict(currenttimestamp)

            let mutable unmatched = []
            for h in frame.Hands do
                match handCleaner.TryUpdate(h) with
                | Some hand ->
                    let e = new LeapEventArgs(state,hand.Id)
                    sensorEvent.Trigger(new SensorEventArgs<_,_>(LeapFeatureTypes.MoveHand, e))
                | None -> unmatched <- h::unmatched

            for h in unmatched do
                let hand,t =
                    match handCleaner.Correct(h) with
                    | Some hand -> hand,LeapFeatureTypes.MoveHand
                    | None -> handCleaner.Extend(h),LeapFeatureTypes.ActiveHand
                let e = new LeapEventArgs(state,hand.Id)
                sensorEvent.Trigger(new SensorEventArgs<_,_>(t, e))

            let mutable unmatched = []
            for p in frame.Pointables do
                match pointableCleaner.TryUpdate(p) with
                | Some pointable ->
                    let t = if pointable.IsFinger then LeapFeatureTypes.MoveFinger else LeapFeatureTypes.MoveTool
                    let e = new LeapEventArgs(state,pointable.Id)
                    sensorEvent.Trigger(new SensorEventArgs<_,_>(t, e))
                | None -> unmatched <- p::unmatched

            for p in unmatched do
                let pointable,t =
                    match pointableCleaner.Correct(p) with
                    | Some pointable ->
                        let t = if pointable.IsFinger then LeapFeatureTypes.MoveFinger else LeapFeatureTypes.MoveTool
                        pointable,t
                    | None ->
                        let pointable = pointableCleaner.Extend(p)
                        let t = if pointable.IsFinger then LeapFeatureTypes.ActiveFinger else LeapFeatureTypes.ActiveTool
                        pointable,t
                let e = new LeapEventArgs(state,pointable.Id)
                sensorEvent.Trigger(new SensorEventArgs<_,_>(t, e))


//            outfile.Write("{0}, ", currenttimestamp)
//            outfile.Flush()

(*
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
                *)
        override this.OnDisconnect(c:Controller) =
            System.Console.WriteLine "OnDisconnect"
        override this.OnExit (c:Controller) =
            System.Console.WriteLine "OnExit"
