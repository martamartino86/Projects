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

    (* Information sent on event triggering *)
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
        (*
         * Clears the history preceding a timestamp, calling the given function for each of them as soon as it has been removed;
         * returns the elements that have been dropped
         *)
        abstract member TruncateHistory: ('T -> unit) -> TimeStamp -> unit

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
        let debugHand = false

        let handDistance (h1:MyHand) (h2:Hand) =
            (h1.Position - h2.PalmPosition).Magnitude

        let isTheSameHand (zombiets:TimeStamp) (zombiehand:MyHand) (newhand:Hand) =
            let speedError = (1.F (* max speed in m/s *) * 1.e-6F (* us -> s *) * 1.e3F (* m -> mm *)) (* mm / us *)
            (handDistance zombiehand newhand) < float32(newhand.Frame.Timestamp - zombiets) * speedError + 3.F

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
            member x.TruncateHistory f t =
                let removedIds =
                    handTimestamps
                    |> Seq.filter (fun x -> x.Value < t)
                    |> Seq.map (fun x -> x.Key)
                    |> Seq.toArray
                for leapId in removedIds do
                    let fakeId = leapToFake.[leapId]
                    let h = state.HandList.[leapToFake.[leapId]]
                    for p in state.PointableList.Values do
                        if p.IdHand = fakeId then
                            p.IdHand <- null
                    handTimestamps.Remove(leapId) |> ignore
                    state.HandList.Remove(fakeId) |> ignore
                    leapToFake.Remove(leapId) |> ignore
                    f h
                    if debugHand then printfn "RIMOSSO: %A" leapId

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
                let maybeZombie =
                    handTimestamps
                    |> Seq.filter ( fun x -> x.Value < h.Frame.Timestamp )
                    |> Seq.map ( fun x -> x.Key,state.HandList.[leapToFake.[x.Key]] )
                    |> Seq.sortBy ( fun (oldLeapId,x) -> handDistance x h )
                    |> Seq.tryFind ( fun (oldLeapId,x) -> isTheSameHand handTimestamps.[oldLeapId] x h )
                match maybeZombie with
                | None -> None
                | Some (oldLeapId, hand) ->
                    let fakeId = hand.Id
                    let hand = new MyHand(fakeId, h.Direction, h.PalmPosition, h.PalmVelocity, h.PalmNormal, h.SphereCenter, h.SphereRadius)
                    handTimestamps.Remove(oldLeapId) |> ignore
                    leapToFake.Remove(oldLeapId) |> ignore
                    state.HandList.[fakeId] <- hand
                    handTimestamps.Add(leapId, h.Frame.Timestamp)
                    leapToFake.Add(leapId, fakeId)
                    if debugHand then printfn "NUOVO ID %A, TROVATO ZOMBIE: %A" leapId oldLeapId
                    Some hand

            member x.Extend(h) = 
                let leapId = h.Id
                let fakeId = new FakeId()
                let hand = new MyHand(fakeId, h.Direction, h.PalmPosition, h.PalmVelocity, h.PalmNormal, h.SphereCenter, h.SphereRadius)
                state.HandList.Add(fakeId, hand)
                handTimestamps.Add(leapId, h.Frame.Timestamp)
                leapToFake.Add(leapId, fakeId)
                if debugHand then printfn "NUOVO ID %A, L'HO AGGIUNTO" leapId 
                hand

    type MyPointableCleaner(s:MyFrame,hc:MyHandCleaner) =
        let epsilon = 1000.0f * 1.5F
        let debugPtbl = false
        let fingerDistance (p1:MyPointable) (p2:Pointable) =
            (p1.Position - p2.TipPosition).Magnitude
    
        let isTheSameFinger (zombiets:TimeStamp) (zombiePointable:MyPointable) (newPointable:Pointable) =
            let speedError = (1.F (* max speed in m/s *) * 1.e-6F (* us -> s *) * 1.e3F (* m -> mm *)) (* mm / us *)
            let diffLength = System.Math.Abs(zombiePointable.Length - newPointable.Length) // finger length
            let diffWidth = System.Math.Abs(zombiePointable.Width - newPointable.Width) // finger width
            if debugPtbl then printfn "--->> %A %A %A" diffLength diffWidth ((fingerDistance zombiePointable newPointable) < (float32(newPointable.Frame.Timestamp - zombiets) * speedError + 3.F))
            (diffLength < epsilon) && (diffWidth < epsilon) &&
                ((fingerDistance zombiePointable newPointable) < (float32(newPointable.Frame.Timestamp - zombiets) * speedError + 3.F))

        let state = s
        let handCleaner = hc
        let pointableTimestamps = new Dictionary<LeapId, TimeStamp>()
        let leapToFake = new Dictionary<LeapId, FakeId>()
        let mutable lastTimestamp:TimeStamp = -1L

        interface IStateCleaner<MyPointable,Pointable> with
            member x.TruncateHistory f t =
                let removedIds =
                    pointableTimestamps
                    |> Seq.filter (fun x -> x.Value < t)
                    |> Seq.map (fun x -> x.Key)
                    |> Seq.toArray
                for leapId in removedIds do
                    let p = state.PointableList.[leapToFake.[leapId]]
                    pointableTimestamps.Remove(leapId) |> ignore
                    state.PointableList.Remove(leapToFake.[leapId]) |> ignore
                    leapToFake.Remove(leapId) |> ignore
                    f p
                    if debugPtbl then printfn "%A RIMUOVO: %A => %A" state.Timestamp leapId (leapToFake.Keys |> Seq.toArray)
            
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
                let maybeZombie =
                    pointableTimestamps
                    |> Seq.filter ( fun x -> x.Value < p.Frame.Timestamp )
                    |> Seq.map ( fun x -> x.Key,state.PointableList.[leapToFake.[x.Key]] )
                    |> Seq.sortBy ( fun (oldLeapId,x) -> fingerDistance x p )
                    |> Seq.tryFind ( fun (oldLeapId,x) -> isTheSameFinger pointableTimestamps.[oldLeapId] x p )
                match maybeZombie with
                | None -> None
                | Some (oldLeapId,pointable) ->
                    let fakeId = pointable.Id
                    let pointable = new MyPointable(fakeId, handCleaner.GetFakeId(p.Hand.Id), p.Direction, p.TipPosition, p.TipVelocity, p.IsFinger, p.IsTool, p.Length, p.Width)
                    pointableTimestamps.Remove(oldLeapId) |> ignore
                    leapToFake.Remove(oldLeapId) |> ignore
                    state.PointableList.[fakeId] <- pointable
                    pointableTimestamps.Add(leapId, p.Frame.Timestamp)
                    leapToFake.Add(leapId, fakeId)
                    if debugPtbl then printfn "%A RINOMINO %A -> %A => %A" state.Timestamp oldLeapId leapId (leapToFake.Keys |> Seq.toArray)
                    Some pointable

            member x.Extend(p) =
                let leapId = p.Id
                let fakeId = new FakeId()
                let pointable = new MyPointable(fakeId, handCleaner.GetFakeId(p.Hand.Id), p.Direction, p.TipPosition, p.TipVelocity, p.IsFinger, p.IsTool, p.Length, p.Width)
                state.PointableList.Add(fakeId, pointable)
                pointableTimestamps.Add(leapId, p.Frame.Timestamp)
                leapToFake.Add(leapId, fakeId)
                if debugPtbl then printfn "%A NUOVO ID: %A => %A" state.Timestamp leapId (leapToFake.Keys |> Seq.toArray)
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

            pointableCleaner.TruncateHistory (fun pointable ->
                                                let e = new LeapEventArgs(state, pointable.Id)
                                                let t = if pointable.IsFinger then LeapFeatureTypes.NotActiveFinger else LeapFeatureTypes.NotActiveTool
                                                sensorEvent.Trigger(new SensorEventArgs<_,_>(t, e))
                ) (currenttimestamp - zombieWindow)

            handCleaner.TruncateHistory (fun hand ->
                                                let e = new LeapEventArgs(state, hand.Id)
                                                sensorEvent.Trigger(new SensorEventArgs<_,_>(LeapFeatureTypes.NotActiveHand, e))
                ) (currenttimestamp - zombieWindow)

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

        override this.OnDisconnect(c:Controller) =
            System.Console.WriteLine "OnDisconnect"
        override this.OnExit (c:Controller) =
            System.Console.WriteLine "OnExit"
