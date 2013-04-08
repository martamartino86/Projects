module GestureDefinitions
    open GestIT
    open FakeDriver
    open LeapTrayApplication

    (* Predicates definitions *)
    let moveright = new Predicate<LeapEventArgs>(fun x ->
        let id = x.Id
        let exists =
            if frameQueue |> Seq.exists (fun f -> not (f.PointableList.ContainsKey(id))) then
                false
            else
                frameQueue |> Seq.pairwise |> Seq.forall (fun (f1,f2) ->
                    let p1 = f1.PointableList.[id].Position
                    let p2 = f2.PointableList.[id].Position
                    let delta_s = System.Math.Abs(p2.x - p1.x)
                    let delta_t = (float32)(f2.Timestamp - f1.Timestamp) * 1000.f
                    let v_m = (delta_s / delta_t) * 1000000.f
    //                if v_m >= 1.f then
    //                    printfn "vel media: %f" v_m
                    (p2.x >= p1.x) && (v_m >= 1.f)
            )
        exists && (System.Math.Abs(frameQueue.Peek().PointableList.[id].Position.x - lastFrameInQueue.PointableList.[id].Position.x) > 150.f)
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
