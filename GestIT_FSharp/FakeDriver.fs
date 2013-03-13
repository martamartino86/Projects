module FakeDriver

    open System.Windows.Forms
    open System.Drawing

    type IFeature =
        interface
        end

    type eventCallback = delegate of System.EventArgs -> unit

    type ISensor = 
          abstract member Subscribe: IFeature * eventCallback -> unit

    type FakeFeatureTypes =
        | KeyDown = 0
        | KeyUp = 1

    type FakeFeatures (t:FakeFeatureTypes) =
        member x.FeatureType with get() = t
        interface IFeature

    type ControlSensor () =
        inherit UserControl()
        let mutable upCallbacks : eventCallback list = []
        let mutable downCallbacks : eventCallback list = []

        interface ISensor with
            member x.Subscribe(f:IFeature, cb: eventCallback) =
                match f with
                | :? FakeFeatures as f ->
                    match f.FeatureType with
                    | FakeFeatureTypes.KeyDown -> downCallbacks <- cb::downCallbacks
                    | FakeFeatureTypes.KeyUp -> upCallbacks <- cb::upCallbacks
                    | _ -> failwith "F# sucks (hard)"
                | _ -> failwith "illegal feature"

        override x.OnKeyDown(e) =
            printfn "SPACE DOWN"
            for cb in downCallbacks do
                cb.Invoke e

        override x.OnKeyUp(e) =
            printfn "SPACE UP"
            for cb in upCallbacks do
                cb.Invoke e