module FakeDriver

    open System.Windows.Forms
    open System.Drawing

    type SensorEventArgs<'T,'U> when 'T :> System.Enum and 'U :> System.EventArgs (t:'T, e:'U) =
        inherit System.EventArgs()
        member x.FeatureType = t
        member x.Event = e

    // Sensor's interface //
    type ISensor<'T,'U> when 'T :> System.Enum and 'U :> System.EventArgs =
          abstract member SensorEvents: IEvent<SensorEventArgs<'T,'U>>

    // Kind of feature that have to be notified from the sensor //
    type KeyFeatureTypes =
        | KeyDown = 0
        | KeyUp = 1
    
    type KeySensor () =
        inherit UserControl()
        let sensorEvent = new Event<SensorEventArgs<KeyFeatureTypes,KeyEventArgs>>()
        interface ISensor<KeyFeatureTypes,KeyEventArgs> with
            member x.SensorEvents = sensorEvent.Publish

        override x.OnKeyDown(e) =
            printfn "SPACE DOWN"
            sensorEvent.Trigger(new SensorEventArgs<_,_>(KeyFeatureTypes.KeyDown, e))

        override x.OnKeyUp(e) =
            printfn "SPACE UP"
            sensorEvent.Trigger(new SensorEventArgs<_,_>(KeyFeatureTypes.KeyUp, e))

    type MouseFeatureTypes =
        | MouseDown = 0
        | MouseUp = 1
        | MouseMove = 2

    type MouseSensor () =
        inherit UserControl()
        let sensorEvent = new Event<SensorEventArgs<MouseFeatureTypes,MouseEventArgs>>()
        interface ISensor<MouseFeatureTypes,MouseEventArgs> with
            member x.SensorEvents = sensorEvent.Publish

        override x.OnMouseDown(e) =
            printfn "MOUSE DOWN"
            sensorEvent.Trigger(new SensorEventArgs<_,_>(MouseFeatureTypes.MouseDown, e))

        override x.OnMouseUp(e) =
            printfn "MOUSE UP"
            sensorEvent.Trigger(new SensorEventArgs<_,_>(MouseFeatureTypes.MouseUp, e))

        override x.OnMouseMove(e) =
            printfn "MOUSE MOVE"
            sensorEvent.Trigger(new SensorEventArgs<_,_>(MouseFeatureTypes.MouseMove, e))