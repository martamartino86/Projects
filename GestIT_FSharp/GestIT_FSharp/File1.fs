module File1
//
//    open GestIT
//    open LeapDriver
//    open ClonableLeapFrame
//    open System.IO
//
//    let p (x:LeapEventArgs) = true
//    let s = new LeapSensor()
//    let g1 = new GroundTerm<LeapFeatureTypes,LeapEventArgs>(LeapFeatureTypes.ActiveFinger, p)
//    
//    let net = g1.ToGestureNet(s)
//    net.Stream <- File.OpenWrite(@"C:\Users\Pc\Desktop\output.ser")
//    
//    
//    
//    
//    (s :> ISensor<_,_>).SensorEvents.Add(fun _ ->
//            printfn "ricevuti dati"
//    )
