// Ulteriori informazioni su F# all'indirizzo http://fsharp.net
// Per ulteriori informazioni, vedere il progetto 'Esercitazione su F#'.
module Program
    open System.Windows.Forms
    open LeapDriver
    open ClonableLeapFrame
    open GestIT

    let mutable lastUp:TimeStamp = -1L

    let movefingerup (n:int) (x:LeapEventArgs) =
        let f = x.Frame
        let id = x.Id
        if (n = 5 && f.Timestamp - 25000L <= lastUp) || f.PointableList.Count > n || f.PointableList.Count = 0 then
            false
        else
            let finger =
                f.PointableList.Values
                |> Seq.maxBy (fun y -> y.Length)
            finger.Position.y >= 200.f

    let s = new LeapDriver.LeapSensor()

    let movefinger_goup = new GroundTerm<_,_>(LeapFeatureTypes.MoveFinger, movefingerup 2)
    let movefinger_stay = new GroundTerm<_,_>(LeapFeatureTypes.MoveFinger, movefingerup 5)
    
    let i1 = new Iter<_,_>(movefinger_goup)
    let c1 = new Choice<_,_>(i1, movefinger_stay)
    i1.Gesture.Add(fun (sender,e) -> SendKeys.SendWait("{UP}"))
    let netgioco1 = c1.ToGestureNet(s)

    let i2 = new Iter<_,_>(movefinger_stay)
    let c2 = new Choice<_,_>(i2, movefinger_goup)
    i2.Gesture.Add(fun (sender,e) -> lastUp <- e.Event.Frame.Timestamp
                                     SendKeys.SendWait("{UP}"))
    let netgioco2 = c2.ToGestureNet(s)

    System.Console.ReadLine() |> ignore