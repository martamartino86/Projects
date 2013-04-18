// Ulteriori informazioni su F# all'indirizzo http://fsharp.net
// Per ulteriori informazioni, vedere il progetto 'Esercitazione su F#'.
module Program
    open System.Windows.Forms
    open Leap
    open LeapDriver
    open GestIT

    let movefingerupgioco (x:LeapEventArgs) =
        let f = x.Frame
        let id = x.Id
        if f.PointableList.Count > 2 || f.PointableList.Count = 0 then
            //printfn "ptb %d - %A" (f.PointableList.Count) (f.Timestamp - lastFingerUp < 1000000L)
            false
        else
            let finger =
                f.PointableList.Values
                |> Seq.maxBy (fun y -> y.Length)
            finger.Position.y >= 250.f

    let s = new LeapDriver.LeapSensor()
    let movefingergioco = new GroundTerm<_,LeapEventArgs>(LeapFeatureTypes.MoveFinger, movefingerupgioco)
    let itergioco = new Iter<_,_>(movefingergioco)
    itergioco.Gesture.Add(fun (sender,e) -> SendKeys.SendWait("{UP}"))
    let netgioco = itergioco.ToGestureNet(s)

    System.Console.ReadLine() |> ignore