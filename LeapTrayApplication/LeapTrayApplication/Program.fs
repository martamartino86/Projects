// Ulteriori informazioni su F# all'indirizzo http://fsharp.net
// Per ulteriori informazioni, vedere il progetto 'Esercitazione su F#'.
module LeapTrayApplication
    open System.Windows.Forms
    open System.Collections.Generic
    open GestIT
    open FakeDriver
    open MyLeapFrame

    (* Structures *)
    let s = new FakeDriver.LeapSensor()
    let frameQueue = new Queue<MyFrame>()
    let lastFrameInQueue = new MyFrame() // it represents the last enqueued frame


    (* Sensor *)
    (s :> ISensor<_,_>).SensorEvents.Add(fun e ->
        let f = e.Event.Frame
        let id = e.Event.Id
        if lastFrameInQueue.Timestamp = f.Timestamp then
            (* update frame informations *)
            TODOUpdateInformations(f)
        else
            (* surely lastFrame.TS < f.TS, so i'm gonna add it to the queue *)
            frameQueue.Enqueue(f)
    )


    [<EntryPoint>]
    let main argv = 
        printfn "%A" argv
        0 // restituisci un intero come codice di uscita
