namespace GestIT

open System.IO
open System.Runtime.Serialization.Formatters.Binary
open System.Threading

type PlaybackSensor<'T, 'U> when 'T :> System.Enum and 'U :> System.EventArgs (s:Stream) =
  let evt = new Event<SensorEventArgs<'T,'U>>()
  let ser = new BinaryFormatter()

  let readObj () = ser.Deserialize(s) :?> (System.DateTime*'T*'U)
  // FIXME: Potrebbe non andare sugli stream di rete...
  let isEof () = s.Length = s.Position

  member this.start () =
    let worker = new Thread(fun () -> 
      let (begint, typ, ev) = readObj()
      let mutable lastTime = begint
      evt.Trigger(new SensorEventArgs<'T, 'U>(typ, ev))

      while not(isEof()) do
        let (t, typ, ev) = readObj()
        let dt = (t - lastTime).TotalMilliseconds
        lastTime <- t
        if  dt > 5. then Thread.Sleep(int dt)
        evt.Trigger(new SensorEventArgs<'T, 'U>(typ, ev))
    )
    worker.IsBackground <- true
    worker.Start()

  interface ISensor<'T,'U> with
    [<CLIEvent>]
    member x.SensorEvents = evt.Publish


