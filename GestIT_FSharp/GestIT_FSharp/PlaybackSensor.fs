namespace GestIT

open System.IO
open System.Runtime.Serialization.Formatters.Binary
open System.Threading

open System.Diagnostics


//type ProxySensor<'T, 'U> when 'T :> System.Enum and 'U :> System.EventArgs =
//  val inputSensor : 'Z
//
//  new () = { inputSensor = new 'Z(
//
//
type PlaybackSensor<'T, 'U> when 'T :> System.Enum and 'U :> System.EventArgs (s:Stream) =
//  let mutable eventsRead = 0
  let evt = new Event<SensorEventArgs<'T,'U>>()
  let ser = new BinaryFormatter()

  let readObj () =
    ser.Deserialize(s) :?> (System.DateTime*'T*'U)
//    try ser.Deserialize(s) :?> (System.DateTime*'T*'U)
//    finally eventsRead <- eventsRead + 1

  // FIXME: Potrebbe non andare sugli stream di rete...
  let isEof () = s.Length = s.Position

//  member this.EventsRead with get() = eventsRead

  member this.start () =
    let worker = new Thread(fun () -> 
      s.Position <- 0L
      let (begint, typ, ev) = readObj()
      let mutable lastTime = begint
//      Debug.WriteLine("{0} {1} {2}", begint, typ.ToString(), ev.ToString())
      evt.Trigger(new SensorEventArgs<'T, 'U>(typ, ev))

      while not(isEof()) do
        let (t, typ, ev) = readObj()
        let dt = (t - lastTime).TotalMilliseconds
        lastTime <- t
        if  dt > 5. then Thread.Sleep(int dt)
//        Debug.WriteLine("{0} {1} {2}", lastTime, typ.ToString(), ev.ToString())
        evt.Trigger(new SensorEventArgs<'T, 'U>(typ, ev))
//      Debug.WriteLine(sprintf "* FILE ENDED * : read %d events" this.EventsRead)
    )
    worker.IsBackground <- true
    worker.Start()

  interface ISensor<'T,'U> with
    [<CLIEvent>]
    member x.SensorEvents = evt.Publish


