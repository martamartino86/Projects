module DriverVariEventuali


(* PROVA TASTIERA
let s = new FakeDriver.KeySensor()

let cdown = new GestIT.GroundTerm<_>(FakeDriver.KeyFeatureTypes.KeyDown, p)
let cup = new GestIT.GroundTerm<_>(FakeDriver.KeyFeatureTypes.KeyUp, p)

let g = new Sequence<_>(cdown, cup)
let gg = new Sequence<_>(g, g)

cdown.Gesture.Add(fun _ -> printfn "Sono un tasto che va giu'")
cup.Gesture.Add(fun _ -> printfn "Sono un tasto che va su")
g.Gesture.Add(fun _ -> printfn "Sono un tasto che si pressa")
gg.Gesture.Add(fun _ -> printfn "Sono un tasto che si doppiopressa")

let s = new FakeDriver.KeySensor()
let net = gg.ToGestureNet(s :> ISensor<_,_>)
*)
/// ** /// ** ///

(* PROVA MOUSE
let mutable mouseclick:Point = Point.Empty
let s = new FakeDriver.MouseSensor(false)
(s :> ISensor<_,_>).SensorEvents.Add(fun e -> if e.FeatureType = MouseFeatureTypes.MouseDown then
                                                mouseclick <- new Point(e.Event.X, e.Event.Y))

let p = new Predicate<_>(fun _ -> true) 
let dragpredicate = new Predicate<MouseEventArgs>(fun emouse ->
    ((emouse.X - mouseclick.X > 10) || (emouse.Y - mouseclick.Y > 10))
)

let mousedown = new GroundTerm<_,_>(FakeDriver.MouseFeatureTypes.MouseDown, p)
let mousemove = new GroundTerm<_,_>(FakeDriver.MouseFeatureTypes.MouseMove, dragpredicate)
let mouseup = new GroundTerm<_,_>(FakeDriver.MouseFeatureTypes.MouseUp, p)

let seq1 = new Sequence<_,_>(mousedown, mousemove)
let seq2 = new Sequence<_,_>(seq1, mouseup)
mousedown.Gesture.Add(fun _ -> printfn "doooown" )
seq1.Gesture.Add(fun _ -> printfn "hai mosso un pochiiino in diagonale..." )
seq2.Gesture.Add(fun _ -> printfn "DRAG & DROOOP!" )

let net = seq2.ToGestureNet(s :> ISensor<_,_>)
*)