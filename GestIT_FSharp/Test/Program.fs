open GestIT
open GestIT.FSharp
open LeapDriver
open System.Windows.Forms
open System.Drawing
//// chiudi menu
//let o1 = new GroundTerm<LeapFeatureTypes,LeapEventArgs>(LeapFeatureTypes.ActiveFinger, fun _ -> true)
//let o2 = new GroundTerm<LeapFeatureTypes,LeapEventArgs>(LeapFeatureTypes.ActiveHand, fun _ -> true)
//let o3 = new GroundTerm<LeapFeatureTypes,LeapEventArgs>(LeapFeatureTypes.ActiveTool, fun _ -> true)
//let o4 = new GroundTerm<LeapFeatureTypes,LeapEventArgs>(LeapFeatureTypes.MoveFinger, fun _ -> true)
//let o5 = new GroundTerm<LeapFeatureTypes,LeapEventArgs>(LeapFeatureTypes.MoveHand, fun _ -> true)
//let n = ((o1 |=| o2) |^| (!* o3) |^| (o4 |=| o5)) |>> o1 |>> o2
////n.Children |> Seq.iter (fun v -> System.Diagnostics.Debug.WriteLine(sprintf "%A" v))
let s = new MouseSensor()
let p = new Pen(Color.Red)
let mutable r = new Rectangle(0, 0, 250, 250)
let form = new Form()
form.Width <- 500
form.Height <- 500
form.BackColor <- Color.Beige
form.Controls.Add(s)
form.Paint.Add(fun g -> g.Graphics.DrawRectangle(p, r))

let md = new GroundTerm<_,MouseEventArgs>(MouseFeatureTypes.MouseDown, fun _ -> true) |-> fun _ -> printfn "down"
let mm = new GroundTerm<_,MouseEventArgs>(MouseFeatureTypes.MouseMove, fun _ -> true) |-> fun _ -> printfn "move"
let mu = new GroundTerm<_,MouseEventArgs>(MouseFeatureTypes.MouseUp, fun _ -> true) |-> fun _ -> printfn "up"

let net1 = md |>> !* mm |>| mu
net1.ToGestureNet(s) |> ignore

let test (e:MouseEventArgs) =
    if r.Contains(e.Location) then true else false
let moverectangle (sender, e:SensorEventArgs<MouseFeatureTypes, MouseEventArgs>) =
    r.Location <- new Point(e.Event.X, e.Event.Y)
    form.Invalidate()
    
let md1 = new GroundTerm<_,MouseEventArgs>(MouseFeatureTypes.MouseDown, test) |-> fun _ -> printfn "clicked on rect"
let net2 = md1 |>> !* (mm |-> moverectangle) |>| mu
net2.ToGestureNet(s) |> ignore

form.Show()
Application.Run()