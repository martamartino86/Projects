open GestIT
open GestIT.FSharp

open LeapDriver

// chiudi menu
let o1 = new GroundTerm<LeapFeatureTypes,LeapEventArgs>(LeapFeatureTypes.ActiveFinger, fun _ -> true)
let o2 = new GroundTerm<LeapFeatureTypes,LeapEventArgs>(LeapFeatureTypes.ActiveHand, fun _ -> true)
let o3 = new GroundTerm<LeapFeatureTypes,LeapEventArgs>(LeapFeatureTypes.ActiveTool, fun _ -> true)
let o4 = new GroundTerm<LeapFeatureTypes,LeapEventArgs>(LeapFeatureTypes.MoveFinger, fun _ -> true)
let o5 = new GroundTerm<LeapFeatureTypes,LeapEventArgs>(LeapFeatureTypes.MoveHand, fun _ -> true)

let n = ((o1 |=| o2) |^| (!* o3) |^| (o4 |=| o5)) |>> o1 |>> o2


//n.Children |> Seq.iter (fun v -> System.Diagnostics.Debug.WriteLine(sprintf "%A" v))

