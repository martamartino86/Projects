namespace GestIT

open System.Runtime.InteropServices

type SensorEventArgs<'T,'U> when 'T :> System.Enum and 'U :> System.EventArgs (t:'T, e:'U) =
  inherit System.EventArgs()
  member x.FeatureType = t
  member x.Event = e

type ISensor<'T,'U> when 'T :> System.Enum and 'U :> System.EventArgs =
  [<CLIEvent>]
  abstract member SensorEvents: IEvent<SensorEventArgs<'T,'U>>


type Token () =
  class
  end

[<AbstractClass>]
type GestureNet () =
  let completionEvent = new Event<Token seq>()
  member this.Completed(t) = completionEvent.Trigger(t)
  member this.Completion = completionEvent.Publish
  abstract member Front: GestureNet list
  abstract member AddToken: Token seq -> unit
  abstract member RemoveToken: Token seq -> unit


[<AbstractClass>]
type GestureExpr<'T,'U> when 'T :> System.Enum and 'U :> System.EventArgs () =
  let gestureEvent = new Event<_>()
  member this.Gestured(e) = gestureEvent.Trigger(this, e)
  [<CLIEvent>]
  member this.Gesture = gestureEvent.Publish
  abstract member ToNet: ISensor<'T,'U> -> GestureNet
  member this.ToInternalGestureNet(s) =
    let net = this.ToNet(s)
    net.Completion.Add(fun t -> this.Gestured())
    net
  member this.ToGestureNet(s) =
    let net = this.ToInternalGestureNet(s)
    for subn in net.Front do
      subn.Completion.Add(fun _ -> net.AddToken([new Token()]))
    net.AddToken([new Token()])
    net

type Predicate<'U> = delegate of 'U -> bool

type GroundTerm<'T,'U> when 'T :> System.Enum and 'U :> System.EventArgs (f:'T, ?p:Predicate<'U>) =
  inherit GestureExpr<'T,'U>()
  member this.Feature = f
  member this.Predicate = p
  override this.ToNet(s) = new GroundTermNet<_,_>(this, s) :> GestureNet

and private GroundTermNet<'T,'U> when 'T :> System.Enum and 'U :> System.EventArgs (exp:GroundTerm<'T,'U>, sensor:ISensor<'T,'U>) as this =
  inherit GestureNet()

  let mutable tokens = new System.Collections.Generic.HashSet<Token>()
  let mutable handler:System.IDisposable = null

  let clearHandler() =
    if handler <> null then
      handler.Dispose()
      handler <- null

  let handle (event:SensorEventArgs<'T,'U>) =
    if (exp.Feature :> System.Enum).Equals(event.FeatureType) then
      let p =
        match exp.Predicate with
          | None -> true
          | Some d -> d.Invoke(event.Event)
      if p then
        let oldtokens = tokens
        tokens <- new System.Collections.Generic.HashSet<Token>()
        clearHandler()
        this.Completed(oldtokens)

  override this.Front = [this]

  override this.AddToken(ts) =
    for t in ts do
      tokens.Add(t) |> ignore
    if handler = null then
      handler <- sensor.SensorEvents.Subscribe(handle)

  override this.RemoveToken(ts) =
    for t in ts do
      tokens.Remove(t) |> ignore
    if tokens.Count = 0 then
      clearHandler()

[<AbstractClass>]
type private BinaryOperatorNet (l:GestureNet, r:GestureNet) =
  inherit GestureNet()

  override this.AddToken(ts) =
    for n in this.Front do
      n.AddToken(ts)

  override this.RemoveToken(ts) =
    l.RemoveToken(ts)
    r.RemoveToken(ts)

type Sequence<'T,'U> when 'T :> System.Enum and 'U :> System.EventArgs (l:GestureExpr<'T,'U>, r:GestureExpr<'T,'U>) =
  inherit GestureExpr<'T,'U>()
  override this.ToNet(s) =
    let lnet = l.ToInternalGestureNet(s)
    let rnet = r.ToInternalGestureNet(s)
    let net = { new BinaryOperatorNet(lnet, rnet) with
                override this.Front = lnet.Front
                } :> GestureNet
    lnet.Completion.Add(rnet.AddToken)
    rnet.Completion.Add(net.Completed)
    net


type Parallel<'T,'U> when 'T :> System.Enum and 'U :> System.EventArgs (l:GestureExpr<'T,'U>, r:GestureExpr<'T,'U>) =
  inherit GestureExpr<'T,'U>()
  override this.ToNet(s) =
    let lnet = l.ToInternalGestureNet(s)
    let rnet = r.ToInternalGestureNet(s)
    let net = { new BinaryOperatorNet(lnet, rnet) with
                override this.Front = lnet.Front @ rnet.Front
              } :> GestureNet
    let semicompleted = new System.Collections.Generic.HashSet<Token>()
    let mycb ts =
      let mutable comp = []
      for t in ts do
        if semicompleted.Contains(t) then
          semicompleted.Remove(t) |> ignore
          comp <- t::comp
        else
          semicompleted.Add(t) |> ignore
      if comp <> [] then
        net.Completed(comp)
    lnet.Completion.Add(mycb)
    rnet.Completion.Add(mycb)
    net

type Choice<'T,'U> when 'T :> System.Enum and 'U :> System.EventArgs (l:GestureExpr<'T,'U>, r:GestureExpr<'T,'U>) =
  inherit GestureExpr<'T,'U>()
  override this.ToNet(s) =
    let lnet = l.ToInternalGestureNet(s)
    let rnet = r.ToInternalGestureNet(s)
    let net = { new BinaryOperatorNet(lnet, rnet) with
                override this.Front = lnet.Front @ rnet.Front
                } :> GestureNet
    lnet.Completion.Add(fun t ->
                        rnet.RemoveToken(t)
                        net.Completed(t))
    rnet.Completion.Add(fun t ->
                        lnet.RemoveToken(t)
                        net.Completed(t))
    net


(*
type OrderIndependence<'T> when 'T :> System.Enum (t:GestureExpr<'T> array) =
  inherit GestureExpr<'T>()
  override this.ToNet(s) =
    let nets = t |> Array.map (fun e -> e.To
    let lnet = l.ToInternalGestureNet(s)
    let rnet = r.ToInternalGestureNet(s)
    let net = { new BinaryOperatorNet(lnet, rnet) with
                override this.AddToken(t) =
                  lnet.AddToken(t)
                  rnet.AddToken(t)
                } :> GestureNet
    lnet.Completion.Add(fun t ->
                        rnet.RemoveToken(t)
                        net.Completed(t))
    rnet.Completion.Add(fun t ->
                        lnet.RemoveToken(t)
                        net.Completed(t))
    net
*)
