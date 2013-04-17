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
type GestureNet<'T,'U> when 'T :> System.Enum and 'U :> System.EventArgs () =
  let completionEvent = new Event<SensorEventArgs<'T,'U> * seq<Token>>()
  member this.Completed(e,t) = completionEvent.Trigger(e,t)
  member this.Completion = completionEvent.Publish
  abstract member Front: GestureNet<'T,'U> list
  abstract member AddTokens: Token seq -> unit
  abstract member RemoveTokens: Token seq -> unit

[<AbstractClass>]
type GestureExpr<'T,'U> when 'T :> System.Enum and 'U :> System.EventArgs () =
  let gestureEvent = new Event<_>()
  member this.Gestured(e) = gestureEvent.Trigger(this, e)
  [<CLIEvent>]
  member this.Gesture = gestureEvent.Publish
  abstract member ToNet: ISensor<'T,'U> -> GestureNet<'T,'U>
  member this.ToInternalGestureNet(s) =
    let net = this.ToNet(s)
    net.Completion.Add(fun (e,ts) -> this.Gestured(e))
    net
  member this.ToGestureNet(s) =
    let net = this.ToInternalGestureNet(s)
    for subn in net.Front do
      subn.Completion.Add(fun _ -> net.AddTokens([new Token()]))
    net.AddTokens([new Token()])
    net

type Predicate<'U> = delegate of 'U -> bool

type GroundTerm<'T,'U> when 'T :> System.Enum and 'U :> System.EventArgs (f:'T, ?p:Predicate<'U>) =
  inherit GestureExpr<'T,'U>()
  new(f,p) = GroundTerm<_,_>(f, new Predicate<_>(p))
  member this.Feature = f
  member this.Predicate = p
  override this.ToNet(s) = new GroundTermNet<_,_>(this, s) :> GestureNet<_,_>

and private GroundTermNet<'T,'U> when 'T :> System.Enum and 'U :> System.EventArgs (exp:GroundTerm<'T,'U>, sensor:ISensor<'T,'U>) as this =
  inherit GestureNet<'T,'U>()

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
        this.Completed(event,oldtokens)

  override this.Front = [this]

  override this.AddTokens(ts) =
    for t in ts do
      tokens.Add(t) |> ignore
    if handler = null then
      handler <- sensor.SensorEvents.Subscribe(handle)

  override this.RemoveTokens(ts) =
    for t in ts do
      tokens.Remove(t) |> ignore
    if tokens.Count = 0 then
      clearHandler()

[<AbstractClass>]
type private OperatorNet<'T,'U> when 'T :> System.Enum and 'U :> System.EventArgs ([<System.ParamArray>] subnets:GestureNet<'T,'U>[]) =
  inherit GestureNet<'T,'U>()

  override this.AddTokens(ts) =
    for n in this.Front do
      n.AddTokens(ts)

  override this.RemoveTokens(ts) =
    for n in subnets do
      n.RemoveTokens(ts)

type Sequence<'T,'U> when 'T :> System.Enum and 'U :> System.EventArgs ([<System.ParamArray>] subexprs:GestureExpr<'T,'U>[]) =
  inherit GestureExpr<'T,'U>()
  override this.ToNet(s) =
    let subnets = subexprs |> Array.map (fun x -> x.ToInternalGestureNet(s))
    let net = { new OperatorNet<_,_>(subnets) with
                override this.Front = subnets.[0].Front
                } :> GestureNet<_,_>
    for i = 0 to subnets.Length - 2 do
      subnets.[i].Completion.Add(fun (e,ts) -> subnets.[i+1].AddTokens(ts))
    subnets.[subnets.Length-1].Completion.Add(net.Completed)
    net


type Parallel<'T,'U> when 'T :> System.Enum and 'U :> System.EventArgs ([<System.ParamArray>] subexprs:GestureExpr<'T,'U>[]) =
  inherit GestureExpr<'T,'U>()
  override this.ToNet(s) =
    let subnets = subexprs |> Array.map (fun x -> x.ToInternalGestureNet(s))
    let net = { new OperatorNet<_,_>(subnets) with
                override this.Front = subnets |> Seq.map (fun x -> x.Front) |> List.concat
              } :> GestureNet<_,_>
    let completed = new System.Collections.Generic.Dictionary<Token,int>()
    let mycb (e,ts) =
      let mutable comp = []
      for t in ts do
        let found,count = completed.TryGetValue(t)
        let count = 1 + (if found then count else 0)
        if count = subnets.Length then
          completed.Remove(t) |> ignore
          comp <- t::comp
        else
          completed.[t] <- count
      if comp <> [] then
        net.Completed(e,comp)
    for n in subnets do
      n.Completion.Add(mycb)
    net

type Choice<'T,'U> when 'T :> System.Enum and 'U :> System.EventArgs ([<System.ParamArray>] subexprs:GestureExpr<'T,'U>[]) =
  inherit GestureExpr<'T,'U>()
  override this.ToNet(s) =
    let subnets = subexprs |> Array.map (fun x -> x.ToInternalGestureNet(s))
    let net = { new OperatorNet<_,_>(subnets) with
                override this.Front = subnets |> Seq.map (fun x -> x.Front) |> List.concat
                } :> GestureNet<_,_>
    for n in subnets do
      n.Completion.Add(fun (e,ts) ->
                       for othern in subnets do
                         if othern <> n then
                           othern.RemoveTokens(ts)
                       net.Completed(e,ts))
    net

type Iter<'T,'U> when 'T :> System.Enum and 'U :> System.EventArgs (x:GestureExpr<'T,'U>) =
  inherit GestureExpr<'T,'U>()
  override this.ToNet(s) =
    let subnet = x.ToInternalGestureNet(s)
    let net = { new OperatorNet<_,_>(subnet) with
                override this.Front = subnet.Front
                } :> GestureNet<_,_>
    subnet.Completion.Add(fun (e,ts) ->
                          subnet.AddTokens(ts)
                          this.Gestured(e))
    net
