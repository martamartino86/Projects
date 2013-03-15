// Per ulteriori informazioni su F#, visitare http://fsharp.net. Vedere il progetto 'Esercitazione su F#'
// per ulteriori linee guida sulla programmazione F#.

#load "Library1.fs"
open GestIT_FSharp

#load "FakeDriver.fs"
open FakeDriver

// Viene definito qui il codice di script della libreria


// Generic PETRI NET, useful for definition of subnets and entire tree.
[<AllowNullLiteralAttribute>]
type PetriNet =
    interface end

type IFeature =
    interface end

type PlaceState =
    | Enabled
    | Disabled

// PLACE of Petri Net
type Place (s:PlaceState, nt:int) =
    let mutable state = s
    let mutable ntoken = nt
    member this.GetState
        with get() = state

// PREDICATE associated to a particular Transition
type Predicate = delegate of unit -> bool

// TRANSITION of Petri Net
type Transition (x:Predicate) =
    let predicate = x

// TOKEN of Petri Net
type Token (p:Place) =
    let mutable place = p
    member this.SetPlace(p) = place <- p // with set p = place <- p

// Lista di token
let (listToken:Token List) = []

// Rapresentation of a ground term
type GroundTerm<'T> when 'T :> System.Enum (f:'T, x:Predicate, iterative:bool) =
    let p = new Place(PlaceState.Disabled, 0)
    let t = new Transition(x)
    let i = iterative
    interface PetriNet
    member this.getPlace
        with get() = p
    member this.getIterative
        with get() = i

// Abstract class for BinaryOperators, that represents all operators working on 2 operands.
[<AbstractClass>]
type BinaryOperator (p1:PetriNet, p2:PetriNet) =
    let leftOp = p1
    let rightOp = p2
    interface PetriNet
    abstract SetToken : unit -> bool * bool
    member x.GetLeftLeaf = leftOp
    member x.GetRightLeaf = rightOp

// Definitions of different Operators:
type Sequence (p1:PetriNet, p2:PetriNet) =
    inherit BinaryOperator(p1,p2)
    override x.SetToken () =
        match p1 with
        | :? GroundTerm<_> as gt -> // controllo solo il figlio sx!
                            let t = new Token(gt.getPlace)
                            t::listToken |> ignore
                            (true, true)
        | _ -> (false, false) // vuol dire che il figlio e' a sua volta un BinaryOperator, e dunque va visitato il sottoalbero

type Parallel (p1:PetriNet, p2:PetriNet) =
    inherit BinaryOperator(p1,p2)
    override x.SetToken () =
        let t1 = new Token(null)
        let t2 = new Token(null)
        match (p1, p2) with
        | (:? GroundTerm<_> as gt1), (:? GroundTerm<_> as gt2) -> // se entrambe i figli sono GroundTerm
                            t2::listToken |> ignore
                            t1::listToken |> ignore
                            t1.SetPlace(gt1.getPlace)
                            t2.SetPlace(gt2.getPlace)
                            (true, true)
        | (:? GroundTerm<_> as gt1), (_ as gt2) -> // se solo il figlio sx e' un GT
                            t1::listToken |> ignore
                            t1.SetPlace(gt1.getPlace)
                            (true, false)
        | (_ as gt1), (:? GroundTerm<_> as gt2) -> // se solo il figlio dx e' un GT
                            t2::listToken |> ignore
                            t2.SetPlace(gt2.getPlace)
                            (false, true)
        | (_, _) -> (false, false)

type Choice (p1:PetriNet, p2:PetriNet) =
    inherit BinaryOperator(p1,p2)

type Disabling (p1:PetriNet, p2:PetriNet) =
    inherit BinaryOperator(p1,p2)
    





// *** // *** // *** //

type Start () =
    interface IFeature

type Move () =
    interface IFeature

type End () =
    interface IFeature

// Gesture:  S >> M* [> E
let s = new GroundTerm<_>(TouchFeatureTypes.Start, (fun () -> true), false)
let m = new GroundTerm<_>(TouchFeatureTypes.Move, (fun () -> true), true)
let seq = new Sequence(s, m)

let e = new GroundTerm<_>(TouchFeatureTypes.End, (fun () -> true), false)
let dis = new Disabling(seq, e)

// Preparazione della rete (inserimento token)


// SIIIIISTEEEEEMAAAAAAAA
let rec DeepVisit (net:PetriNet) =
    match net with
    | :? BinaryOperator -> 
    | :? GroundTerm<_> -> null
    | _ -> null

let PrepareNet(net:PetriNet) =
    ()