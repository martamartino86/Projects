module NewModel


(*
// Generic PETRI NET, useful for definition of subnets and entire tree.
[<AllowNullLiteralAttribute>]
type PetriNet<'T> when 'T :> System.Enum =
    interface
        abstract member SetOperator : PetriNet<'T> -> unit
        //abstract member IsActive: unit -> bool
    end


type IFeature =
    interface end

// PREDICATE associated to a particular Transition
type Predicate = delegate of unit -> bool

// Rapresentation of a ground term
type GroundTerm<'T> when 'T :> System.Enum (f:'T, x:Predicate) =
    let mutable parent = null
    member x.Feature = f
    member x.Predicate = x
    member x.Operator
        with get() = parent
        and set(p) = parent <- p
    interface PetriNet<'T>
   
type Token<'T> when 'T :> System.Enum (groundterm:GroundTerm<'T>) =
    let mutable gt = groundterm
    member x.GroundTerm
        with get() = gt
        and set(g) = gt <- g

let mutable (listToken:Token<'T> List) = []

// Abstract class for BinaryOperators, that represents all operators working on 2 operands.
[<AbstractClass>]
type BinaryOperator<'T> when 'T :> System.Enum (p1:PetriNet<'T>, p2:PetriNet<'T>) as x =
    let leftOp = p1
    let rightOp = p2
    do
        p1.SetOperator x
        p2.SetOperator x
    interface PetriNet<'T>
    // SetToken: viene invocato per (ri)posizionare il/i token in una sottorete
    abstract SetToken : unit -> bool * bool
    // FireToken: viene invocato da uno dei figli per far passare all'altro il token
    abstract FireToken: Token<'T> -> unit
    member x.GetLeftLeaf = leftOp
    member x.GetRightLeaf = rightOp

// OPERATORS:
type Sequence<'T> when 'T :> System.Enum (p1:PetriNet<'T>, p2:PetriNet<'T>) =
    inherit BinaryOperator<'T>(p1,p2)
    override x.SetToken () =
        match p1 with
        | :? GroundTerm<'T> as gt -> // controllo solo il figlio sx!
                            let t = new Token<_>(gt)
                            listToken <- t::listToken
                            (true, true)
        | _ -> (false, false) // vuol dire che il figlio e' a sua volta un BinaryOperator, e dunque va visitato il sottoalbero
    override x.FireToken (t:Token<'T>) =
        // togli il token da p1 e mettilo in p2
        if ((t.GroundTerm = (p1 :?> GroundTerm<'T>)) && (p2 :? GroundTerm<'T>)) then
            t.GroundTerm <- (p2 :?> GroundTerm<'T>)
        else
            (p2 :?> BinaryOperator<'T>).SetToken() |> ignore
      
type Parallel<'T> when 'T :> System.Enum (p1:PetriNet<'T>, p2:PetriNet<'T>) =
    inherit BinaryOperator<'T>(p1,p2)
    let mutable nTimes = 0
    override x.SetToken () =
        let t1 = new Token<_>(null)
        let t2 = new Token<_>(null)
        match (p1, p2) with
        | (:? GroundTerm<'T> as gt1), (:? GroundTerm<'T> as gt2) -> // se entrambe i figli sono GroundTerm
                            listToken <- t1::t2::listToken
                            t1.GroundTerm <- gt1
                            t2.GroundTerm <- gt2
                            (true, true)
        | (:? GroundTerm<'T> as gt1), (_ as gt2) -> // se solo il figlio sx e' un GT
                            listToken <- t1::listToken                        
                            t1.GroundTerm <- gt1
                            (true, false)
        | (_ as gt1), (:? GroundTerm<'T> as gt2) -> // se solo il figlio dx e' un GT
                            listToken <- t2::listToken
                            t2.GroundTerm <- gt2
                            (false, true)
        | (_, _) -> (false, false)
    override x.FireToken (t:Token<'T>) =
        if (nTimes % 2 = 0) then
            ()

type Choice<'T> when 'T :> System.Enum (p1:PetriNet<'T>, p2:PetriNet<'T>) =
    inherit BinaryOperator<'T>(p1,p2)
    override x.SetToken() =
        let t1 = new Token<_>(null)
        let t2 = new Token<_>(null)
        match (p1, p2) with
        | (:? GroundTerm<'T> as gt1), (:? GroundTerm<'T> as gt2) -> // se entrambe i figli sono GroundTerm
                            listToken <- t1::t2::listToken
                            t1.GroundTerm <- gt1
                            t2.GroundTerm<- gt2
                            (true, true)
        | (:? GroundTerm<'T> as gt1), (_ as gt2) -> // se solo il figlio sx e' un GT
                            listToken <- t1::listToken                        
                            t1.GroundTerm <- gt1
                            (true, false)
        | (_ as gt1), (:? GroundTerm<'T> as gt2) -> // se solo il figlio dx e' un GT
                            listToken <- t2::listToken
                            t2.GroundTerm <- gt2
                            (false, true)
        | (_, _) -> (false, false)

type Disabling<'T> when 'T :> System.Enum (p1:PetriNet<'T>, p2:PetriNet<'T>) =
    inherit BinaryOperator<'T>(p1,p2)
     override x.SetToken() =
        match p1 with
        | :? GroundTerm<'T> as gt -> // controllo solo il figlio sx!
                            let t = new Token<_>(gt)
                            listToken <- t::listToken
                            (true, true)
        | _ -> (false, false) // vuol dire che il figlio e' a sua volta un BinaryOperator, e dunque va visitato il sottoalbero
    

// Preparazione della rete (inserimento token)

let rec DeepVisit (net:PetriNet<'T>) =
    if (net :? BinaryOperator<'T>) then
        let netBin = net :?> BinaryOperator<'T>
        let (b1, b2) = netBin.SetToken()
        if ((b1, b2) = (false, false)) then
            DeepVisit(netBin.GetLeftLeaf)
            DeepVisit(netBin.GetRightLeaf)
        else if ((b1, b2) = (true, false)) then
            DeepVisit(netBin.GetRightLeaf)
        else if ((b1, b2) = (false, true)) then
            DeepVisit(netBin.GetLeftLeaf)
        else
            ()

let PrepareNet(net:PetriNet<'T>) =
    DeepVisit(net)

type GestureEvent<'T> when 'T :> System.Enum (f:'T) =
    let mutable feature = f
    member x.Feature
        with get() = feature
        and set(f) = feature <- f


// funzioni in libertà temporanea
let receivedEvent(e:GestureEvent<'T>) =
    for t in listToken do
        //if (t.GroundTerm.Feature = e.Feature) then
            (t.GroundTerm.Operator :?> BinaryOperator<'T>).FireToken(t)

*)