namespace GestIT_FSharp

    // Operator: sono gli operatori base applicabili ai ground-term per la definizione di gestures.
    type OperatorType =
        | Sequence
        | Parallel
        | Choice
        | OrderIndipendence
        | Disable

    // Status: rappresenta lo stato del pozzo della rete di Petri.
    type Status =
        | Enabled       // ha il token al suo interno
        | Disabled      // altrimenti

    // GestureTree: rappresenta l'albero per il riconoscimento della gesture
    type GestureTree () =
        class end

    type GestureEvent() =
        inherit System.EventArgs()

    type Token () =
        class end

    // GestureLeafContent: rappresenta il contenuto di una foglia dell'albero, cioè di SimpleTmpExp.
    type GestureLeafContent () =
        // da overridare: il metodo override dovrà fare i giusti confronti sull'evento per capire se la foglia lo ammette oppure no
        member x.Accept(e:GestureEvent) =
            false
        member x.Consume(e:GestureEvent) =
            false

    // SimpleTmpExp: rappresenta una foglia dell'albero, che a sua volta è un oggetto ground-term (es. TouchStart)
    //              Tutti i pozzi, eccetto il primo, vengono creati senza token al loro interno e Disabled.
    type GestureLeaf (c:GestureLeafContent, nt:int, s:Status) =
        inherit GestureTree()

        let content = c
        let mutable nToken = nt
        let mutable state = s
        
        member x.AcceptToken(e:GestureEvent) =
            if ((state = Status.Enabled) && (content.Accept(e))) then true
            else false

        member x.ConsumeToken(e:GestureEvent) =
            if (content.Consume(e) = false) then false
            else
                state <- Status.Disabled
                nToken <- nToken - 1
                true
        
        
        
                
//    type Class1() = 
//        member this.X = "F#"
