// Per ulteriori informazioni su F#, visitare http://fsharp.net. Vedere il progetto 'Esercitazione su F#'
// per ulteriori linee guida sulla programmazione F#.

#load "Library1.fs"
open GestIT_FSharp

// Viene definito qui il codice di script della libreria

type PlaceState =
    | Enabled
    | Disabled

type Place (s:PlaceState, nt:int) =
    let mutable state = s
    let mutable ntoken = nt

type Transition (e:System.EventArgs) =
    let event = e

type PetriNet (p1:Place, p2:Place, t:Transition) =
    let place1 = p1
    let place2 = p2
    let transition = t
    let emptyTransition:Transition = new Transition(null) // transizione di appoggio per passare da una sottorete all'altra

