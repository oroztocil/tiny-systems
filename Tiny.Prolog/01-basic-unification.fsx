// ----------------------------------------------------------------------------
// 01 - Implementing basic unication of terms
// ----------------------------------------------------------------------------

// A term is a recursive type, which can be either an atom (a known fact
// like 'socrates'), a variable (to which we want to asign a term by
// unification), or a predicate (with predicate name and a list of arguments).
type Term =
    | Atom of string
    | Variable of string
    | Predicate of string * Term list
//| Call of Term * Term list

// A clause is for example 'mortal(X) :- human(X)' or 'human(socrates)'. It
// consists of a head and body (head :- body). Body is a sequence of terms.
type Clause = { Head: Term; Body: Term list }

// A program is just a list of clauses
type Program = Clause list

// Create a clause that states a fact
let fact p = { Head = p; Body = [] }

// Create a clause that defines a rule
let rule p b = { Head = p; Body = b }

// ----------------------------------------------------------------------------
// Unification of terms and lists
// ----------------------------------------------------------------------------

let rec unifyLists l1 l2 : option<list<string * Term>> =
    match l1, l2 with
    | [], [] ->
        // TODO: Succeeds, but returns an empty substitution
        Some([])
    | h1 :: t1, h2 :: t2 ->
        // TODO: Unify 'h1' with 'h2' using 'unify' and
        // 't1' with 't2' using 'unifyLists'. If both
        // succeed, return the generated joint substitution!
        let headSubst = unify h1 h2
        let listSubst = unifyLists t1 t2

        match headSubst, listSubst with
        | Some(hs), Some(ls) -> Some(hs @ ls)
        | _ -> None
    | _ ->
        // TODO: Lists cannot be unified
        None

and unify (t1: Term) (t2: Term) =
    // TODO: Add all the necessary cases here!
    // * For matching atoms, return empty substitution
    // * For matching predicates, return the result of 'unifyLists'
    // * For variable and any term, return a new substitution
    // * For anything else, return None (failed to unify)
    match t1, t2 with
    | Atom(a1), Atom(a2) when a1 = a2 -> Some([])
    | Predicate(p1, l1), Predicate(p2, l2) when p1 = p2 -> unifyLists l1 l2
    | Variable(v), _ -> Some([ v, t2 ])
    | _, Variable(v) -> Some([ (v, t1) ])
    | _ -> None

// ----------------------------------------------------------------------------
// Basic unification tests
// ----------------------------------------------------------------------------

// Example: human(socrates) ~ human(X)
// Returns: [X -> socrates]
unify (Predicate("human", [ Atom("socrates") ])) (Predicate("human", [ Variable("X") ]))

// Example: human(socrates) ~ mortal(X)
// Returns: None (fail)
unify (Predicate("human", [ Atom("socrates") ])) (Predicate("mortal", [ Variable("X") ]))

// Example: parent(charles, harry) ~ parent(charles, X)
// Returns: [X -> harry]
unify
    (Predicate("parent", [ Atom("charles"); Atom("harry") ]))
    (Predicate("parent", [ Atom("charles"); Variable("X") ]))

// Example: parent(X, harry) ~ parent(charles, Y)
// Returns: [X -> charles; Y -> harry]
unify (Predicate("parent", [ Variable("X"); Atom("harry") ])) (Predicate("parent", [ Atom("charles"); Variable("Y") ]))

// Example: succ(succ(succ(zero))) ~ succ(X)
// Returns: [X -> succ(succ(zero))]
unify
    (Predicate("succ", [ Predicate("succ", [ Predicate("succ", [ Atom("zero") ]) ]) ]))
    (Predicate("succ", [ Variable("X") ]))

// Example: succ(succ(zero)) ~ succ(zero)
// Returns: None (fail)
unify (Predicate("succ", [ Predicate("succ", [ Atom("zero") ]) ])) (Predicate("succ", [ Atom("zero") ]))
