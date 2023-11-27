// ----------------------------------------------------------------------------
// 03 - Searching for clauses & variable renaming
// ----------------------------------------------------------------------------

type Term =
    | Atom of string
    | Variable of string
    | Predicate of string * Term list

type Clause = { Head: Term; Body: Term list }

type Program = Clause list

let fact p = { Head = p; Body = [] }

let rule p b = { Head = p; Body = b }

// ----------------------------------------------------------------------------
// Substitutions and unification of terms
// ----------------------------------------------------------------------------


let rec substitute (subst: Map<string, Term>) (term: Term) =
    match term with
    | Variable(v) when subst.ContainsKey v -> subst.[v]
    | Predicate(name, args) ->
        let substArgs = substituteTerms subst args
        Predicate(name, substArgs)
    | _ -> term


and substituteSubst (newSubst: Map<string, Term>) (subst: list<string * Term>) =
    List.map (fun (n, t) -> (n, substitute newSubst t)) subst


and substituteTerms (subst: Map<string, Term>) (terms: list<Term>) =
    List.map (fun t -> substitute subst t) terms


let rec unifyLists l1 l2 =
    match l1, l2 with
    | [], [] -> Some([])
    | h1 :: t1, h2 :: t2 ->
        let s1 = unify h1 h2

        match s1 with
        | Some(su1) ->
            let t1 = substituteTerms (Map.ofList su1) t1
            let t2 = substituteTerms (Map.ofList su1) t2
            let s2 = unifyLists t1 t2

            match s2 with
            | Some(su2) ->
                let su1 = substituteSubst (Map.ofList su2) su1
                Some(su1 @ su2)
            | _ -> None
        | _ -> None
    | _ -> None

and unify t1 t2 : option<list<string * Term>> =
    match t1, t2 with
    | Atom(a1), Atom(a2) when a1 = a2 -> Some([])
    | Predicate(p1, l1), Predicate(p2, l2) when p1 = p2 -> unifyLists l1 l2
    | Variable(v), _ -> Some([ (v, t2) ])
    | _, Variable(v) -> Some([ (v, t1) ])
    | _ -> None

// ----------------------------------------------------------------------------
// Searching the program (database) and variable renaming
// ----------------------------------------------------------------------------

let nextNumber =
    let mutable n = 0

    fun () ->
        n <- n + 1
        n

let rec freeVariables (term: Term) =
    // TODO: Return a list of all variables that appear in 'term'
    // (this may contain duplicates, we will eliminate them below)
    // HINT: Use List.collect: ('a -> list<'b>) -> list<'a> -> list<'b>
    match term with
    | Variable(v) -> [ v ]
    | Predicate(_, args) -> List.collect freeVariables args
    | _ -> []


let withFreshVariables (clause: Clause) : Clause =
    // TODO: Get a list of distinct variables in the clause (using
    // 'freeVariables' and 'List.distinct'), generate a substitution
    // that append a number 'n' obtained by 'nextNumber()' to the end
    // of all the variable names, and apply the substitutions to the
    // head and body of the clause.
    //
    // For example, 'grandparent(X,Y) :- parent(X,Z), parent(Z,Y)' may
    // become 'grandparent(X3,Y3) :- parent(X3,Z3), parent(Z3,Y3)'
    //
    // This may not be correct if the user-provided names of variables
    // had numbers in them in a certain format, but that's OK for now!
    let headVar = freeVariables clause.Head
    let bodyVars = List.collect freeVariables clause.Body
    let distinctVars = List.distinct (headVar @ bodyVars)

    match distinctVars with
    | [] -> clause
    | _ ->
        let num = nextNumber ()

        let nameSubst =
            Map.ofList (List.map (fun v -> (v, Variable(v + string num))) distinctVars)

        let newHead = substitute nameSubst clause.Head
        let newBody = substituteTerms nameSubst clause.Body
        { Head = newHead; Body = newBody }


let query (program: list<Clause>) (query: Term) : list<Clause * list<string * Term>> =
    // TODO: Return all clauses from 'program' whose 'Head' can be
    // unified with the specified 'query' and return the resulting
    // substitutions. Before unifying, rename variables in the program
    // rule using 'withFreshVariables'. You can do this using 'List.choose'
    // or by using list comprehension.
    //
    // The return type of this is a list of tuples consisting of the matching
    // clause and a substitution (list<string * Term>). Calling 'unify'
    // gives you 'option<list<string * Term>>', so you need to pattern match
    // on this and if it is 'Some(subst)' return 'Some(clause, subst)'.
    let renamedProgram = List.map withFreshVariables program
    let unifications = List.map (fun c -> (c, unify c.Head query)) renamedProgram

    List.fold (fun res (c, s) ->
      match s with
      | Some(subst) -> (c, subst) :: res
      | _ -> res
      ) [] unifications


// ----------------------------------------------------------------------------
// Querying the British royal family
// ----------------------------------------------------------------------------

// Generating fresh variables - repeated calls
// should append new number to all variable names
rule
    (Predicate("grandparent", [ Variable("X"); Variable("Y") ]))
    [ Predicate("parent", [ Variable("X"); Variable("Z") ])
      Predicate("parent", [ Variable("Z"); Variable("Y") ]) ]
|> withFreshVariables

// Some information about the British royal family
let family =
    [ fact (Predicate("male", [ Atom("William") ]))
      fact (Predicate("female", [ Atom("Diana") ]))
      fact (Predicate("male", [ Atom("Charles") ]))
      fact (Predicate("male", [ Atom("George") ]))
      fact (Predicate("parent", [ Atom("Diana"); Atom("William") ]))
      fact (Predicate("parent", [ Atom("Charles"); Atom("William") ]))
      fact (Predicate("parent", [ Atom("William"); Atom("George") ]))
      rule
          (Predicate("father", [ Variable("X"); Variable("Y") ]))
          [ Predicate("parent", [ Variable("X"); Variable("Y") ])
            Predicate("male", [ Variable("X") ]) ] ]

// Query: male(X)
// Match #1: male(William)
// Match #2: male(Charles)
// Match #3: male(George)
query family (Predicate("male", [ Variable("X") ]))

// Query: father(X, William)
// Match #1: father(X, Y) :- parent(X, Y), male(X)
query family (Predicate("father", [ Variable("X"); Atom("William") ]))
