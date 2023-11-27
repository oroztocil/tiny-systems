// ----------------------------------------------------------------------------
// 04 - Generating and solving goals recursively
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
    match term with
    | Variable(v) -> [ v ]
    | Predicate(_, args) -> List.collect freeVariables args
    | _ -> []


let withFreshVariables (clause: Clause) : Clause =
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
    let renamedProgram = List.map withFreshVariables program
    let unifications = List.map (fun c -> (c, unify c.Head query)) renamedProgram

    List.fold
        (fun res (c, s) ->
            match s with
            | Some(subst) -> (c, subst) :: res
            | _ -> res)
        []
        unifications

let rec solve program (subst : list<string * Term>) goals =
    match goals with
    | g :: goals ->
        // TODO: We need to solve the goal (term) 'g'. To do so, find all
        // matching clauses in the 'program' using 'query' and iterate over
        // the returned list using 'for clause, newSubst in matches do'.
        // For each possible solution, we need to add the 'clause.Body' to
        // the list of 'goals' and apply the substitution 'newSubst' to the
        // new concatentated list of 'goals'. Then we need to apply the
        // substitution 'newSubst' to the substitution 'subst' we have so far,
        // append the two and call 'solve' recursively with this new substitution
        // to solve the new goals.
        let matches = query program g
        // System.Console.WriteLine("\nMATCHES")
        // System.Console.WriteLine(matches)

        // System.Console.WriteLine("\nGOALS")
        // System.Console.WriteLine(goals)

        // System.Console.WriteLine("\nSUBST")
        // System.Console.WriteLine(subst)

        for clause, newSubst in matches do
            let newGoals = clause.Body @ goals
            let newGoals = substituteTerms (Map.ofList newSubst) newGoals
            let updatedSubst = substituteSubst (Map.ofList newSubst) subst
            solve program (updatedSubst @ newSubst) newGoals

    | [] ->
        // TODO: We solved all goals, which means 'subst' is a possible solution!
        // Print 'subst' (either using printfn "%A" or in some nicer way).
        printfn "%A" subst

// ----------------------------------------------------------------------------
// Querying the British royal family
// ----------------------------------------------------------------------------

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

// Query: father(X, William)
// Result #1: [ X -> Charles, ... ]
solve family [] [ Predicate("father", [ Variable("X"); Atom("William") ]) ]

// Query: father(X, Y)
// Result #1: [ X -> Charles, Y -> William, ... ]
// Result #2: [ X -> George, Y -> William, ... ]
solve family [] [ Predicate("father", [ Variable("X"); Variable("Y") ]) ]
