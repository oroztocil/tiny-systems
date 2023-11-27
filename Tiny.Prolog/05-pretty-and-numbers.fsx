// ----------------------------------------------------------------------------
// 05 - Pretty printing & adding numbers to TinyProlog
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
// Pretty printing terms
// ----------------------------------------------------------------------------

let rec (|Number|_|) term =
    match term with
    | Atom("zero") -> Some(0)
    | Predicate("succ", arg) ->
        match arg with
        | [ Number n ] -> Some(n + 1)
        | _ -> None
    | _ ->
        // TODO: Write an active pattern to recognize numbers in the form used below.
        // If the term is 'Atom("zero")' return Some(0).
        // If the term is 'Predicate("succ", [n])' where 'n' is itself
        // a term representing number, return the number value +1.
        None


let rec formatTerm term =
    match term with
    // Simple cases for number, atom and variable are done already...
    | Number n -> string n
    | Atom s -> s
    | Variable v -> v
    | Predicate(p, items) ->
        // TODO: format all arguments recursively using 'formatTerm'
        // You can then concatenate the arguments using 'String.concat'
        p + "(" + (String.concat "," (List.map formatTerm items)) + ")"

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

let rec solveRec program (subst: list<string * Term>) goals : list<string * Term> =
    match goals with
    | g :: goals ->
        let matches = query program g
        let mutable result = []

        for clause, newSubst in matches do
            let newGoals = clause.Body @ goals
            let newGoals = substituteTerms (Map.ofList newSubst) newGoals
            let updatedSubst = substituteSubst (Map.ofList newSubst) subst
            result <- result @ solveRec program (updatedSubst @ newSubst) newGoals

        result
    | [] ->
        // TODO: When printing the computed substitution 'subst', print
        // the terms nicely using 'formatTerm'. You can use 'for' loop like:
        // 'for var, term in subst do printfn ...'
        subst
// for var, term in subst do
// printfn "%s = %s" var (formatTerm term)

let solve program goals =
    let targetVars = List.collect (fun t -> freeVariables t) goals
    let result = solveRec program [] goals
    let result = List.filter (fun (s, t) -> List.contains s targetVars) result

    for var, term in result do
        printfn "%s = %s" var (formatTerm term)

// ----------------------------------------------------------------------------
// Querying the British royal family
// ----------------------------------------------------------------------------

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

// Queries from previous step (now with readable output)
solve family [ Predicate("father", [ Variable("X"); Atom("William") ]) ]
solve family [ Predicate("father", [ Variable("X"); Variable("Y") ]) ]


// ----------------------------------------------------------------------------
// Calculating with numbers
// ----------------------------------------------------------------------------

// Helper that generates a term representing a number
let rec num n =
    match n with
    | 0 -> Atom("zero")
    | n -> Predicate("succ", [ num (n - 1) ])

// Addition and equality testing for Peano arithmetic
// $ add(zero, X, X)
// $ add(succ(X), Y, succ(Z)) :- add(X, Y, Z)
// $ eq(X, X)
let nums =
    [ fact (Predicate("add", [ Atom("zero"); Variable("X"); Variable("X") ]))
      rule
          (Predicate(
              "add",
              [ Predicate("succ", [ Variable("X") ])
                Variable("Y")
                Predicate("succ", [ Variable("Z") ]) ]
          ))
          [ Predicate("add", [ Variable("X"); Variable("Y"); Variable("Z") ]) ]
      fact (Predicate("eq", [ Variable("X"); Variable("X") ])) ]


// Query: add(2, 3, X)
// Output should include: 'X = 5'
//   (and other variables resulting from recursive calls)
solve nums [ Predicate("add", [ num 2; num 3; Variable("X") ]) ]

// Query: add(2, X, 5)
// Output should include: 'X = 3'
//   (we can use 'add' to calculate subtraction too!)
solve nums [ Predicate("add", [ num 2; Variable("X"); num 5 ]) ]

// Query: add(2, Y, X)
// Output should include: 'Y = Z??' and 'X = succ(succ(Z??))'
//   (with some number for ?? - indicating that this can be any term)
solve nums [ Predicate("add", [ num 2; Variable("Y"); Variable("X") ]) ]
