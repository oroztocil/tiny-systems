// ----------------------------------------------------------------------------
// 01 - Complete the simple numerical constraint solver
// ----------------------------------------------------------------------------

type Number =
    | Zero
    | Succ of Number
    | Variable of string


// NOTE: The four functions below currently return a wrong
// result, but one that makes the code run. As you implement
// them (one by one), the tests should graudally start working.


let rec occursCheck (v: string) (n: Number) =
    match n with
    | Variable(v2) -> v = v2
    | Succ(n2) -> occursCheck v n2
    | Zero -> false

let rec substitute (v: string) (subst: Number) (n: Number) =
    match n with
    | Zero -> Zero
    | Succ n2 -> Succ(substitute v subst n2)
    | Variable v2 when v = v2 -> subst
    | _ -> n

let substituteConstraints (v: string) (subst: Number) (constraints: list<Number * Number>) =
    // TODO: Substitute 'v' for 'subst' (use 'substitute') in
    // all numbers in all the constraints in 'constraints'
    List.map (fun (l, r) -> (substitute v subst l, substitute v subst r)) constraints

let substituteAll (subst: list<string * Number>) (n: Number) =
    List.fold (fun n2 (v, s) -> substitute v s n2) n subst

let rec solve constraints =
    match constraints with
    | [] -> []
    | (Succ n1, Succ n2) :: constraints -> solve ((n1, n2) :: constraints)
    | (Zero, Zero) :: constraints -> solve constraints
    | (Succ _, Zero) :: _
    | (Zero, Succ _) :: _ -> failwith "Cannot be solved"
    | (n, Variable v) :: constraints
    | (Variable v, n) :: constraints ->
        if occursCheck v n then
            failwith "Cannot be solved (occurs check)"

        let constraints = substituteConstraints v n constraints
        let subst = solve constraints
        let n = substituteAll subst n
        (v, n) :: subst

// Should work: x = Zero
solve [ Succ(Variable "x"), Succ(Zero) ]

// // Should faild: S(Z) <> Z
// solve
//   [ Succ(Succ(Zero)), Succ(Zero) ]

// Not done: Need to substitute x/Z in S(x)
solve [ Succ(Variable "x"), Succ(Zero); Variable "y", Succ(Variable "x") ]

// Not done: Need to substitute z/Z in S(S(z))
solve [ Variable "x", Succ(Succ(Variable "z")); Succ(Variable "z"), Succ(Zero) ]

// Not done: Need occurs check
solve [ Variable "x", Succ(Variable "x") ]
