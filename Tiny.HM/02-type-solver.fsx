// ----------------------------------------------------------------------------
// 02 - Solving type constraints with numbers and Booleans
// ----------------------------------------------------------------------------

// NOTE: We will only need lists later, but to make this exercise
// a bit more interesting, we will implement constraint resolution
// for lists here already. This will help you in the next steps!
type Type =
    | TyVariable of string
    | TyBool
    | TyNumber
    | TyList of Type

let rec occursCheck (vcheck: string) (ty: Type) =
    match ty with
    | TyVariable(v) -> v = vcheck
    | TyList(listType) -> occursCheck vcheck listType
    | _ -> false

let rec substituteAll (subst: Map<string, Type>) ty =
    match ty with
    | TyVariable(varName) when subst.ContainsKey varName -> subst.[varName]
    | TyList(listType) -> TyList(substituteAll subst listType)
    | _ -> ty

let substituteConstrs (subst: Map<string, Type>) (cs: list<Type * Type>) =
    List.map (fun (l, r) -> (substituteAll subst l, substituteAll subst r)) cs


let rec solve cs =
    match cs with
    | [] -> []
    | (TyNumber, TyNumber) :: cs -> solve cs
    | (TyBool, TyBool) :: cs -> solve cs
    | (TyList(t1), TyList(t2)) :: cs -> solve ((t1, t2) :: cs)
    | (t, TyVariable v) :: cs
    | (TyVariable v, t) :: cs ->
        if occursCheck v t then
            failwith "Does not type check (occurs check)"

        let cs = substituteConstrs (Map.ofList [ v, t ]) cs
        let subst = solve cs
        let t = substituteAll (Map.ofList subst) t
        (v, t) :: subst
    | _ -> failwith "Does not type check (unsatisfiable constraint)"


// ----------------------------------------------------------------------------
// Constraint solving tests
// ----------------------------------------------------------------------------

// Can be solved ('a = number, 'b = list<number>)
solve
    [ TyList(TyVariable("a")), TyList(TyNumber)
      TyVariable("b"), TyList(TyVariable("a")) ]

// // Cannot be solved (list<'a> <> bool)
// solve
//   [ TyList(TyVariable("a")), TyVariable("b")
//     TyVariable("b"), TyBool ]

// Can be solved ('a = number, 'b = list<number>)
solve [ TyList(TyVariable("a")), TyVariable("b"); TyVariable("b"), TyList(TyNumber) ]

// Cannot be solved ('a <> list<'a>)
solve [ TyList(TyVariable("a")), TyVariable("a") ]
