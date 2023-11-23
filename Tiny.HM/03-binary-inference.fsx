// ----------------------------------------------------------------------------
// 03 - Type inference for binary operators and conditionals
// ----------------------------------------------------------------------------

// NOTE: Start with some basic expressions from TinyML
// This time, If requires a real Boolean argument and we have
// operators '+' (int -> int -> int) and '=' (int -> int -> bool)
type Expression =
    | Constant of int
    | Binary of string * Expression * Expression
    | If of Expression * Expression * Expression
    | Variable of string

type Type =
    | TyVariable of string
    | TyBool
    | TyNumber
    | TyList of Type

// ----------------------------------------------------------------------------
// Constraint solving
// ----------------------------------------------------------------------------


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
// Constraint generation & inference
// ----------------------------------------------------------------------------

// Variable context to keep types of declared variables
// (those will typically be TyVariable cases, but don't have to)
type TypingContext = Map<string, Type>

let rec generate (ctx: TypingContext) e =
    match e with
    | Constant _ -> TyNumber, []

    | Binary("+", e1, e2) ->
        let t1, s1 = generate ctx e1
        let t2, s2 = generate ctx e2
        let cs = s1 @ s2 @ [ t1, TyNumber; t2, TyNumber ]
        TyNumber, cs

    | Binary("=", e1, e2) ->
        let t1, s1 = generate ctx e1
        let t2, s2 = generate ctx e2
        let cs = s1 @ s2 @ [ t1, TyNumber; t2, TyNumber ]
        TyBool, cs

    | Binary(op, _, _) -> failwithf "Binary operator '%s' not supported." op

    | Variable v -> ctx.[v], []

    | If(eCond, eThen, eElse) ->
        let tCond, sCond = generate ctx eCond
        let tThen, sThen = generate ctx eThen
        let tElse, sElse = generate ctx eElse
        let cs = sCond @ sThen @ sElse @ [ tCond, TyBool; tThen, tElse ]
        TyNumber, cs


// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------


// Simple expressions: x = 10 + x
// Assuming x:'a, infers that 'a = int
let e1 = Binary("=", Variable("x"), Binary("+", Constant(10), Variable("x")))

let t1, cs1 = generate (Map.ofList [ "x", TyVariable "a" ]) e1

solve cs1

// Simple expressions: if x then 2 + 1 else y
// Assuming x:'a, y:'b, infers 'a = bool, 'b = int
let e2 = If(Variable("x"), Binary("+", Constant(2), Constant(1)), Variable("y"))

let t2, cs2 = generate (Map.ofList [ "x", TyVariable "a"; "y", TyVariable "b" ]) e2

solve cs2

// Simple expressions: if x then 2 + 1 else x
// Cannot be solved, because 'x' used as 'int' and 'bool'
let e3 = If(Variable("x"), Binary("+", Constant(2), Constant(1)), Variable("x"))

let t3, cs3 = generate (Map.ofList [ "x", TyVariable "a"; "y", TyVariable "b" ]) e3

solve cs3
