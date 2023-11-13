// ----------------------------------------------------------------------------
// Type inference for binary operators and conditionals
// ----------------------------------------------------------------------------

type Expression =
    | Constant of int
    | Binary of string * Expression * Expression
    | If of Expression * Expression * Expression
    | Variable of string
    // NOTE: Added three more kinds of expression from TinyML
    | Application of Expression * Expression
    | Lambda of string * Expression
    | Let of string * Expression * Expression

type Type =
    | TyVariable of string
    | TyBool
    | TyNumber
    | TyList of Type
    // NOTE: Added type for functions (of single argument)
    | TyFunction of Type * Type

// ----------------------------------------------------------------------------
// Constraint solving
// ----------------------------------------------------------------------------

let rec occursCheck (vcheck: string) (ty: Type) =
    "CHECKING " |> System.Console.Write
    vcheck |> System.Console.Write
    " IN " |> System.Console.Write
    ty |> System.Console.WriteLine
    match ty with
    | TyVariable(v) -> v = vcheck
    | TyList(listType) -> occursCheck vcheck listType
    | TyFunction(argType, retType) -> (occursCheck vcheck argType) || (occursCheck vcheck retType)
    | _ -> false

let rec substituteAll (subst: Map<string, Type>) ty =
    match ty with
    | TyVariable(varName) when subst.ContainsKey varName -> subst.[varName]
    | TyList(listType) -> TyList(substituteAll subst listType)
    // TODO: Add case for 'TyFunction' (need to substitute in both nested types)
    | TyFunction(argType, retType) -> TyFunction(substituteAll subst argType, substituteAll subst retType)
    | _ -> ty

let substituteConstraints (subst: Map<string, Type>) (cs: list<Type * Type>) =
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

        let cs = substituteConstraints (Map.ofList [ v, t ]) cs
        let subst = solve cs
        let t = substituteAll (Map.ofList subst) t
        (v, t) :: subst
    | (TyFunction(argType1, retType1), TyFunction(argType2, retType2)) :: cs ->
        // if occursCheck v t then
        //     failwith "Does not type check (occurs check)"

        // if occursCheck v t then
        //     failwith "Does not type check (occurs check)"
        solve ((argType1, argType2) :: (retType1, retType2) :: cs)
    | _ -> failwith "Does not type check (unsatisfiable constraint)"

// ----------------------------------------------------------------------------
// Constraint generation & inference
// ----------------------------------------------------------------------------

type TypingContext = Map<string, Type>

// NOTE: You will need this helper in checking of Lambda and Application.
// It generates a new type variable each time you call 'newTypeVariable()'
let newTyVariable =
    let mutable n = 0

    fun () ->
        n <- n + 1
        TyVariable(sprintf "_a%d" n)

let rec generate (ctx: TypingContext) e =
    match e with
    | Constant _ -> TyNumber, []

    | Binary("+", e1, e2)
    | Binary("*", e1, e2) ->
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

    | Let(v, e1, e2) ->
        let t1, s1 = generate ctx e1
        let ctx = Map.add v t1 ctx
        let t2, s2 = generate ctx e2
        let cs = s1 @ s2
        t2, cs

    | Lambda(v, e) ->
        let tArg = newTyVariable ()
        let ctx = Map.add v tArg ctx
        let tRet, sRet = generate ctx e
        TyFunction(tArg, tRet), sRet

    | Application(e1, e2) ->
        // TODO: Tricky case! We cannot inspect the generated type of 'e1'
        // to see what the argument/return type of the function is. Instead,
        // we have to generate a new type variable and add a constraint.
        let tLam, sLam = generate ctx e1
        let tApp, sApp = generate ctx e2
        let tRet = newTyVariable ()
        let cs = sLam @ sApp @ [ tLam, TyFunction(tApp, tRet) ]
        tRet, cs


// ----------------------------------------------------------------------------
// Putting it together & test cases
// ----------------------------------------------------------------------------

// Run both of the phases and return the resulting type
let infer e =
    let typ, constraints = generate Map.empty e
    "RESULT:" |> System.Console.WriteLine
    "TYPE: " |> System.Console.Write
    typ |> System.Console.WriteLine
    "CONSTRAINTS: " |> System.Console.Write
    constraints |> System.Console.WriteLine
    "SUBSTITUTIONS: " |> System.Console.Write
    let subst = solve constraints
    subst |> System.Console.WriteLine
    "EXPR TYPE: " |> System.Console.Write
    let typ = substituteAll (Map.ofList subst) typ
    typ


// NOTE: Using the above, you will end up with ugly random type variable
// names like '_a4' etc. You can improve this by collecting all the type
// variable names that appear in a type and substituting them with a
// list of nice names. Useful bit of code to generate the substitution is:
//
//   Map.ofList [ for i, n in Seq.indexed ["_a4"; "_a5"] ->
//     n, string('a' + char i) ]
//
// You would still need to write code to collect all type variables in a type.


// let x = 10 in x = 10
Let("x", Constant 10, Binary("=", Variable "x", Constant 10)) |> infer

// let f = fun x -> x*2 in (f 20) + (f 1)
Let(
    "f",
    Lambda("x", Binary("*", Variable("x"), Constant(2))),
    Binary("+", Application(Variable("f"), Constant(20)), Application(Variable("f"), Constant(1)))
)
|> infer

// fun x f -> f (f x)
Lambda("x", Lambda("f", Application(Variable "f", Application(Variable "f", Variable "x"))))
|> infer

Application(Lambda("x", Lambda("f", Application(Variable "f", Application(Variable "f", Variable "x")))), Constant(1))
|> infer

// fun f -> f f
// This does not type check due to occurs check
Lambda("f", Application(Variable "f", Variable "f")) |> infer

// fun f -> f 1 + f (2 = 3)
// This does not type check because argument of 'f' cannot be both 'int' and 'bool'
Lambda(
    "f",
    Binary("+", Application(Variable "f", Constant 1), Application(Variable "f", Binary("=", Constant 2, Constant 3)))
)
|> infer
