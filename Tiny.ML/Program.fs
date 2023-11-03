open Tiny.ML
open Tiny.ML.Interpreter

open type System.Console

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

// Basic arithmentic: (1*2) + (20*2)
let eba1 =
    Binary("+", Binary("*", Constant(1), Constant(2)), Binary("*", Constant(20), Constant(2)))

evaluate Map.empty eba1 |> WriteLine

// Basic artihmetic with variables: x + (x*20)
let eba2 = Binary("+", Variable("x"), Binary("*", Variable("x"), Constant(20)))

let ctx1 = Map.ofList [ "x", (lazy ValNum 4) ]
evaluate ctx1 eba2 |> WriteLine
let ctx2 = Map.ofList [ "x", (lazy ValNum 2) ]
evaluate ctx2 eba2 |> WriteLine

// Arithmetic with unary operator: (1*2) + (-(-20 * 2))
let euo =
    Binary("+", Binary("*", Constant(1), Constant(2)), Unary("-", Binary("*", Constant(-20), Constant(2))))

evaluate Map.empty euo |> WriteLine

// Conditional expression: if 1 then 42 else 0
let eif1 = If(Constant(1), Constant(42), Constant(0))
evaluate Map.empty eif1 |> WriteLine

// Conditional expression: if 5+(-4) then 21*2 else 0
let eif2 =
    If(Binary("+", Constant(5), Unary("-", Constant(4))), Binary("*", Constant(21), Constant(2)), Constant(0))

evaluate Map.empty eif2 |> WriteLine

// Conditional expression: if 5+4 then 0 else 21*2
let eif3 =
    If(Binary("+", Constant(5), Constant(4)), Constant(0), Binary("*", Constant(21), Constant(2)))

evaluate Map.empty eif3 |> WriteLine

// Basic function declaration (should return closure)
//   (fun x -> x * 2)
let ef1 = Lambda("x", Binary("*", Variable("x"), Constant(2)))
evaluate Map.empty ef1 |> WriteLine

// Basic function calls (should return number)
//   (fun x -> x * 2) 21
let ef2 =
    Application(Lambda("x", Binary("*", Variable("x"), Constant(2))), Constant(21))

evaluate Map.empty ef2 |> WriteLine

// Wrong function call (the first argument is not a function)
//   21 (fun x -> x * 2)
let ef3 =
    Application(Constant(21), Lambda("x", Binary("*", Variable("x"), Constant(2))))

try
    evaluate Map.empty ef3 |> WriteLine
with
| ex -> WriteLine(ex.Message)


// Wrong binary operator (it is now possible to apply '+'
// to functions; this makes no sense and should fail!)
//   21 + (fun x -> x * 2)
let ef4 =
    Binary("+", Constant(21), Lambda("x", Binary("*", Variable("x"), Constant(2))))

try
    evaluate Map.empty ef4 |> WriteLine
with
| ex -> WriteLine(ex.Message)

// Simple let binding
//   let x = 2 in (20*x) + x
let el1 =
    Let("x", Constant(2), Binary("+", Variable("x"), Binary("*", Variable("x"), Constant(20))))

evaluate Map.empty el1 |> WriteLine

// Function calls with let binding
//   let f = fun x -> x*2 in (f 20) + (f 1)
//
// In F#, you would write this as follows
//   let f x = x*2
//   (f 20) + (f 1)
let el2 =
    Let(
        "f",
        Lambda("x", Binary("*", Variable("x"), Constant(2))),
        Binary("+", Application(Variable("f"), Constant(20)), Application(Variable("f"), Constant(1)))
    )

evaluate Map.empty el2 |> WriteLine

// Data types - simple tuple example (using the e#1, e#2 notation for field access)
//   (2*21, 123)#1
//   (2*21, 123)#2
let ed1 =
    TupleGet(true, Tuple(Binary("*", Constant(2), Constant(21)), Constant(123)))

evaluate Map.empty ed1 |> WriteLine

let ed2 =
    TupleGet(false, Tuple(Binary("*", Constant(2), Constant(21)), Constant(123)))

evaluate Map.empty ed2 |> WriteLine

// Data types - trying to get a first element of a value
// that is not a tuple (This makes no sense and should fail)
//   (42)#1
let ed3 = TupleGet(true, Constant(42))

try
    evaluate Map.empty ed3 |> WriteLine
with
| ex -> WriteLine(ex.Message)


// Data types - creating a union value
let ec1 = Case(true, Binary("*", Constant(21), Constant(2)))
evaluate Map.empty ec1 |> WriteLine

// Data types - working with union cases
//   match Case1(21) with Case1(x) -> x*2 | Case2(x) -> x*100
//   match Case2(21) with Case1(x) -> x*2 | Case2(x) -> x*100
let ec2 =
    Match(
        Case(true, Constant(21)),
        "x",
        Binary("*", Variable("x"), Constant(2)),
        Binary("*", Variable("x"), Constant(100))
    )

evaluate Map.empty ec2 |> WriteLine

let ec3 =
    Match(
        Case(false, Constant(21)),
        "x",
        Binary("*", Variable("x"), Constant(2)),
        Binary("*", Variable("x"), Constant(100))
    )

evaluate Map.empty ec3 |> WriteLine

// Recursion and conditionals - implementing factorial!
//   let rec factorial = fun x ->
//     if x then 1 else x*(factorial (-1 + x))
//   in factorial 5
let er =
    Recursive(
        "factorial",
        Lambda(
            "x",
            If(
                Variable("x"),
                Constant(1),
                Binary("*", Variable("x"), Application(Variable("factorial"), Binary("+", Constant(-1), Variable("x"))))
            )
        ),
        Application(Variable "factorial", Constant 6)
    )

evaluate Map.empty er |> WriteLine


// Ultimate functional programming - lists and List.map!
// We represent lists as cons cells using tuples, so [1,2,3]
//
// = Case(true, Tuple(Constant(1), Case(true, Tuple(Constant(2),
//     Case(true, Tuple(Constant(3), Case(false, Unit) ))))))

// Helper function to construct lists, so that we
// do not need to write them by hand!
let rec makeListExpr l =
    match l with
    | x :: xs -> Case(true, Tuple(x, makeListExpr xs))
    | [] -> Case(false, Unit)

let el = makeListExpr [ for i in 1..5 -> Constant i ]

// List.map function in TinyML:
//
//   let rec map = (fun f -> fun l ->
//     match l with
//     | Case1 t -> Case1(f x#1, (map f) x#2)
//     | Case2(Unit) -> Case2(Unit))
//   in map (fun y -> y * 10) l
//
let em =
    Recursive(
        "map",
        Lambda(
            "f",
            Lambda(
                "l",
                Match(
                    Variable("l"),
                    "x",
                    Case(
                        true,
                        Tuple(
                            Application(Variable "f", TupleGet(true, Variable "x")),
                            Application(Application(Variable "map", Variable "f"), TupleGet(false, Variable "x"))
                        )
                    ),
                    Case(false, Unit)
                )
            )
        ),
        Application(Application(Variable "map", Lambda("y", Binary("*", Variable "y", Constant 10))), el)
    )

evaluate Map.empty em |> WriteLine

// TODO: Can you implement 'List.filter' in TinyML too??
// The somewhat silly example removes 3 from the list.
// Add '%' binary operator and you can remove odd/even numbers!
//
//   let rec filter = (fun f -> fun l ->
//     match l with
//     | Case1 t ->
//          if f x#1 then Case1(x#1, (map f) x#2)
//          else (map f) x#2
//     | Case2(Unit) -> Case2(Unit))
//   in map (fun y -> y + (-2)) l
//

// WIP

// let ef =
//     Recursive(
//         "filter",
//         Lambda(
//             "f",
//             Lambda(
//                 "l",
//                 Match(
//                     Variable("l"),
//                     "x",
//                     Case(
//                         true,
//                         If(
//                         // Cond
//                         ,
//                         // Then
//                         Tuple(
//                             TupleGet(true, Variable "x"),
//                             Application(Application(Variable "map", Variable "f"), TupleGet(false, Variable "x"))
//                         ),
//                         // Else
//                         Application(Application(Variable "map", Variable "f"), TupleGet(false, Variable "x")))
//                     ),
//                     Case(false, Unit)
//                 )
//             )
//         ),
//         Application(Application(Variable "filter", Lambda("y", Binary("+", Variable "y", Constant -2))), el)
//     )

// evaluate Map.empty ef  |> WriteLine
