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

let ctx1 = Map.ofList [ "x", ValNum 4 ]
evaluate ctx1 eba2 |> WriteLine
let ctx2 = Map.ofList [ "x", ValNum 2 ]
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
