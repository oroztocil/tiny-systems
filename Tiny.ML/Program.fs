open Tiny.ML
open System
open Tiny.ML.Interpreter


// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

// Basic arithmentic: (1*2) + (20*2)
let eba1 =
    Binary("+", Binary("*", Constant(1), Constant(2)), Binary("*", Constant(20), Constant(2)))

evaluate Map.empty eba1 |> Console.WriteLine

// Basic artihmetic with variables: x + (x*20)
let eba2 = Binary("+", Variable("x"), Binary("*", Variable("x"), Constant(20)))

let ctx1 = Map.ofList [ "x", ValNum 4 ]
evaluate ctx1 eba2 |> Console.WriteLine
let ctx2 = Map.ofList [ "x", ValNum 2 ]
evaluate ctx2 eba2 |> Console.WriteLine
