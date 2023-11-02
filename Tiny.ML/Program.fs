open Tiny.ML
open System


let e1 = Binary("+", Constant(2), Binary("*", Constant(10), Constant(4)))
let ctx1 = Map.empty
let r1 = Interpreter.evaluate ctx1 e1
Console.WriteLine(r1)

let e2= Binary("+", Constant(2), Binary("*", Variable("x"), Constant(4)))
let ctx2 = Map.ofList ["x", NumValue(10)]
let r2 = Interpreter.evaluate ctx2 e2
Console.WriteLine(r1)
