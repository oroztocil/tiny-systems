open Tiny.Basic
open System

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

let empty =
    { Program = []
      Variables = Map.empty
      Rng = new Random()
      ReturnStack = [] }

let helloOnce =
    [ Some 10, Print [Const(StringValue "HELLO WORLD\n")]
      Some 10, Print [Const(StringValue "HELLO NPRG077\n")]
      None, Run ]

let helloInf =
    [ Some 20, Goto 10
      Some 10, Print [Const(StringValue "HELLO WORLD\n")]
      Some 10, Print [Const(StringValue "HELLO NPRG077\n")]
      None, Run ]

let testVariables =
    [ Some 10, Assign("S", Const(StringValue "HELLO WORLD\n"))
      Some 20, Assign("I", Const(NumberValue 1))
      Some 30, Assign("B", Function("=", [ Variable("I"); Const(NumberValue 1) ]))
      Some 40, Print [Variable "S"]
      Some 50, Print [Variable "I"]
      Some 60, Print [Variable "B"]
      None, Run ]

// NOTE: Simpler test program without 'If" (just variables and '=' function)
runInputs empty testVariables |> ignore

let helloTen =
    [ Some 10, Assign("I", Const(NumberValue 10))
      Some 20, If(Function("=", [ Variable("I"); Const(NumberValue 0) ]), Goto(60))
      Some 30, Print [Const(StringValue "HELLO WORLD\n")]
      Some 40, Assign("I", Function("-", [ Variable("I"); Const(NumberValue 1) ]))
      Some 50, Goto 20
      Some 60, Print [Const(StringValue "")]
      None, Run ]

// NOTE: Prints hello world ten times using conditionals
runInputs empty helloTen |> ignore

// NOTE: Writing all the BASIC expressions is quite tedious, so this is a
// very basic (and terribly elegant) trick to make our task a bit easier.
// We define a couple of shortcuts and custom operators to construct expressions.
// With these, we can write e.g.:
//  'Function("RND", [Const(NumberValue 100)])' as '"RND" @ [num 100]' or
//  'Function("-", [Variable("I"); Const(NumberValue 1)])' as 'var "I" .- num 1'
let num v = Const(NumberValue v)
let str v = Const(StringValue v)
let var n = Variable n
let (.||) a b = Function("||", [ a; b ])
let (.<) a b = Function("<", [ a; b ])
let (.>) a b = Function(">", [ a; b ])
let (.-) a b = Function("-", [ a; b ])
let (.=) a b = Function("=", [ a; b ])
let (@) s args = Function(s, args)

// NOTE: Random stars generation. This has hard-coded max width and height (60x20)
// but you could use 'System.Console.WindowWidth'/'Height' here to make it nicer.
let stars =
    [ Some 10, Clear
      Some 20, Poke("RND" @ [ num 60 ], "RND" @ [ num 20 ], str "*")
      Some 30, Assign("I", num 100)
      Some 40, Poke("RND" @ [ num 60 ], "RND" @ [ num 20 ], str " ")
      Some 50, Assign("I", var "I" .- num 1)
      Some 60, If(var "I" .> num 1, Goto(40))
      Some 100, Goto(20)
      None, Run ]

// NOTE: Make the cursor invisible to get a nicer stars animation
// System.Console.CursorVisible <- false
// runInputs empty stars |> ignore

let nim =
  [ Some 10, Assign("M", num 20)
    Some 20, Print [ str "THERE ARE "; var "M"; str " MATCHES LEFT\n" ]
    Some 30, Print [ str "PLAYER 1: YOU CAN TAKE BETWEEN 1 AND ";
      "MIN" @ [num 5; var "M"]; str " MATCHES\n" ]
    Some 40, Print [ str "HOW MANY MATCHES DO YOU TAKE?\n" ]
    Some 50, Input("P")
    Some 60, If((var "P" .< num 1) .|| (var "P" .> num 5) .|| (var "P" .> var "M"), Goto 40)
    Some 70, Assign("M", var "M" .- var "P")
    Some 80, If(var "M" .= num 0, Goto 200)
    Some 90, Print [ str "THERE ARE "; var "M"; str " MATCHES LEFT\n" ]
    Some 100, Print [ str "PLAYER 2: YOU CAN TAKE BETWEEN 1 AND ";
      "MIN" @ [num 5; var "M"]; str " MATCHES\n" ]
    Some 110, Print [ str "HOW MANY MATCHES DO YOU TAKE?\n" ]
    Some 120, Input("P")
    Some 130, If((var "P" .< num 1) .|| (var "P" .> num 5) .|| (var "P" .> var "M"), Goto 110)
    Some 140, Assign("M", var "M" .- var "P")
    Some 150, If(var "M" .= num 0, Goto 220)
    Some 160, Goto 20
    Some 200, Print [str "PLAYER 1 WINS!"]
    Some 210, Stop
    Some 220, Print [str "PLAYER 2 WINS!"]
    Some 230, Stop
    None, Run
  ]

// runInputs empty nim |> ignore

let nimGoSub =
  [ Some 10, Assign("M", num 20)
    Some 20, Assign("U", num 1)
    Some 30, GoSub(100)
    Some 40, Assign("U", num 2)
    Some 50, GoSub(100)
    Some 60, Goto(20)
    Some 100, Print [ str "THERE ARE "; var "M"; str " MATCHES LEFT\n" ]
    Some 110, Print [ str "PLAYER "; var "U"; str ": YOU CAN TAKE BETWEEN 1 AND ";
      Function("MIN", [num 5; var "M"]); str " MATCHES\n" ]
    Some 120, Print [ str "HOW MANY MATCHES DO YOU TAKE?\n" ]
    Some 130, Input("P")
    Some 140, If((var "P" .< num 1) .|| (var "P" .> num 5) .|| (var "P" .> var "M"), Goto 120)
    Some 150, Assign("M", var "M" .- var "P")
    Some 160, If(var "M" .= num 0, Goto 200)
    Some 170, Return
    Some 200, Print [str "PLAYER "; var "U"; str " WINS!"]
    None, Run
  ]

runInputs empty nimGoSub |> ignore
