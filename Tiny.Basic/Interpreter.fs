module Tiny.Basic

open System

type Value =
    | StringValue of string
    | NumberValue of int
    | BoolValue of bool

type Expression =
    | Const of Value
    | Function of string * Expression list
    | Variable of string

type Command =
    | Run
    | Goto of int
    | Assign of string * Expression
    | If of Expression * Command
    | Clear
    | Poke of Expression * Expression * Expression
    | Print of Expression list
    | Input of string
    | Stop
    | GoSub of int
    | Return

type State =
    { Program: list<int * Command>
      Variables: Map<string, Value>
      Rng: Random
      ReturnStack: int list }

// ----------------------------------------------------------------------------
// Utilities
// ----------------------------------------------------------------------------

let printValue value =
    match value with
    | StringValue(v) -> Console.Write(v)
    | NumberValue(v) -> Console.Write(v)
    | BoolValue(v) -> Console.Write(v)

let getLine state line =
    match List.tryFind (fun (cmdLine, _) -> cmdLine = line) state.Program with
    | Some cmd -> cmd
    | None -> failwith $"Line {line} not defined"

let addLine state (line, cmd) =
    let filteredProgram =
        List.filter (fun (cmdLine, _) -> cmdLine <> line) state.Program

    let newProgram = (line, cmd) :: filteredProgram
    let sortedProgram = List.sortBy fst newProgram
    { state with Program = sortedProgram }

// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------

// let binaryOp<'T, 'R> (f: 'T -> 'T -> 'R) (args: 'T list) =
//   match args with
//   | [T a; T b] -> 'R(f a b)
//   | _ -> failwith "Expected two arguments for binary op"

let binaryNumOp f a b =
    match a, b with
    | NumberValue a, NumberValue b -> NumberValue(f a b)
    | _ -> failwith "Expected two numerical arguments"

let binaryRelOp f a b = BoolValue(f a b)

let binaryBoolOp f a b =
    match a, b with
    | BoolValue a, BoolValue b -> BoolValue(f a b)
    | _ -> failwith "Expected two bool arguments"


let rec evalExpression state expr =
    match expr with
    | Const(c) -> c
    | Function(name, body) ->
        match body with
        | [ a ] -> evalUnaryFunc state (name, a)
        | [ a; b ] -> evalBinaryFunc state (name, a, b)
        | _ -> failwith $"Unsupported function '{name}'"
    | Variable(name) -> state.Variables[name]

and evalUnaryFunc state (name, a) =
    let value = evalExpression state a

    match name, value with
    | "RND", NumberValue(n) -> NumberValue(state.Rng.Next(n))
    | _ -> failwith $"Unsupported function '{name}'"

and evalBinaryFunc state (name, left, right) =
    let leftValue = evalExpression state left
    let rightValue = evalExpression state right

    match name with
    | "=" -> binaryRelOp (=) leftValue rightValue
    | "<" -> binaryRelOp (<) leftValue rightValue
    | ">" -> binaryRelOp (>) leftValue rightValue
    | "||" -> binaryBoolOp (||) leftValue rightValue
    | "-" -> binaryNumOp (-) leftValue rightValue
    | "MIN" -> binaryNumOp min leftValue rightValue
    | _ -> failwith $"Unsupported function '{name}'"

let rec runCommand state (line, cmd) =
    match cmd with
    | Print(exprList) ->
        List.iter
            (fun expr ->
                let value = evalExpression state expr
                printValue value)
            exprList

        printValue (StringValue("\n"))
        runNextLine state line
    | Run ->
        let first = List.head state.Program
        runCommand state first
    | Goto(line) ->
        let nextCmd = getLine state line
        runCommand state nextCmd
    | Assign(name, expr) ->
        let value = evalExpression state expr

        let newState =
            { state with
                Variables = Map.add name value state.Variables }

        runNextLine newState line
    | If(cond, cmd) ->
        let condValue = evalExpression state cond

        match condValue with
        | BoolValue(true) -> runCommand state (line, cmd)
        | _ -> runNextLine state line
    | Clear ->
        Console.Clear()
        runNextLine state line
    | Poke(xExpr, yExpr, sExpr) ->
        let xVal = evalExpression state xExpr
        let yVal = evalExpression state yExpr
        let sVal = evalExpression state sExpr

        match xVal, yVal, sVal with
        | NumberValue(x), NumberValue(y), StringValue(s) ->
            Console.CursorLeft <- x
            Console.CursorTop <- y
            Console.Write(s)
            runNextLine state line
        | _ -> failwith "Invalid Poke arguments"
    | Input(varName) ->
        let inputValue = Console.ReadLine()
        let value =
            match System.Int32.TryParse inputValue with
            | true, n -> NumberValue(n)
            | _ -> StringValue(inputValue)
        let newState = { state with Variables = Map.add varName value state.Variables }
        runNextLine newState line
    | Stop -> state
    | GoSub(subLine) ->
        let nextCmd = getLine state subLine
        let newStack = line :: state.ReturnStack
        let newState = { state with ReturnStack = newStack }
        runCommand newState nextCmd
    | Return ->
        match state.ReturnStack with
        | retLine :: rest ->
            let newState = { state with ReturnStack = rest }
            runNextLine newState retLine
        | [] -> failwith "Not location to return to"

and runNextLine state line =
    match line with
    | -1 -> state
    | _ ->
        match List.tryFind (fun (nextLine, _) -> nextLine > line) state.Program with
        | Some cmd -> runCommand state cmd
        | None -> state

// ----------------------------------------------------------------------------
// Interactive program editing
// ----------------------------------------------------------------------------

let runInput state (line, cmd) =
    match line with
    | Some ln -> addLine state (ln, cmd)
    | None -> runCommand state (-1, cmd)

let runInputs state cmds =
    List.fold (fun currentState cmd -> runInput currentState cmd) state cmds
