namespace Tiny.ML

type Value = ValNum of int

type Expression =
    | Constant of int
    | Binary of string * Expression * Expression
    | Unary of string * Expression
    | Variable of string
    | If of Expression * Expression * Expression

type VariableContext = Map<string, Value>

module Interpreter =
    let rec evaluate (ctx: VariableContext) expr =
        match expr with
        | Constant c -> ValNum(c)
        | Binary(op, left, right) ->
            let leftVal = evaluate ctx left
            let rightVal = evaluate ctx right

            match leftVal, rightVal with
            | ValNum leftNum, ValNum rightNum ->
                match op with
                | "+" -> ValNum(leftNum + rightNum)
                | "*" -> ValNum(leftNum * rightNum)
                | _ -> failwith "Unsupported operation"
        | Variable(v) -> ctx.[v]
        | Unary(_, _) -> failwith "Not Implemented"
        | If(_, _, _) -> failwith "Not Implemented"
