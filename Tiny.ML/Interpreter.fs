namespace Tiny.ML

type Value = NumValue of int

type Expression =
  | Constant of int
  | Binary of string * Expression * Expression
  | Variable of string

type VariableContext = Map<string, Value>

module Interpreter =
  let rec evaluate (ctx: VariableContext) expr =
    match expr with
    | Constant c -> NumValue(c)
    | Binary(op, left, right) ->
      let leftVal = evaluate ctx left
      let rightVal = evaluate ctx right
      match leftVal, rightVal with
      | NumValue leftNum, NumValue rightNum ->
        match op with
        | "+" -> NumValue(leftNum + rightNum)
        | "*" -> NumValue(leftNum * rightNum)
        | _ -> failwith "Unsupported operation"
    | Variable(v) -> ctx.[v]
