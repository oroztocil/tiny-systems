namespace Tiny.ML

type Value =
    | ValNum of int
    | ValClosure of string * Expression * VariableContext
    | ValTuple of Value * Value

and Expression =
    | Constant of int
    | Binary of string * Expression * Expression
    | Unary of string * Expression
    | Variable of string
    | If of Expression * Expression * Expression
    | Application of Expression * Expression
    | Lambda of string * Expression
    | Let of string * Expression * Expression
    | Tuple of Expression * Expression
    | TupleGet of bool * Expression

and VariableContext = Map<string, Value>

module Interpreter =
    let rec evaluate (ctx: VariableContext) expr =
        match expr with
        | Constant c -> ValNum(c)
        | Binary (op, left, right) ->
            let leftVal = evaluate ctx left
            let rightVal = evaluate ctx right

            match leftVal, rightVal with
            | ValNum leftNum, ValNum rightNum ->
                match op with
                | "+" -> ValNum(leftNum + rightNum)
                | "*" -> ValNum(leftNum * rightNum)
                | _ -> failwith "Unsupported operation"
            | _ -> failwith "Unsupported binary operation arguments"
        | Variable (v) -> ctx.[v]
        | Unary (op, right) ->
            let rightVal = evaluate ctx right

            match rightVal with
            | ValNum rightNum ->
                match op with
                | "-" -> ValNum(-rightNum)
                | _ -> failwith "Unsupported operation"
            | _ -> failwith "Unsupported unary operation argument"
        | If (condExpr, thenExpr, elseExpr) ->
            let condVal = evaluate ctx condExpr

            match condVal with
            | ValNum (1) -> evaluate ctx thenExpr
            | _ -> evaluate ctx elseExpr
        | Lambda (varName, body) -> ValClosure(varName, body, ctx)

        | Application (leftExpr, rightExpr) ->
            let leftVal = evaluate ctx leftExpr
            let rightVal = evaluate ctx rightExpr

            match leftVal with
            | ValClosure (varName, body, closureCtx) -> evaluate (Map.add varName rightVal closureCtx) body
            | _ -> failwith "Left argument of application must be closure"
        | Let (varName, valueExpr, inExpr) ->
            let value = evaluate ctx valueExpr
            evaluate (Map.add varName value ctx) inExpr
