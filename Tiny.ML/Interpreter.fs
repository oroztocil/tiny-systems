namespace Tiny.ML

type Value =
    | ValNum of int
    | ValClosure of string * Expression * VariableContext
    | ValTuple of Value * Value
    | ValCase of bool * Value
    | ValUnit

and Expression =
    | Constant of int
    | Binary of string * Expression * Expression
    | Unary of string * Expression
    | Variable of string
    | If of Expression * Expression * Expression
    | Application of Expression * Expression
    | Lambda of string * Expression
    | Let of string * Expression * Expression
    | Recursive of string * Expression * Expression
    | Tuple of Expression * Expression
    | TupleGet of bool * Expression
    | Case of bool * Expression
    | Match of Expression * string * Expression * Expression
    | Unit

and VariableContext = Map<string, Lazy<Value>>

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

        | Variable (varName) ->
            match ctx.TryFind varName with
            | Some result -> result.Value
            | _ -> failwith $"Accessing unbound variable '{varName}'"

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
            | ValClosure (varName, body, closureCtx) -> evaluate (Map.add varName (lazy rightVal) closureCtx) body
            | _ -> failwith "Left argument of application must be closure"

        | Let (varName, valueExpr, inExpr) ->
            let value = lazy (evaluate ctx valueExpr)
            evaluate (Map.add varName value ctx) inExpr

        | Recursive (funcName, lambdaExpr, inExpr) ->
            match lambdaExpr with
            | Lambda(varName, body) ->
                let rec lambdaVal = ValClosure(varName, body, Map.add funcName (lazy lambdaVal) ctx)
                evaluate (Map.add funcName (lazy lambdaVal) ctx) inExpr
            | _ -> failwith "Second argument of Recursive must be a lambda"

        | Tuple (firstExpr, secondExpr) ->
            let firstVal = evaluate ctx firstExpr
            let secondVal = evaluate ctx secondExpr
            ValTuple(firstVal, secondVal)

        | TupleGet (wantFirst, expr) ->
            let value = evaluate ctx expr

            match wantFirst, value with
            | true, ValTuple (first, _) -> first
            | false, ValTuple (_, second) -> second
            | _ -> failwith "TupleGet argument is not a tuple"

        | Case (isFirst, expr) ->
            let value = evaluate ctx expr
            ValCase(isFirst, value)

        | Match (matchedExpr, varName, firstExpr, secondExpr) ->
            let matchedVal = evaluate ctx matchedExpr

            match matchedVal with
            | ValCase (true, value) -> evaluate (Map.add varName (lazy value) ctx) firstExpr
            | ValCase (false, value) -> evaluate (Map.add varName (lazy value) ctx) secondExpr
            | _ -> failwith "Matched value is not a case value"
        | Unit -> ValUnit
