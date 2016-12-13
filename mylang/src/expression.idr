module expr

import nat
import variable
import state
import uint8

-- SYNTAX for Nat expressions

||| NatExpr is the type of expressions that evaluate to
||| natural numbers (Nat). We'll call them Nat Expressions.
public export
data NatExpr =
  NatLitExpr Nat |
  NatVarExpr (Variable Nat) |
  NatPlusExpr NatExpr NatExpr |
  NatMinusExpr NatExpr NatExpr |
  NatMultExpr NatExpr NatExpr

-- SEMANTICS for Nat Expressions

||| natExprEval evaluates Nat Expressions to yield Nats.
export
natExprEval: NatExpr -> State -> Nat
natExprEval (NatLitExpr n) st = n
natExprEval (NatVarExpr v) st = (getNatState st) v
natExprEval (NatPlusExpr e1 e2) st =
  nat_plus   -- the result is the sum of
    (natExprEval e1 st) -- the value of expr1
    (natExprEval e2 st) -- and expr2
natExprEval (NatMinusExpr e1 e2) st =
    nat_minus
        (natExprEval e1 st)
        (natExprEval e2 st)
natExprEval (NatMultExpr e1 e2) st =
    nat_mult
        (natExprEval e1 st)
        (natExprEval e2 st)

-- Syntax for Bool expressions


||| BoolExpr is the type of expressions that evaluate to
||| Boolean values (Bool). We'll call them Bool Expressions.
public export
data BoolExpr =
  BoolLitExpr Bool |
  BoolVarExpr (Variable Bool) |
  BoolAndExpr BoolExpr BoolExpr |
  BoolEqExpr NatExpr NatExpr |
  BoolNeqExpr NatExpr NatExpr |
  BoolEqExpr_uint8


-- Semantics for Bool expressions

||| natExprEval evaluates Nat Expressions to yield Nats.
export
boolExprEval: BoolExpr -> State -> Bool
boolExprEval (BoolLitExpr b) st = b
boolExprEval (BoolVarExpr v) st = (getBoolState st) v
boolExprEval (BoolAndExpr e1 e2) st =
    bool_and (boolExprEval e1 st) (boolExprEval e2 st)
boolExprEval (BoolEqExpr n1 n2) st =
    eq (natExprEval n1 st) (natExprEval n2 st)
boolExprEval (BoolNeqExpr n1 n2) st =
    neq (natExprEval n1 st) (natExprEval n2 st)

||| Definition for the UInt8 expression data type
public export
data UInt8Expr =
    UInt8LitExpr (UInt8) |
    UInt8VarExpr (Variable UInt8) |
    UInt8PlusExpr (UInt8Expr) (UInt8Expr) |
    UInt8IncExpr (UInt8Expr)

||| Definition for each constructor of a UInt8 expression
export
uint8ExprEval: UInt8Expr -> State -> UInt8
uint8ExprEval (UInt8LitExpr n) st = n
uint8ExprEval (UInt8VarExpr v) st = (getUInt8State st) v
uint8ExprEval (UInt8PlusExpr n1 n2) st =
    add_UInt8 (uint8ExprEval n1 st) (uint8ExprEval n2 st)
uint8ExprEval (UInt8IncExpr n) st = inc_UInt8 (uint8ExprEval n st)
