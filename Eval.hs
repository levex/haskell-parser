module Eval where

import Parser

-- data Expr
--     = Val String
--     | TypeExpr LType
--     | LiteralExpr LType String
--     | OperatorExpr LOp
--     | OperatorAppl Expr LOp Expr
--     | EmptyExpr
--     | VariableDecl LType String
--     deriving (Show)

applyOp :: LOp -> String -> String -> String
applyOp OpAdd s1 s2
  = show ((read s1 :: Int) + (read s2 :: Int))
applyOp OpMul s1 s2
  = show ((read s1 :: Int) * (read s2 :: Int))

eval :: Expr -> String
eval (Val x)
  = x
eval (LiteralExpr _ s)
  = s
eval (OperatorAppl e1 op e2)
  = applyOp op (eval e1) (eval e2)
