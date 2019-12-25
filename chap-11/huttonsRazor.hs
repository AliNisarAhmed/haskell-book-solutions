module HuttonsRazor where

  data Expr
    = Lit Integer
    | Add Expr Expr

  eval :: Expr -> Integer
  eval (Lit x) = x
  eval (Add y z) = eval y + eval z

  a1 = Add (Lit 9001) (Lit 1)
  a2 = Add a1 (Lit 20001)
  a3 = Add (Lit 1) a2

  --   printExpr a3 -> SHOULD BE  "1 + 9001 + 1 + 20001"

  printExpr :: Expr -> String
  printExpr (Lit x) = show x 
  printExpr (Add y z) = printExpr y ++ " + " ++ printExpr z