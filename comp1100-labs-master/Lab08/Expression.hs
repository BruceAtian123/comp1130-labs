module Expression where

data Expression a
  = Number a
  | Node (Expression a)
         Operand
         (Expression a)
  deriving (Show, Eq)

data Operand
  = Plus
  | Minus
  | Times
  | DividedBy
  | Power
  deriving (Show, Eq)

eval :: Floating a => Expression a -> Expression a
eval e = case e of
    Number x -> Number x
    Node a operation b -> Node (eval a) operation  (eval b)


operandToSign :: (Floating a) => Operand -> (a -> a-> a)
operandToSign op = case op of
    Plus -> (+)
    Minus -> (-)
    Times -> (*)
    DividedBy -> (/)
    Power -> (**)
