{-# LANGUAGE DeriveFunctor #-}
module Types (List, (|>), Literal(..), Expr(..), Function(..), Operator(..), ArithmeticOperator(..), LogicalOperator(..), Line(..), RuntimeError(..)) where  

import Data.Char (isAlpha, isDigit)
import qualified Data.List as List 
import Debug.Trace as Debug
-- import Control.Monad.State.Lazy as State
import Control.Monad.Trans.State.Lazy as State
import Control.Monad
import Control.Applicative


import Data.Map as Map


type List a = [a]

infixl 0 |>
(|>) :: a -> (a -> b) -> b
x |> f = f x


data Line = Evaluation (Expr Literal) | Declaration (Function Literal) deriving (Show)

data RuntimeError 
    = ParseError String
    | VariableNotFound String
    | TypeError String
    deriving (Show, Eq) 

data Literal 
    = IntNum Integer
    | Boolean Bool
    | Error String
    deriving (Show, Eq)

data Expr typ
    = Lit typ 
    | Var String
    -- | Lam typ (Expr typ)
    -- | App (Expr typ) (Arg typ)
    | Builtin Operator (Expr typ) (Expr typ) -- Operator => var
    | Lambda String (Expr typ)
    | Apply (Expr typ) (Expr typ)
    | IfThenElse [ (Expr typ, Expr typ) ] (Expr typ)
    -- | StringLiteral String
    deriving (Functor)

data Function typ = Function String (Expr typ) deriving (Show)




instance (Show typ) => Show (Expr typ) where 
    show expr = 
        case expr of 
            Lit v -> 
                show v
            Var v -> 
                v
            Builtin op a1 a2 -> 
                show a1 ++ " " ++ show op ++ " " ++ show a2

            Lambda argumentName body -> 
                "(\\" ++ argumentName ++ " -> " ++ show body ++ ")"


            Apply name argument -> 
                "(" ++ show name ++ " " ++ show argument ++ ")"

            IfThenElse thenBlocks elseBlock -> 
                let thens = 
                        List.map (\(cond, block) -> "if " ++ show cond ++ " then " ++ show block ++ " ") thenBlocks
                in 
                   concat thens ++ "else " ++ show elseBlock

data ArithmeticOperator = Plus | Minus | Multiply | Divide | Modulo 

data LogicalOperator = Equals 

data Operator
    = Arithmetic ArithmeticOperator
    | Logical LogicalOperator


instance Show ArithmeticOperator where 
    show Plus     = "+"
    show Minus    = "-"
    show Multiply = "*"
    show Divide   = "/"
    show Modulo   = "%"

instance Show LogicalOperator where
    show Equals = "=="

instance Show Operator where 
    show (Arithmetic op) = show op
    show (Logical op)    = show op


