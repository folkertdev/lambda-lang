module Evaluate (evaluate) where 

import Types

import Data.Map as Map
import Data.List as List


{-| Apply a function over all variable names, used for substitution -} 
mapVars :: (String -> Expr a) -> Expr a -> Expr a
mapVars f expr = 
    case expr of 
        Lit _ -> 
            expr

        Var name -> 
            f name 

        Builtin op a1 a2 -> 
            Builtin op (mapVars f a1) (mapVars f a2)

        Apply function argument -> 
            Apply (mapVars f function) (mapVars f argument)

        Lambda argumentName body -> 
            Lambda argumentName (mapVars f body) 

        IfThenElse thenBlocks elseBlock -> 
            IfThenElse (List.map (mapBoth (mapVars f)) thenBlocks) (mapVars f elseBlock)


substitute :: String -> Expr a -> Expr a -> Expr a 
substitute name replacement =  
    mapVars (\var -> if var == name then replacement else Var var) 


apply :: Map String (Expr a) -> Expr a -> Either RuntimeError (Expr a)
apply bindings expr = 
    case expr of 
        Apply (Var f) x ->
            case Map.lookup f bindings of 
                Nothing -> 
                    Left . VariableNotFound $ "Function '" ++ show f ++ "' is not defined"

                Just (Lambda argumentName body) ->
                    Right $ substitute argumentName x body

        Apply (Lambda varName body) argument ->
            apply bindings (substitute varName argument body)

        Apply (Apply f x) y -> do
            function <- apply bindings (Apply f x) 
            apply bindings (Apply function y)

        _ -> 
            Right expr 


evaluate :: Map String (Expr Literal) -> Expr Literal -> Either RuntimeError (Expr Literal)
evaluate bindings expr =
    case expr of 
        Lit i -> Right (Lit i)

        Var v ->
            case Map.lookup v bindings of  
                Nothing -> 
                    Left . VariableNotFound $ "Variable '" ++ show v ++ "' is not defined." 
                Just expr -> 
                    evaluate bindings expr

        Apply f x -> do
            substituted <- apply bindings (Apply f x) 
            evaluate bindings substituted 

        Builtin (Arithmetic operator) arg1 arg2 -> 
            let function = 
                    case operator of 
                        Plus     -> (+)
                        Minus    -> (-)
                        Multiply -> (*)
                        Divide   -> div
                        Modulo   -> mod

            in do
                leftArg <- evaluate bindings arg1
                rightArg <- evaluate bindings arg2
                case (leftArg, rightArg) of 
                    (Lit (IntNum x), Lit (IntNum y)) -> 
                        Right . Lit . IntNum $ function x y

                    (Lit (IntNum _), second) -> 
                        Left . TypeError $ "Expected IntNum, got " ++ show second ++ " in the second argument of '" ++ show operator 

                    (first, _) -> 
                        Left . TypeError $ "Expected IntNum, got " ++ show first  ++ " in the second argument of '" ++ show operator 

        Builtin (Logical operator) arg1 arg2 -> 
            let function :: Eq a => a -> a -> Bool 
                function = 
                    case operator of 
                        Equals -> (==)
            in do
                leftArg <- evaluate bindings arg1
                rightArg <- evaluate bindings arg2
                case (leftArg, rightArg) of 
                    (Lit (IntNum x), Lit (IntNum y)) -> 
                        Right . Lit . Boolean $ function x y

                    (Lit (Boolean x), Lit (Boolean y)) -> 
                        Right . Lit . Boolean $ function x y

                    _ -> 
                        Left . TypeError $ "The arguments for " ++ show operator ++ " are of different types."

        IfThenElse thenBlocks elseBlock -> 
            case thenBlocks of 
                [] -> evaluate bindings elseBlock
                ((cond, block):ts) -> do 
                    condition <- evaluate bindings cond
                    case condition of 
                        Lit (Boolean True) -> 
                            evaluate bindings block

                        Lit (Boolean False) -> 
                            evaluate bindings (IfThenElse ts elseBlock)

                        _ -> 
                            Left . TypeError $ "if-then-else: Expected Boolean, got " ++ show condition 


                    
                    

        _ -> 
            Left . TypeError $ "cannot evaluate" ++ show expr
     

mapBoth :: (a -> b) -> (a, a) -> (b, b)
mapBoth f (a, b) = (f a, f b) 
