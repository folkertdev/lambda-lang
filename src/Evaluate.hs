module Evaluate where 

import Types

import Data.Map as Map
import Data.List as List

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


apply :: Map String (Expr a) -> Expr a -> Expr a
apply bindings expr = 
    case expr of 
        Apply (Var f) x ->
            case Map.lookup f bindings of 
                Nothing -> 
                    error $ "function '" ++ show f ++ "' not defined"

                Just (Lambda argumentName body) ->
                    substitute argumentName x body

        Apply (Lambda varName body) argument ->
            apply bindings (substitute varName argument body)

        Apply (Apply f x) y -> 
            let 
                function = apply bindings (Apply f x) 
            in
                apply bindings (Apply function y)

        _ -> 
            expr 


evaluate :: Map String (Expr Literal) -> Expr Literal -> Expr Literal
evaluate bindings expr =
    case expr of 
        Lit i -> return (Lit i)

        Var v ->
            case Map.lookup v bindings of  
                Nothing -> error $ "NameError: variable '" ++ v ++ "' not defined"
                Just expr -> 
                    evaluate bindings expr

        Apply f x ->
            let 
                substituted = apply bindings (Apply f x) 
            in 
                evaluate bindings substituted 

        Builtin (Arithmetic operator) arg1 arg2 -> 
            let function = 
                    case operator of 
                        Plus     -> (+)
                        Minus    -> (-)
                        Multiply -> (*)
                        Divide   -> div
                        Modulo   -> mod

                arguments =  (evaluate bindings arg1, evaluate bindings arg2)
            in
                case arguments of 
                    (Lit (IntNum x), Lit (IntNum y)) -> 
                        Lit . IntNum $ function x y

                    _ -> 
                        error $ "TypeError: Expected (IntNum, IntNum), got " ++ show arguments

        Builtin (Logical operator) arg1 arg2 -> 
            let function = 
                    case operator of 
                        Equals -> (==)

                arguments = ( evaluate bindings arg1, evaluate bindings arg2)
            in 
                case arguments of 
                    (Lit x, Lit y) -> 
                        Lit . Boolean $ function x y

                    _ -> 
                        error $ "TypeError: Expected (IntNum, IntNum), got " ++ show arguments

        IfThenElse thenBlocks elseBlock -> 
            case thenBlocks of 
                [] -> evaluate bindings elseBlock
                ((cond, block):ts) ->
                    let 
                        condition = evaluate bindings cond
                    in 
                        case condition of 
                            Lit (Boolean True) -> 
                                evaluate bindings block

                            Lit (Boolean False) -> 
                                evaluate bindings (IfThenElse ts elseBlock)

                            _ -> 
                                error $ "TypeError: Expected Boolean, got " ++ show condition


                    
                    

        _ -> 
            -- return . Lit $ Error ("cannot evaluate"++ show expr)
            error ("ValueError: cannot evaluate"++ show expr)
     

