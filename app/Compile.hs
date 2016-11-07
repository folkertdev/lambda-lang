import System.Environment (getArgs)

import Types 
import Parser
import Evaluate

import Data.Map as Map
import qualified Data.List as List


main :: IO () 
main = do 
    arguments <- getArgs
    case arguments of 
        [] -> error "expecting a filename, got nothing"
        (filename:_) -> do
            contents <- readFile filename  
           
            contents
                |> lines
                |> List.map evaluator
                |> List.foldl (flip compose) ( return (), Map.empty ) 
                -- |> uncurry (\action bindings -> action >> print bindings)
                |> fst 

{-| Some combination of a monoid product and the state monad, but I don't currently see the 
 - abstraction -} 
compose :: (Map a b -> ( IO (), Map a b)) -> ( IO (), Map a b) -> ( IO (), Map a b)
compose f ( oldIO, oldMap) = 
    let (newIO, newMap) = f oldMap
    in
        ( oldIO >> newIO, newMap )  
   
evaluator :: String -> Map String (Expr Literal) -> ( IO (), Map String (Expr Literal))
evaluator input bindings = do 

    let expr' = Parser.parse input 
        
    case expr' of 
        Left err ->
            (print err, bindings)

        Right line -> 
            case line of 
                Declaration (Function name body) -> 
                    ( return (), Map.insert name body bindings)

                Evaluation expr -> 
                    ( print $ evaluate bindings expr, bindings)


