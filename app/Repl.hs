import Types 
import Parser
import Evaluate

import Data.Map as Map
import Control.Monad (unless)


main :: IO () 
main = do 
    putStrLn "The LambdaLang REPL (type \"quit\" to quit)"
    repl Map.empty

{-| Read Eval Print Loop
-} 
repl :: Map String (Expr Literal) -> IO ()  
repl bindings = do 
    putStr "> "
    input <- getLine 

    let expr' = Parser.parse input 
        
    unless (input == "quit") $ 
        case expr' of 
            Left err -> do 
                print err
                repl bindings 

            Right line -> 
                case line of 
                    Declaration (Function name body) -> 
                        repl (Map.insert name body bindings)

                    Evaluation expr -> do 
                        print $ evaluate bindings expr 
                        repl bindings



