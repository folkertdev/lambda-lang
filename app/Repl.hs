import Types 

{-| Read Eval Print Loop
-} 
main :: Map String (Expr Literal) -> IO ()  
main bindings = do 
    putStr "> "
    input <- getLine 

    let expr' = toExpr input 
        
    unless (input == "quit") $ 
        case expr' of 
            Left err -> do 
                print err
                main bindings 

            Right line -> 
                case line of 
                    Declaration (Function name body) -> 
                        main (Map.insert name body bindings)

                    Evaluation expr -> do 
                        print $ flip State.evalState bindings $ evaluate expr 
                        main bindings



