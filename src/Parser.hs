module Parser (parse) where 

import Types
import Data.List as List
import Control.Applicative ((<|>))
import Text.Read (readEither, readMaybe)
import Data.Char (isAlpha, isDigit)

import Debug.Trace as Debug

startsWith :: Eq a => List a -> List a -> Bool
startsWith = List.isPrefixOf



{-| A custom Token type 

This make the rest of the process more convenient
and omits duplicate type checking. 
-} 
data Token 
    = TokenVal Integer
    | TokenBool Bool
    | TokenVar String
    | TokenAssignment
    | TokenKeyword Keyword
    | TokenOpenParen
    | TokenClosedParen
    | TokenOperator Char
    deriving (Show, Eq)


data Keyword 
    = Let
    | If
    | Then
    | Else
    deriving (Show, Eq)


mapFst f (a, b) = (f a, b)
mapSnd f (a, b) = (a, f b)
mapBoth f (a, b) = (f a, f b)

tokenizeLine [] = Right []
tokenizeLine chars 
    | startsWith "let " chars = (:) (TokenKeyword Let) <$> tokenize (drop 4 chars)
    | otherwise = tokenize chars 


tokenize :: String -> Either RuntimeError (List Token)
tokenize [] = Right [] 
tokenize chars@(c:cs)
    | c `elem` "+-*/%" = (:) (TokenOperator c) <$> tokenize cs
    | c == '('  = (:) TokenOpenParen   <$> tokenize cs
    | c == ')'  = (:) TokenClosedParen <$> tokenize cs
    | c == ' '  = tokenize cs
    | startsWith "==" chars  = (:) (TokenOperator '=') <$> tokenize (drop 2 chars)
    | c == '='  = (:) TokenAssignment <$> tokenize cs 
    | startsWith "if"   chars = (:) (TokenKeyword If)   <$> tokenize (drop 2 chars)
    | startsWith "then" chars = (:) (TokenKeyword Then) <$> tokenize (drop 4 chars)
    | startsWith "else" chars = (:) (TokenKeyword Else) <$> tokenize (drop 4 chars)
    | startsWith "True"  chars = (:) (TokenBool True)  <$> tokenize (drop 4 chars)
    | startsWith "False" chars = (:) (TokenBool False) <$> tokenize (drop 5 chars)
    | isDigit c = 
        span isDigit chars
            |> mapFst (TokenVal . (read :: String -> Integer))
            |> mapSnd tokenize
            |> (\(x,xs) -> (:) x <$> xs)
    | isAlpha c = 
        span isAlpha chars
            |> mapFst TokenVar
            |> mapSnd tokenize
            |> (\(x,xs) -> (:) x <$> xs)
    | otherwise = Left . ParseError $ "unexpected input token: " ++ show chars


parse :: String -> Either RuntimeError Line 
parse str = do 
    tokens <- tokenizeLine str
    parseLine tokens 

        


parseLine :: List Token -> Either RuntimeError Line
parseLine tokens = 
    case tokens of 
        (TokenKeyword Let : TokenVar name: rest) -> 
            let 
                isVar (TokenVar _) = True
                isVar _ = False


                ( arguments, remaining ) = span isVar rest 

            in 
                case remaining of 
                    (TokenAssignment:rightSide) ->
                        topLevelParser rightSide 
                            |> fmap (Declaration . Function name . constructLambda arguments . fst)


                    _ -> 
                        Left $ ParseError "Left-hand side, but no assignment"
        _ -> 
            case topLevelParser tokens of 
                Left err -> Left err
                Right (result, []) -> Right (Evaluation result)
                Right (result, xs) -> Left . ParseError $ "could only parse " ++ show result ++ ", failed on tokens " ++ show xs ++ "."
    

  where constructLambda arguments body = List.foldr (Lambda . extractName) body arguments
        extractName (TokenVar n) = n
        extractName _ = error "extractName: not a Tokenvar" 

topLevelParser = parseB


parseB :: List Token -> Either RuntimeError (Expr Literal, List Token)
parseB ts = do 
    (acc, rest) <- parseE ts
    parseB' acc rest


parseB' :: Expr Literal -> List Token -> Either RuntimeError (Expr Literal, List Token)
parseB' leftArgument tokens =
    case tokens of 
        (TokenOperator '=':ts) -> do
            (acc, rest) <- parseE ts 
            parseE' (Builtin (Logical Equals) leftArgument acc) rest

        _  -> 
            Right ( leftArgument, tokens ) 


{-| Parse + and - -} 
parseE :: List Token -> Either RuntimeError (Expr Literal, List Token) 
parseE ts = do 
    (acc, rest) <- parseT ts 
    parseE' acc rest


parseE' :: Expr Literal -> List Token -> Either RuntimeError (Expr Literal, List Token)
parseE' leftArgument tokens =
    case tokens of 
        (TokenOperator '+':ts) -> do
            (acc, rest) <- parseT ts 
            parseE' (Builtin (Arithmetic Plus) leftArgument acc) rest

        (TokenOperator '-':ts) -> do 
            (acc, rest) <- parseT ts 
            parseE' (Builtin (Arithmetic Minus) leftArgument  acc) rest

        _  -> 
            Right ( leftArgument, tokens ) 


{-| Parse *, / and % -} 
parseT :: List Token -> Either RuntimeError (Expr Literal, List Token) 
parseT ts = do 
    (acc, rest) <- parseL ts
    parseT' acc rest

parseT' :: Expr Literal -> List Token -> Either RuntimeError (Expr Literal, List Token)
parseT' leftArgument tokens = 
    case tokens of 
        (TokenOperator '*':ts) -> do
            ( acc, rest ) <- parseL ts 
            parseT' (Builtin (Arithmetic Multiply) leftArgument acc) rest

        (TokenOperator '/':ts) -> do
            ( acc, rest ) <- parseL ts 
            parseT' (Builtin (Arithmetic Divide) leftArgument acc) rest

        (TokenOperator '%':ts) -> do
            ( acc, rest ) <- parseL ts 
            parseT' (Builtin (Arithmetic Modulo) leftArgument acc) rest

        _ -> 
            Right (leftArgument, tokens)



{-| Parse a Lambda -} 
parseL :: List Token -> Either RuntimeError (Expr Literal, List Token)
parseL ts =
    case ts of 
        (TokenVar v:ts) -> uncurry parseB' (parseA (Var v) ts)
        _ -> parseF ts

parseLiteral str = 
    (Boolean <$> readMaybe str) <|> (IntNum <$> readMaybe str)



guardE :: String -> Bool -> Either RuntimeError ()
guardE err False = Left (ParseError err)
guardE _   True  = Right ()

parseF :: List Token -> Either RuntimeError (Expr Literal, List Token)
parseF tokens = 
    case tokens of 
        (TokenVal  n:ts) -> Right (Lit (IntNum  n), ts)
        (TokenBool b:ts) -> Right (Lit (Boolean b), ts)
        (TokenVar v:ts) -> 
            Right ( Var v, ts)

        (TokenKeyword If:ts) -> do 
            ( condition, tokens1 ) <- parseB ts
            case tokens1 of 
                (TokenKeyword Then:rest1) -> do 
                    ( thenBranch, rest2 ) <- parseB rest1
                    case rest2 of 
                        (TokenKeyword Else:rest3) -> do 
                            ( elseBranch, rest4 ) <- parseB rest3
                            parseB' (IfThenElse [ (condition, thenBranch) ] elseBranch) rest4
                        _ -> 
                            Left $ ParseError "If-then-else with missing else-clause"
                _ -> 
                    Left $ ParseError "If-then-else with missing then-clause"
                        
                    
                
            {-
            guardE "'if' not followed be 'then'" (head tokens1 == TokenKeyword Then)
            ( thenBranch, tokens2 ) <- parseB tokens1
            guardE "'then' not followed by 'else'" (head tokens2 == TokenKeyword Else)
            ( elseBranch, tokens3 ) <- parseB tokens2
            parseB' (IfThenElse [ (condition, thenBranch) ] elseBranch) tokens3
            -} 

        (TokenOpenParen:ts) -> do
            ( expr, tokens' ) <- parseB ts 
            case tokens' of 
                (TokenClosedParen:rest) -> parseB' expr rest 

                _ -> Left $ ParseError "missing right parenthesis"

        _ -> Left . ParseError $ "unexpected token " ++ show tokens



{-| Parse as many separate (not combined with operator) as possible -} 
parseA :: Expr Literal -> List Token -> (Expr Literal, List Token)
parseA enclosing tokens = 
    case parseF tokens of 
        Left _ -> ( enclosing, tokens ) 
        
        Right ( argument, [] ) -> 
            ( Apply enclosing argument, [] )  

        Right ( argument, remaining ) -> 
            parseA (Apply enclosing argument) remaining

