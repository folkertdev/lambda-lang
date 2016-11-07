module Parser (parse) where 

import Types
import Data.List as List
import Control.Applicative ((<|>))
import Text.Read (readEither, readMaybe)
import Data.Char (isAlpha, isDigit)

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
                        parseB rightSide 
                            |> fmap (Declaration . Function name . constructLambda arguments . fst)


                    _ -> 
                        Left $ ParseError "Left-hand side, but no assignment"
        _ -> 
            case parseB tokens of 
                Left err -> Left err
                Right (result, []) -> Right (Evaluation result)
                Right (result, xs) -> Left . ParseError $ "could only parse " ++ show result ++ ", failed on tokens " ++ show xs ++ "."
    

  where constructLambda arguments body = List.foldr (Lambda . extractName) body arguments
        extractName (TokenVar n) = n
        extractName _ = error "extractName: not a Tokenvar" 


parseB :: List Token -> Either RuntimeError (Expr Literal, List Token)
parseB ts = do 
    (acc, rest) <- parseL ts
    parseB' acc rest


parseB' :: Expr Literal -> List Token -> Either RuntimeError (Expr Literal, List Token)
parseB' leftArgument tokens =
    case tokens of 
        (TokenOperator '=':ts) -> do
            (acc, rest) <- parseT ts 
            parseE' (Builtin (Logical Equals) leftArgument acc) rest

        _  -> 
            Right ( leftArgument, tokens ) 

{-| Parse a Lambda -} 
parseL :: List Token -> Either RuntimeError (Expr Literal, List Token)
parseL ts =
    case ts of 
        (TokenVar v:ts) -> uncurry parseT' (parseA (Var v) ts)
        _ -> parseE ts



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
    (acc, rest) <- parseF ts
    parseT' acc rest

parseT' :: Expr Literal -> List Token -> Either RuntimeError (Expr Literal, List Token)
parseT' leftArgument tokens = 
    case tokens of 
        (TokenOperator '*':ts) -> do
            ( acc, rest ) <- parseF ts 
            parseT' (Builtin (Arithmetic Multiply) leftArgument acc) rest

        (TokenOperator '/':ts) -> do
            ( acc, rest ) <- parseF ts 
            parseT' (Builtin (Arithmetic Divide) leftArgument acc) rest

        (TokenOperator '%':ts) -> do
            ( acc, rest ) <- parseF ts 
            parseT' (Builtin (Arithmetic Modulo) leftArgument acc) rest

        _ -> 
            Right (leftArgument, tokens)

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
            guardE "'if' not followed be 'then'" (head tokens1 == TokenKeyword Then)
            ( thenBranch, tokens2 ) <- parseE tokens1
            guardE "'then' not followed by 'else'" (head tokens2 == TokenKeyword Else)
            ( elseBranch, tokens3 ) <- parseE tokens2
            parseB' (IfThenElse [ (condition, thenBranch) ] elseBranch) tokens3

        (TokenKeyword Then:ts) -> parseF ts
        (TokenKeyword Else:ts) -> parseF ts 

        (TokenOpenParen:ts) -> do
            ( expr, tokens' ) <- parseB ts 
            case tokens' of 
                -- should this be parseB'? 
                (TokenClosedParen:rest) -> parseE' expr rest 

                _ -> Left $ ParseError "missing right parenthesis"

        _ -> Left . ParseError $ "unexpected token " ++ show tokens



{-| Parse as many separate (not combined with operator) as possible -} 
parseA :: Expr Literal -> List Token -> (Expr Literal, List Token)
parseA enclosing tokens = 
    case parseF tokens of 
        Left _ -> ( enclosing, tokens ) 

        Right ( argument, remaining ) -> 
            parseA (Apply enclosing argument) remaining

