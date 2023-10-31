
import Data.List (takeWhile,dropWhile)


{- AST -}

data StringExpression a = SVar a
                        | Literal String
                        | Concat (StringExpression a)
                                 (StringExpression a)
                        | UserString
  deriving (Eq, Show)

data BoolExpression a = T | F
                      | StrEq (StringExpression a)
                              (StringExpression a)
  deriving (Eq, Show)

data Statement a = Print (StringExpression a)
                 | If (BoolExpression a) (Statement a)
                 | StrAssign a (StringExpression a)
  deriving (Eq, Show)



evalString :: (a -> String) -> StringExpression a -> IO String
evalString f (SVar v) = pure (f v)
evalString f (Literal s) = pure s
evalString f (Concat lhs rhs)
 = do
    lval <- evalString f lhs
    rval <- evalString f rhs
    pure (lval ++ rval)
evalString f UserString = getLine

evalBool :: (a -> String) -> BoolExpression a -> IO Bool
evalBool f T = pure True
evalBool f F = pure False
evalBool f (StrEq lhs rhs)
 = do
    lval <- evalString f lhs
    rval <- evalString f rhs
    pure (lval == rval)

runStatement :: (Eq a)
             => (a -> String) -> Statement a
             -> IO (a -> String)
runStatement f (Print s) = do
   sval <- evalString f s
   putStrLn sval
   pure f
runStatement f (If b stm) = do
   bval <- evalBool f b
   if bval
   then runStatement f stm
   else pure f
runStatement f (StrAssign var expr)
 = do
    val <- evalString f expr
    pure (\x -> if x == var then val else f x)

type Parser e = String -> Maybe (String, e)


keyword :: String -> Parser ()
keyword [] s = Just (s , ())
keyword (k:ks) [] = Nothing
keyword (k:ks) (s:ss) = if k == s
                        then keyword ks ss
                        else Nothing

(<|>) :: Parser a -> Parser a -> Parser a
p <|> q = \s -> case p s of
                 Nothing -> q s
                 (Just v) -> Just v


--pUserString, pSVar, pConcat,pLiteral :: Parser (StringExpression String)

pUserString s = do
   (s',_) <- keyword "read" s
   Just (s', UserString)

alpha x = elem x ['a'..'z']

pSVar s = let v = takeWhile alpha s
              s' = dropWhile alpha s
          in if v == "" then Nothing else Just (s',SVar v)

pConcat s = do
   (s,lhs) <- (pUserString <|> pSVar <|> pLiteral <|>  parseString) s
   (s,_) <- keyword "+" s
   (s,rhs) <- parseString s
   Just (s, Concat lhs rhs)

allowed x = elem x $ ['a'..'z'] ++ ['A'..'Z'] ++ " !?.,"

pChars s = Just (dropWhile allowed s, takeWhile allowed s)

pLiteral s = do
   (s,_) <- keyword "\"" s
   (s,lit) <- pChars s
   (s,_) <- keyword "\"" s
   Just (s, Literal lit)

parseString :: Parser (StringExpression String)
parseString = pConcat <|> pUserString <|> pSVar <|> pLiteral

pT s = do
  (s,_) <- keyword "true" s
  Just (s, T)

pF s = do
  (s,_) <- keyword "false" s
  Just (s, F)

pStrEq s = do
  (s,lhs) <- parseString s
  (s,_) <- keyword "=" s
  (s,rhs) <- parseString s
  Just (s, StrEq lhs rhs)

parseBool :: Parser (BoolExpression String)
parseBool = pT <|> pF <|> pStrEq


pSpace (' ':s) = Just (s,())
pSpace x = Nothing

pPrint s = do
   (s,_) <- keyword "print" s
   (s,_) <- pSpace s
   (s,expr) <- parseString s
   Just (s, Print expr)

pIf s = do
   (s,_) <- keyword "if" s
   (s,_) <- pSpace s
   (s,b) <- parseBool s
   (s,_) <- pSpace s
   (s,_) <- keyword "then" s
   (s,_) <- pSpace s
   (s,stm) <- parseStatement s
   Just (s, If b stm)
   
varname s = let v = takeWhile alpha s
                s' = dropWhile alpha s
          in if v == "" then Nothing else Just (s', v)
pAssign s = do
   (s,x) <- varname s
   (s,_) <- keyword "=" s
   (s,expr) <- parseString s
   Just (s, StrAssign x expr)

parseStatement :: Parser (Statement String)
parseStatement = pPrint <|> pIf <|> pAssign


repl f = do
   line <- getLine
   case parseStatement line of
     Nothing -> putStrLn "Syntax error!"
     (Just ("",stm)) -> do
                f' <- runStatement f stm
                repl f'

