{-# LANGUAGE ViewPatterns, LambdaCase #-}
module Parser where

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Trans.Writer
import Data.List

-- TODO
-- * Control.Applicative.Alternative  ?
-- forM_

data ParserError
    = EOFError
    | NotANumberError
    | InvalidBooleanError
    | InvalidStringError
    | NoParseError
    | OtherError String

data LType
    = TypeString
    | TypeBool
    | TypeInt
    deriving (Show)

data LOp
    = OpAdd
    | EmptyOp
    deriving (Show)

data Expr
    = Val String
    | TypeExpr LType
    | LiteralExpr LType String
    | OperatorExpr LOp
    | OperatorAppl Expr LOp Expr
    | EmptyExpr
    | VariableDecl LType String
    deriving (Show)

instance Show ParserError where
  show EOFError = "Unexpected EOF"
  show NotANumberError = "Invalid number"
  show InvalidBooleanError = "Invalid boolean"
  show InvalidStringError = "Invalid string"
  show NoParseError = "Parse error"
  show (OtherError str) = str -- blurp, unsafe

-- Maps keywords to Expressions
type Keyword = (String, Expr)
keywords :: [Keyword]
keywords = [("True", LiteralExpr TypeBool "T"),
            ("False", LiteralExpr TypeBool "F"),
            ("Bool", TypeExpr TypeBool),
            ("Int", TypeExpr TypeInt),
            ("String", TypeExpr TypeString)]

-- Tracks coloumn
type Location = Int
type ParserState = (
                    Location,  -- Location tracking where we are
                    String     -- Data we have to parse
                   )

type Parser a = ExceptT ParserError (WriterT String (State ParserState)) a

mkParserState :: String -> ParserState
mkParserState str = (0, str)

runMonadicParser f st
  = (runState . runWriterT . runExceptT) f (mkParserState st)

runExpr :: Parser Expr -> String -> Expr
runExpr f st
  = case res of
      Left err -> EmptyExpr
      Right s  -> s
  where
   ((res, log), rem) = runMonadicParser f st

run :: Show a => Parser a -> String -> String
run f st
  = case res of
      Left err -> show err ++ " at column " ++ show (fst rem)
      Right s  -> show s
    where
      ((res, log), rem) = runMonadicParser f st

-- Handles parsing error by returning a value when it hits EOF
-- and passing through to the next call if it is a different error
handleParseError f e x
  = case x of
      EOFError -> return e
      _        -> f

parseKeyword :: Parser Expr
parseKeyword = do
  (loc, str) <- get
  foldl1 (<|>) (map parseKeyword' keywords)

(<|>) :: Parser a -> Parser a -> Parser a
p1 <|> p2 = p1 `catchError` (\_ -> p2)

-- TODO
-- (+++) :: Parser a -> Parser a -> Parser a
-- p1 +++ p2 = try p1 <|> p2

parseKeyword' :: Keyword -> Parser Expr
parseKeyword' (str, exp) = do
  (loc, st) <- get
  case st of
    (stripPrefix str -> Just r) -> do
        put (loc + length str, r)
        return exp
    []                          -> throwError EOFError
    _                           -> throwError NoParseError

skipWhitespace :: Parser Expr
skipWhitespace = flip safe (return EmptyExpr) $ do
  (loc, str) <- get
  case str of
    (c : cs) ->
      if (c == ' ') then do
        put (loc + 1, cs)
        skipWhitespace
      else
        return EmptyExpr
    _        -> return EmptyExpr

parseWord :: Parser Expr
parseWord = do
  (loc, str) <- get
  case str of
    (c : cs) -> do
      let (w, r) = span (\x -> x /= ' ') str
      put (loc + length w, r)
      return $ Val w
    _        -> throwError EOFError

safe :: Parser a -> Parser a -> Parser a
safe p k = do
  p `catchError` (const k)

parseVariableDecl :: Parser Expr
parseVariableDecl = do
  (loc, str) <- get
  tpe <- safe parseKeyword (return EmptyExpr)
  (TypeExpr dtype) <- case tpe of
             (TypeExpr ty) -> do
               return (TypeExpr ty)
             _             -> throwError $ OtherError "Type expected"
  skipWhitespace
  (Val name) <- parseWord
  case name of
    (_: _)     -> return (VariableDecl dtype name)
    _          -> throwError $ OtherError "Variable name expected"

-- Refactor?
parseOp :: Parser Expr
parseOp = do
  skipWhitespace
  p1 <- parseWord `catchError` (\case {
          EOFError -> throwError $ OtherError "Argument expected"
        })
  skipWhitespace
  (loc, str) <- get
  (OperatorExpr op) <- case str of
      (c : cs) -> do
        if (c == '+') then do
          put (loc + 1, cs)
          return $ OperatorExpr OpAdd
        else throwError $ OtherError "Invalid operator"
      _        -> throwError EOFError
  skipWhitespace
  p2 <- parseWord `catchError` (\_ ->
        throwError $ OtherError "Partial application unsupported")
  return (OperatorAppl p1 op p2)

parseString :: Parser Expr
parseString = do
  (loc, str) <- get
  case str of
    (c : cs) ->
      if (c == '"' && isSuffixOf "\"" cs) then do
        put (loc + (length str), [])
        return $ Val (init cs)
      else throwError InvalidStringError
    _       -> throwError EOFError

parseNumbers :: Parser Expr
parseNumbers = do
  (loc, str) <- get
  case str of
    (c : cs) ->
      if (c >= '0' && c <= '9') then do
        put (loc + 1, cs)
        (Val x) <- parseNumbers `catchError` (handleParseError parseNumbers (Val []))
        return (Val (c : x))
      else
        throwError NotANumberError
    _        -> throwError EOFError

literalize :: Expr -> Expr
literalize (Val x)
  = LiteralExpr TypeInt s
  where
    (Val s) = runExpr parseNumbers x
literalize _ = EmptyExpr
