{-# LANGUAGE ViewPatterns, LambdaCase #-}
module Parser where

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer
import Data.List
import Data.List.Split

-- TODO
-- * Control.Applicative.Alternative  ?
-- forM_

data ParserError
    = EOFError
    | NotANumberError
    | InvalidBooleanError
    | InvalidStringError
    | NoParseError
    | VariableExistsError
    | UnassignedVariableError
    | OtherError String

data LType
    = TypeString
    | TypeBool
    | TypeInt
    deriving (Show)

data LOp
    = OpAdd
    | OpMul
    | EmptyOp
    deriving (Show)

data Expr
    = Val String
    | TypeExpr LType
    | LiteralExpr LType String
    | OperatorExpr LOp
    | OperatorAppl Expr LOp Expr
    | EmptyExpr
    | VariableDecl LType String Expr
    deriving (Show)

instance Show ParserError where
  show EOFError = "Unexpected EOF"
  show NotANumberError = "Invalid number"
  show InvalidBooleanError = "Invalid boolean"
  show InvalidStringError = "Invalid string"
  show NoParseError = "Parse error"
  show VariableExistsError = "Variable already exists"
  show UnassignedVariableError = "Variable was not assigned a value"
  show (OtherError str) = str -- blurp, unsafe

-- Maps keywords to Expressions
type Keyword = (String, Expr)
keywords :: [Keyword]
keywords = [("True", LiteralExpr TypeBool "T"),
            ("False", LiteralExpr TypeBool "F"),
            ("Bool", TypeExpr TypeBool),
            ("Int", TypeExpr TypeInt),
            ("String", TypeExpr TypeString)]

type LVariable = (LType, String)
type VariableEnvironment = [(String, LVariable)]

-- Tracks coloumn
type Location = Int
type ParserState = (
                    VariableEnvironment, -- Variables parsed
                    Location,  -- Location tracking where we are
                    String     -- Data we have to parse
                   )

type Parser a = ExceptT ParserError (WriterT [String] (State ParserState)) a

reserved = "+*="
whitespace = " "

mkParserState :: String -> ParserState
mkParserState str = ([("ENV", (TypeInt, "1337"))], 0, str)

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
      Left err -> show err ++ " at column " ++ show loc
      Right s  -> show s
    where
      ((res, log), (_, loc, rem)) = runMonadicParser f st

skipWhitespace :: Parser Expr
skipWhitespace = flip safe (return EmptyExpr) $ do
  (var, loc, str) <- get
  case str of
    (c : cs) ->
      if (c == ' ') then do
        put (var, loc + 1, cs)
        skipWhitespace
      else
        return EmptyExpr
    _        -> return EmptyExpr

-- Handles parsing error by returning a value when it hits EOF
-- and passing through to the next call if it is a different error
handleParseError f e x
  = case x of
      EOFError -> return e
      _        -> f

parseKeyword :: Parser Expr
parseKeyword = do
  (var, loc, str) <- get
  foldl1 (<|>) (map parseKeyword' keywords)

(<|>) :: Parser a -> Parser a -> Parser a
p1 <|> p2 = p1 `catchError` (\_ -> p2)

parseLine :: Parser Expr
parseLine = do
  init <- get
  parseVariableDecl `catchError` (\err -> do
    case err of
      (NoParseError) -> parseOp `catchError` (\err -> throwError err)
      _              -> throwError err)

parseLines :: Parser Expr
parseLines = do
  (var, _, str) <- get
  let lines = splitOneOf "\n;" str
  exp <- forM lines (\e -> do
        (ivar, iloc, _) <- get
        put (ivar, iloc, e)
        expr <- parseLine
        return expr)
  return (exp !! (length exp - 1))

-- TODO
-- (+++) :: Parser a -> Parser a -> Parser a
-- p1 +++ p2 = try p1 <|> p2

parseKeyword' :: Keyword -> Parser Expr
parseKeyword' (str, exp) = do
  (var, loc, st) <- get
  case st of
    (stripPrefix str -> Just r) -> do
        put (var, loc + length str, r)
        return exp
    []                          -> throwError EOFError
    _                           -> throwError NoParseError

parseWord :: Parser Expr
parseWord = do
  (var, loc, str) <- get
  case str of
    (c : cs) -> do
      let (w, r) = span (\x -> not $ elem x sep) str
      put (var, loc + length w, r)
      return $ Val w
    _        -> throwError EOFError
  where
    sep = whitespace ++ reserved

safe :: Parser a -> Parser a -> Parser a
safe p k = do
  p `catchError` (const k)

parseVariableDecl :: Parser Expr
parseVariableDecl = do
  tpe <- safe parseKeyword (return EmptyExpr)
  (TypeExpr dtype) <- case tpe of
             (TypeExpr ty) -> do
               return (TypeExpr ty)
             _             -> throwError NoParseError
  skipWhitespace
  (Val name) <- parseWord
  (var, _, _) <- get
  case lookup name var of
    Just _  -> throwError VariableExistsError
    Nothing -> do
      skipWhitespace
      tell ["Variable was not found"]
      (var, loc, str) <- get
      case str of
        (c : cs) -> do
          if (c == '=') then do
            put (var, loc + 1, cs)
            skipWhitespace
            r <- parseVal var parseWord
            v@(LiteralExpr _ val) <- case r of
                  k@(LiteralExpr dtype val) -> return k
                  _                         -> throwError $ OtherError "type mismatch"
            (_, l, s) <- get
            put ((name, (dtype, val)) : var, l, s)
            return $ VariableDecl dtype name v
          else throwError UnassignedVariableError
        _       -> do
          throwError UnassignedVariableError

-- Refactor?
-- Implement variable declarations
parseOp :: Parser Expr
parseOp = do
  skipWhitespace
  (var, _, _) <- get
  p1 <- parseVal var (parseWord `catchError` (\case {
          EOFError -> throwError $ OtherError "Argument expected"
        }))
  skipWhitespace
  (_, loc, str) <- get
  (OperatorExpr op) <- case str of
      (c : cs) -> do
        if (c == '+') then do
          put (var, loc + 1, cs)
          return $ OperatorExpr OpAdd
        else if (c == '*') then do
          put (var, loc + 1, cs)
          return $ OperatorExpr OpMul
        else throwError $ OtherError "Invalid operator"
      _        -> throwError EOFError
  skipWhitespace
  p2 <- parseVal var (parseWord `catchError` (\_ ->
        throwError $ OtherError "Partial application unsupported"))
  return (OperatorAppl p1 op p2)

parseString :: Parser Expr
parseString = do
  (var, loc, str) <- get
  case str of
    (c : cs) ->
      if (c == '"' && isSuffixOf "\"" cs) then do
        put (var, loc + (length str), [])
        return $ LiteralExpr TypeString (init cs)
      else throwError InvalidStringError
    _       -> throwError EOFError

parseNumbers :: Parser Expr
parseNumbers = do
  (var, loc, str) <- get
  case str of
    (c : cs) ->
      if (c >= '0' && c <= '9') then do
        put (var, loc + 1, cs)
        (LiteralExpr TypeInt x) <- parseNumbers `catchError`
            (handleParseError parseNumbers (LiteralExpr TypeInt []))
        return (LiteralExpr TypeInt (c : x))
      else
        throwError NotANumberError
    _        -> throwError EOFError

-- Parses a (Val x) Expression into VariableExpr or LiteralExpr
parseVal :: VariableEnvironment -> Parser Expr -> Parser Expr
parseVal env expr = do
  (Val e) <- expr
  tell ["parseVal: ", e]
  case (lookup e env) of
    Nothing      -> do
      init <- get
      put $ mkParserState e
      s@(LiteralExpr _ n) <- parseNumbers `catchError` (\err -> do
          put $ mkParserState e
          parseString `catchError` (\_ -> do
            throwError $ OtherError "Invalid literal or variable not found"))
      put init
      return s
    Just (t, v)  -> do
      return $ LiteralExpr t v
