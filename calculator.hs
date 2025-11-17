{-
  A basic calculator for arithmetic expressions
  Based on the example in Chapter 8 of "Programming in Haskell"
  by Graham Hutton.

  Pedro Vasconcelos, 2025
-}
module Main where

import Parsing
import Data.Char

--
-- a data type for expressions
-- made up from integer numbers, + and *
--
type Name = String
type Env = [(Name, Integer)]

data Expr = Num Integer
          | Add Expr Expr
          | Mul Expr Expr
          | Sub Expr Expr
          | Div Expr Expr
          | Mod Expr Expr
          | Var Name
          deriving Show

data Command = Assign Name Expr
              | Eval Expr
              deriving Show

-- searches for a variable in an environment
findVarEnv :: Name -> Env -> Integer
findVarEnv n env = case lookup n env of
              Just val -> val
              Nothing -> error ("Variavel não definida: " ++ n)

-- add a variable (name and integer) to an environment and updates it
update :: Name -> Integer -> Env -> Env
update name val env = (name, val) : filter (\(n,_) -> n /= name) env

-- a recursive evaluator for expressions
--
eval :: Env -> Expr -> Integer
eval env (Num n) = n
eval env (Add e1 e2) = eval env e1 + eval env e2
eval env (Mul e1 e2) = eval env e1 * eval env e2
eval env (Sub e1 e2) = eval env e1 - eval env e2
eval env (Div e1 e2) = eval env e1 `div` eval env e2
eval env (Mod e1 e2) = eval env e1 `mod` eval env e2
eval env (Var n) = findVarEnv n env

-- | a parser for expressions
-- Grammar rules:
-- expr ::= term exprCont
-- exprCont ::= '+' term exprCont | epsilon

-- term ::= factor termCont
-- termCont ::= '*' factor termCont | epsilon

-- factor ::= natural | '(' expr ')'

variable :: Parser Name
variable = many1 (satisfy isLetter)

command :: Parser Command
command =  do v <- variable
              char '='
              e <- expr
              return (Assign v e)
          <|>
           do e <- expr
              return (Eval e)


expr :: Parser Expr
expr = do t <- term
          exprCont t

exprCont :: Expr -> Parser Expr
exprCont acc = 
              (do char '+'
                  t <- term
                  exprCont (Add acc t))
                <|>
              (do char '-'
                  t <- term
                  exprCont (Sub acc t))
                <|>
                  return acc
              
term :: Parser Expr
term = do f <- factor
          termCont f

termCont :: Expr -> Parser Expr
termCont acc =  
                (do char '*'
                    f <- factor  
                    termCont (Mul acc f))
                <|>
                (do char '/'
                    f <- factor
                    termCont (Div acc f))
                <|>
                (do char '%'
                    f <- factor
                    termCont (Mod acc f))
                <|> return acc


factor :: Parser Expr
factor =
        (do v <- variable
            (return (Var v)))
        <|>
          (do n <- natural
              return (Num n))
        <|>
        (do char '('
            e <- expr
            char ')'
            return e)

             

natural :: Parser Integer
natural = do xs <- many1 (satisfy isDigit)
             return (read xs)

----------------------------------------------------------------             

main :: IO ()
main
  = do txt <- getContents
       -- Start the 'calculator' loop with an EMPTY environment ([])
       calculator [] (lines txt)

-- | read-eval-print loop (now stateful)
--   Receives the CURRENT environment and the remaining lines
calculator :: Env -> [String] -> IO ()
calculator env []  = return () -- No more lines, terminate
calculator env (l:ls) = do
    -- Ignore empty lines (to avoid errors)
    if null l
    then calculator env ls
    else do
      -- Execute line 'l' with the current environment 'env'
      let (output, newEnv) = execute env l
      -- Print the result
      putStrLn output
      -- Recurse with the NEW environment (newEnv)
      -- and the rest of the lines (ls)
      calculator newEnv ls

-- | execute a single command
--   This is the main function that replaces 'evaluate'
execute :: Env -> String -> (String, Env)
execute env txt =
  case parse command txt of -- <-- Parse a 'command'

    -- Case 1: The command was 'Eval expr'
    -- The parser succeeded and left no remaining text ("")
    [(Eval expr, "")] -> (show value, env) -- Return the value, and the UNCHANGED env
      where
        value = eval env expr  -- Evaluate the expression in the current env

    -- Case 2: The command was 'Assign name expr'
    [(Assign name expr, "")] -> (show value, newEnv) -- Return the value, and the NEW env
      where
        value  = eval env expr           -- Evaluate the expression in the current env
        newEnv = update name value env -- Create a NEW env with the variable

    -- Case 3: The parse failed (e.g., '[]') or left text (e.g., '[(... ,"abc")]')
    _ -> ("parse error", env) -- Return error, and the UNCHANGED env