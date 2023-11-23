module Parse where

import Parsing

-- expr ::= term + expr | term
-- term ::= factor * term | factor
-- factor ::= (expr) | int

expr = do x <- term
          char '+'
          y <- expr
          return (Node (Right '+') x y)
        <|> term
term = do x <- factor
          char '*'
          y <- term
          return (Node (Right ('*')) x y)
        <|> factor

factor = do char '('
            x <- expr
            char ')'
            return (x)
         <|> do n <- int
                return (Node (Left n) (Empty) (Empty))
