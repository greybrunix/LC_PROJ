module WPC_Parser where

import Parsing
{-
<vector>      ::= LPAREN <vector_ids>
                | ID
<vector_ids>  ::= ID <vec_ids_m>
                | RPAREN
<vec_ids_m>   ::= COMMA <vector_ids>
                | <vector_ids>
<log_expr>    ::= <log_term> op_ex <log_expr>
                | <log_term>
<log_term>    ::= <log_factor> op_term <log_term>
                | <log_factor>
<log_factor>  ::= LPAREN <log_factor> RPAREN
                | ID
                | INT
                | True
                | False
                | NOT <expr>
                | Forall <vector> DOT <expr>
                | Exists <vector> DOT <expr>
<op_ex>       ::= LT | LE | GT | GE | EQ | NE
<expr_vector> ::= LPAREN <exprs>
                | <expr>
<exprs>       ::= <expr> <exprs_m>
                | RPAREN
<exprs_m>     ::= COMMA  <exprs>
                | <exprs>
<expr>        ::=  <term> ADD <expr> | <term> SUB <expr> | <term>
<term>        ::=  <factor> MUL <term> | <factor> DIV <term> | <factor>
<factor>      ::=  LPAREN <expr> RPAREN | INT | ID
-}

-- Parser (BTree String))

flux :: Parser (BTree String)
flux = do string "skip"
          return (Node "$" Empty Empty)
   <|> comp
   <|> do char '('
          x <- choice
          char ')'
          return (x) 
   <|> do char '('
          x <- flux
          char ')'
          return (x)
comp = do x <- command
          char ';'
          y <- flux
          return (Node ";" x y)
choice = do x <- flux
            char '|'
            char '|'
            y <- flux
            return (Node "|" x y)
command = assume
      <|> force
      <|> havoc
--      <|> attrib
assume = do string "assume "
            x <- log_expr
            return (Node "A" x Empty)
force  = do string "force " <|> string "assert "
            x <- log_expr
            return (Node "F" x Empty)
havoc  = do string "havoc "
            x <- vector
            return (Node "H" x Empty)

{-
attrib = do x <- vector
            char '<'
            char '-'
            y <- expr_vector
            return (Node ":=" x y)
-}
vector = do char '('
            x <- vector_ids
            return x
        <|> do x <- ident
               return (Node x Empty Empty)
vector_ids = do x <- ident
                y <- vector_ids_m
                return (Node "," (Node x Empty Empty) y)
            <|> do x <- ident ; char ')'
                   return (Node x Empty Empty)
vector_ids_m = do char ','
                  x <- vector_ids
                  return x


log_expr = do x <- ident ; return (Node x Empty Empty)
--       <|> do askld
