module WPC_Parser where

import Parsing

{-
WPC GRAMMAR
<flux>        ::= SKIP RPAREN
                | LPAREN <comp> 
                | LPAREN <choice>
<comp>        ::= <command> COMP <flux>
<choice>      ::= <flux> CHOICE <flux> 
<command>     ::= <assume> 
                | <force> 
                | <attrib> 
                | <havoc>
<assume>      ::= ASSUME <log_expr>
<force>       ::= FORCE <log_expr>
<attrib>      ::= <vector> ATTRIB <expr_vector>
<havoc>       ::= HAVOC <vector>
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

-- Parser (BTree (Either String Char))

flux :: Parser (BTree (Either String Char))
flux = do string "skip" 
          char ')'
          return (Node (Right '$') Empty Empty)
   <|> comp
   <|> do char '('
          x <- choice
          return x
   <|> do char '('
          x <- flux
          return (x)
comp = do x <- command
          char ';'
          y <- flux
          return (Node (Right ';') x y)
choice = do x <- flux
            char '|'
            char '|'
            y <- flux
            return (Node (Right '|') x y)
command = assume
      <|> force
      <|> havoc
      <|> attrib
assume = do string "assume "
            x <- log_expr
            return (Node (Right 'A') x Empty)
force  = do string "force " <|> string "assert "
            x <- log_expr
            return (Node (Right 'F') x Empty)
havoc  = do string "havoc "
            x <- vector
            return (Node (Right 'H') x Empty)
attrib = do x <- vector
            char '<'
            char '-'
            y <- expr_vector
            return (Node (Right 'a') x y)
log_expr = do x <- ident ; return (Node (Left x) Empty Empty)
       <|> do askld
