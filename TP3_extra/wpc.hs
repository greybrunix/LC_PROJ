module WPC_Parser where

import Parsing

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
      <|> attrib
assume = do string "assume "
            x <- log_expr
            return (Node "A" x Empty)
force  = do string "force " <|> string "assert "
            x <- log_expr
            return (Node "F" x Empty)
havoc  = do string "havoc "
            x <- vector
            return (Node "H" x Empty)
     <|> do string "havoc "
            x <- ident
            return (Node "H" (Node x Empty Empty) Empty)
attrib = do x <- vector
            string " <- "
            y <- expr_vector
            return (Node "<-" x y)
     <|> do x <- ident 
            string " <- "
            y <- expr
            return (Node "<-" (Node x Empty Empty) y)
vector = do char '('
            x <- vector_ids
            return x
vector_ids = do x <- ident
                y <- vector_ids_m
                return (Node "," (Node x Empty Empty) y)
            <|> do x <- ident ; char ')'
                   return (Node x Empty Empty)
vector_ids_m = do char ','
                  x <- vector_ids
                  return x
expr_vector = do char '('
                 x <- exprs
                 return x
exprs = do x <- expr
           y <- exprs_m
           return (Node "," x y)
    <|> do x <- expr
           char ')'
           return x
exprs_m = do char ','
             x <- exprs
             return x
expr = do x <- term
          char '+'
          y <- expr
          return (Node "+" x y)
      <|> do x <- term
             char '-'
             y <- expr
             return (Node "-" x y)
      <|> term
term = do x <- factor
          char '*'
          y <- term
          return (Node "*" x y)
      <|> do x <- factor
             string " div "
             y <- term
             return (Node "/" x y)
      <|> factor
factor = do char '('
            x <- expr
            char ')'
            return x
        <|> do x <- int
               return (Node (show x) Empty Empty)
        <|> do x <- ident
               return (Node x Empty Empty)
        <|> do char '-'
               x <- expr
               return (Node "-" x Empty)
log_expr = do x <- ident ; return (Node x Empty Empty)
         <|> do char '('; x <- log_term
                y <- log_op
                z <- log_expr
                char ')'; return (Node y x z)
        <|> log_term
log_term = do x <- log_factor
              y <- log_con
              z <- log_term
              return (Node y x z)
        <|> log_factor
log_factor = do char '('
                x <- log_expr
                char ')'
                return x
        <|> do x <- log_things; return (Node x Empty Empty)
        <|> do x <- int; return (Node (show x) Empty Empty)
        <|> do string "ForAll"
               x <- vector
               string " . "
               y <- log_expr
               return (Node "FORALL" x y)
        <|> do string "Exists"
               x <- vector
               string " . "
               y <- log_expr
               return (Node "Exists" x y)
        <|> do char '~'
               x <- log_expr
               return (Node "NOT" x Empty)
log_op = string "<=" <|> string ">=" <|>  string "<" <|> string ">"
     <|> string "=" <|> string "!="
log_con = string "AND" <|> string "OR" <|> string "IMPLIES"
log_things = ident <|> string "TRUE" <|> string "FALSE"  
