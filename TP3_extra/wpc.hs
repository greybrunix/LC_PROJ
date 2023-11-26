module WPC_Parser where

import Parsing

data BTree a = Empty | Node a (BTree a) (BTree a) deriving (Show)
-- Parser (BTree String))

flux :: Parser (BTree String)
flux = do string "skip"
          return (Node "TRUE" Empty Empty)
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
            string "||"
            y <- flux
            return (Node "||" x y)
command = assume
      <|> force
      <|> havoc
      <|> attrib
assume = do string "assume"
            space
            x <- log_expr
            return (Node "A" x Empty)
force  = do string "force" <|> string "assert"
            space
            x <- log_expr
            return (Node "F" x Empty)
havoc  = do string "havoc"
            space
            x <- vector
            return (Node "H" x Empty)
     <|> do string "havoc"
            space
            x <- ident
            return (Node "H" (Node "ID" (Node x Empty Empty) Empty) Empty)
attrib = do x <- vector
            string " <- "
            y <- expr_vector
            return (Node "<-" x y)
     <|> do x <- ident 
            string " <- "
            y <- expr
            return (Node "<-" (Node "ID" (Node x Empty Empty) Empty) y)
vector = do char '('
            x <- vector_ids
            return x
vector_ids = do x <- ident
                y <- vector_ids_m
                return (Node "," (Node "ID" (Node x Empty Empty) Empty) y)
            <|> do x <- ident ; char ')'
                   return (Node "ID" (Node x Empty Empty) Empty)
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
               return (Node "ID" (Node x Empty Empty) Empty)
        <|> do char '-'
               x <- expr
               return (Node "-" x Empty)
log_expr = do char '('
              x <- log_term
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
        <|> log_things
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
log_con = string "/\\" <|> string "\\/" <|> string "->"
log_things = do string "TRUE"
                return (Node "TRUE" Empty Empty)
          <|> do string "FALSE"  
                 return (Node "FALSE" Empty Empty)
          <|> do x <- ident
                 return (Node "ID" (Node x Empty Empty) Empty)

rdtree :: (BTree (String)) -> String
rdtree Empty = ""
rdtree (Node a t1 t2) = case a of
                               ";" -> rdtree t1 ++ rdtree t2 ++ ")"
                               "A" -> "Implies(" ++ rdtree t1 ++ "," ++ rdtree t2 
                               "F" -> "And(" ++ rdtree t1 ++ ","++  rdtree t2 
                               "TRUE" -> "And()" -- random tautology
                               "FALSE" -> "Or()" -- random contradiction
                               "ID" -> case t1 of
                                       (Node s _ _ ) -> s -- lol
                               "INT" -> case t1 of
                                       (Node s _ _ ) -> s -- lol again
                               "NOT" -> "Not(" ++ rdtree t1 ++ ")"
                               

wpc = rdtree . fst . head . parse flux
