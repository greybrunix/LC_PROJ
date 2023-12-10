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
      <|> do char '{'
             x <- flux
             char '}'
             return x
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
               return (Node "INT" (Node (show x) Empty Empty) Empty)
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
log_factor = do string "forall "
                x <- vector
                char '.';space
                y <- log_expr
                return (Node "FORALL" x y)
        <|> do string "forall "
               x <- ident
               char '.';space
               y <- log_expr
               return (Node "FORALL" (Node "ID" (Node x Empty Empty) Empty) y)
        <|> do string "exists "
               x <- vector
               char '.';space
               y <- log_expr
               return (Node "EXISTS" x y)
        <|> do string "exists "; x <- ident;char '.';space;y <- log_expr
               return (Node "EXISTS" (Node "ID" (Node x Empty Empty) Empty) y)
        <|> do char '('
               x <- log_expr
               char ')'
               return x
        <|> do char '~'
               x <- log_expr
               return (Node "NOT" x Empty)
        <|> do x <- int; return (Node "INT" (Node (show x) Empty Empty) Empty)
        <|> log_things
log_op = string "<=" <|> string ">=" <|>  string "<" <|> string ">"
     <|> string "=" <|> string "!="
log_con = string "/\\" <|> string "\\/" <|> string "->"
log_things = do string "TRUE"
                return (Node "TRUE" Empty Empty)
          <|> do string "FALSE"  
                 return (Node "FALSE" Empty Empty)
          <|> do x <- ident
                 return (Node "ID" (Node x Empty Empty) Empty)

smt :: (BTree (String)) -> String
smt Empty = ""
smt (Node a t1 t2) = case a of
                               ";" -> smt t1 ++ smt t2 ++ ")"
                               "A" -> "Implies(" ++ smt t1 ++ "," ++ smt t2 
                               "F" -> "And(" ++ smt t1 ++ ","++  smt t2 
                               "TRUE" -> "And()" -- random tautology
                               "FALSE" -> "Or()" -- random contradiction
                               "ID" -> case t1 of
                                       (Node s _ _ ) -> s -- lol
                               "INT" -> case t1 of
                                       (Node s _ _ ) -> s -- lol again
                               "NOT" -> "Not(" ++ smt t1 ++ ")"
                               "||"  -> "And(" ++ smt t1 ++ "," ++ smt t2 ++ ")"

rdtree :: (BTree (String)) -> String
rdtree Empty = ""
rdtree (Node a t1 t2) = case a of
                         ";" -> case t1 of
                                (Node "<-" _ _) -> "(" ++ rdtree t2 ++ ")" ++ rdtree t1
                                (Node "H" t1' t2') -> "forall a. " ++ rdtree t2 ++ rdtree t1
                                _ ->"(" ++ rdtree t1 ++ rdtree t2 ++ ")"
                         "A" -> rdtree t1 ++ " -> " ++ rdtree t2 
                         "F" -> rdtree t1 ++ " /\\ " ++  rdtree t2 
                         "TRUE" -> "True" -- random tautology
                         "FALSE" -> "False" -- random contradiction
                         "ID" -> case t1 of
                                 (Node s _ _ ) -> s
                         "INT" -> case t1 of
                                 (Node s _ _ ) -> s
                         "NOT" -> "~(" ++ rdtree t1 ++ ")"
                         "||" -> "(" ++ rdtree t1 ++ " /\\ " ++ rdtree t2 ++ ")"
                         "<-" -> "[" ++ rdtree t1 ++ "/" ++ rdtree t2 ++ "]"
                         "H"  -> "[" ++ rdtree t1 ++ "/a]"
                         "/\\" -> "(" ++ rdtree t1 ++ " /\\ " ++ rdtree t2 ++ ")"
                         "\\/" -> "(" ++ rdtree t1 ++ " \\/ " ++ rdtree t2 ++ ")"
                         "->" -> "(" ++ rdtree t1 ++ " -> " ++ rdtree t2 ++ ")"
                         "=" -> "(" ++ rdtree t1 ++ " = " ++ rdtree t2 ++ ")"
                         "<=" -> "(" ++ rdtree t1 ++ " <= " ++ rdtree t2 ++ ")"
                         ">=" -> "(" ++ rdtree t1 ++ " >= " ++ rdtree t2 ++ ")"
                         ">" -> "(" ++ rdtree t1 ++ " > " ++ rdtree t2 ++ ")"
                         "<" -> "(" ++ rdtree t1 ++ " < " ++ rdtree t2 ++ ")"
                         "+" -> "(" ++ rdtree t1 ++ " + " ++ rdtree t2 ++ ")"
                         "-" -> "(" ++ rdtree t1 ++ " - " ++ rdtree t2 ++ ")"
                         "*" -> "(" ++ rdtree t1 ++ " * " ++ rdtree t2 ++ ")"
                         "!=" -> "(" ++ rdtree t1 ++ " != " ++ rdtree t2 ++ ")"
                         "/" -> "(" ++ rdtree t1 ++ " div " ++ rdtree t2 ++ ")"
                         "FORALL" -> "forall " ++ rdtree t1 ++ ". (" ++ rdtree t2 ++ ")"
                         "EXISTS" -> "exists " ++ rdtree t1 ++ ". (" ++ rdtree t2 ++ ")"
-- missing vectors and vector expressions

consume :: [(BTree String,String)] -> Maybe String
consume [] = Nothing
consume ((_,(x:xs)):[]) = Nothing
consume x  = Just (rdtree . fst . head $ x)


consume_pysmt :: [(BTree String,String)] -> Maybe String
consume_pysmt [] = Nothing
consume_pysmt x  = Just (smt . fst . head $ x) 

wpc = consume . parse flux
wpc_pysmt = consume_pysmt . parse flux
