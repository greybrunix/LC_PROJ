module Gen_Parser where

import Parsing

data BTree a = Empty | Node a (BTree a) (BTree a)
                     deriving (Show)

-- recognizing grammar
flux :: Parser (BTree String)
flux =
  (>>=) command (\x ->
  (>>=) (string " ; ") (\_ ->
  (>>=) flux (\y ->
  return (Node ";" x y))))
       <|>
  (>>=) (char '(') (\_ ->
       (>>=) flux (\x ->
       (>>=) (char ')') (\_ ->
  (>>=) (string " || ") (\_ ->
  (>>=) (char '(') (\_ ->
        (>>=) flux (\y ->
        (>>=) ( char ')') (\_ ->
  return (Node "||" x y))))))))
       <|>
  (>>=) (string "skip") (\_ ->
  return (Node "$" Empty Empty))
command :: Parser (BTree String)
command =
  (>>=) (char '{') (\_ ->
  (>>=) flux (\x ->
  (>>=) (char '}') (\_ ->
  return (Node "Prio" x Empty))))
      <|>
  (>>=) (string "assume ") (\_ ->
  (>>=) ident_t (\x ->
  return (Node "A" x Empty)))
      <|>
  (>>=) (string "force "
        <|>
        string "assert ") (\_ ->
  (>>=) ident_t (\x ->
  return (Node "F" x Empty)))
      <|>
  (>>=) (string "havoc ") (\_ ->
  (>>=) (ident_t
        <|>
        vector) (\x ->
  return (Node "H" x Empty)))
      <|>
  (>>=) (vector
        <|>
        ident_t) (\var ->
  (>>=) (string " := ") (\_ ->
  (>>=) (expr
        <|>
        vec_ex) (\exp ->
  return (Node ":=" var exp))))
expr =
  (>>=) term (\t ->
  (>>=) a_op (\s ->
  (>>=) expr (\e ->
  return (Node s t e))))
        <|>
  term
term =
  (>>=) factor (\f ->
  (>>=) m_op   (\s ->
  (>>=) term   (\t ->
  return (Node s f t))))
        <|>
  factor
factor =
  (>>=) (char '(') (\_ ->
  (>>=) expr (\e ->
  (>>=) (char ')') (\_ ->
  return e)))
        <|>
  ident_t <|> int_t
a_op = string " + " <|> string " - "
m_op = string " * " <|> string " div "
vector =
  (>>=) (char '{') (\_ ->
  (>>=) vector_ids (\x ->
  return x))
vector_ids =
  (>>=) ident_t (\id ->
  (>>=) vector_more (\x ->
  return (Node "vector" id x)))
  <|>
  (>>=) ident_t (\id ->
  (>>=) (char '}') (\_ ->
  return id))
vector_more =
  (>>=) comma_t (\_ ->
  (>>=) vector_ids (\x ->
  return x))
vec_ex =
  (>>=) (char '{') (\_ ->
  (>>=) vec_ex_ids (\x ->
  return x))
vec_ex_ids =
  (>>=) expr (\e ->
  (>>=) vec_ex_more (\x ->
  return (Node "vector" e x)))
  <|>
  (>>=) expr (\e ->
  (>>=) (char '}') (\_ ->
  return e))
vec_ex_more =
  (>>=) comma_t (\_ ->
  (>>=) vec_ex_ids (\x ->
  return x))
comma_t = char ','
ident_t =
  (>>=) ident (\id ->
  return (Node "ID" (Node id Empty Empty) Empty))
int_t =
  (>>=) int (\i ->
  return (Node "INT" (Node (show i) Empty Empty) Empty))
-- end recognizing

-- translation parser
consume :: [(BTree (String), String)] -> Maybe String
consume []     = Nothing
consume ((x,y:ys):xs) = Nothing
-- entire input was consumed
consume ((x,""):xs) = if (check_commas(x)) then Just (tree2eval x) else Nothing

check_commas :: BTree (String) -> Bool
check_commas Empty = True
check_commas (Node a t1 t2) = if ((==) a ":=") then (==) (count_commas t1) (count_commas t2) else and [(check_commas t1),(check_commas t2)]

count_commas :: BTree (String) -> Int
count_commas Empty = 0
count_commas (Node a t1 t2) = if ((==) a "vector") then succ (count_commas t2) else (+) (count_commas t1) (count_commas t2)

tree2eval :: BTree String -> String
tree2eval Empty = ""
tree2eval (Node a t1 t2) = case a of
  ";"       -> "compo(" ++ tree2eval t1 ++ ", " ++ tree2eval t2 ++ ")"
  "||"      -> "choice(" ++ tree2eval t1 ++ ", " ++ tree2eval t2 ++ ")"
  "Prio"    -> tree2eval t1 ++ tree2eval t2
  "A"       -> "assume(" ++ tree2eval t1 ++ ")"
  "F"       -> "force("  ++ tree2eval t1 ++ ")"
  "H"       -> "havoc(["  ++ tree2eval t1 ++ "])"
  "$"       -> "skip()"
  "vector"  -> tree2eval t1 ++ ", " ++ tree2eval t2
  ":="      -> "attrib([" ++ tree2eval t1 ++ "], [" ++ tree2eval t2 ++ "])"
  " + "     -> tree2eval t1 ++ " + " ++ tree2eval t2
  " - "     -> tree2eval t1 ++ " - " ++ tree2eval t2
  " * "     -> tree2eval t1 ++ " * " ++ tree2eval t2
  " div "   -> tree2eval t1 ++ " div " ++ tree2eval t2
  "ID"      -> tree2eval t1
  "INT"     -> tree2eval t1
  _         -> a
-- end translation
