module Gen_Parser where

import Parsing

data BTree a = Empty | Node a (BTree a) (BTree a)
  deriving (Show)
data NTree a = EmptyN | NodeN a [NTree a]
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
  (>>=) (string "Skip") (\_ ->
  return (Node "$" Empty Empty))
command :: Parser (BTree String)
command =
  (>>=) (char '{') (\_ ->
  (>>=) flux (\x ->
  (>>=) (char '}') (\_ ->
  return (Node "Prio" x Empty))))
      <|>
  (>>=) (string "Assume ") (\_ ->
  (>>=) ident_t (\x ->
  return (Node "A" x Empty)))
      <|>
  (>>=) (string "Force "
        <|>
        string "Assert ") (\_ ->
  (>>=) ident_t (\x ->
  return (Node "F" x Empty)))
      <|>
  (>>=) (string "Havoc ") (\_ ->
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
  return (Node id Empty Empty))
int_t =
  (>>=) int (\i ->
  return (Node (show i) Empty Empty))
-- end recognizing

-- translation parser
check_commas :: BTree (String) -> Bool
check_commas Empty = True
check_commas (Node a t1 t2) = if ((==) a ":=") then (==) (count_commas t1) (count_commas t2) else (&&) (check_commas t1) (check_commas t2)

count_commas :: BTree (String) -> Int
count_commas Empty = 0
count_commas (Node a t1 t2) = if ((==) a "vector") then succ (count_commas t2) else (+) (count_commas t1) (count_commas t2)

ntree :: BTree (String) -> NTree (String)
ntree Empty = EmptyN
ntree (Node a Empty Empty) = NodeN a []
ntree (Node a Empty t2) = NodeN a [ntree t2]
ntree (Node a t1 Empty) = NodeN a [ntree t1]
ntree (Node a t1 t2) = NodeN a ((ntree t1) : [(ntree t2)])

cmps :: NTree (String) -> NTree (String)
cmps EmptyN = EmptyN
cmps (NodeN a []) = (NodeN a [])
cmps (NodeN a (t1:[])) = if (/=) a "Prio"
                              then if (/=) a ";"
                                   then (NodeN a [cmps t1])
                                   else (NodeN a (cmp t1))
                            else (NodeN a [cmps t1])
cmps (NodeN a (t1:[t2]))= if (/=) a ";"
                                   then (NodeN a ((cmps t1):([cmps t2])))
                                   else (NodeN a ((cmps t1):(cmp t2)))
chcs :: NTree (String) -> NTree (String)
chcs EmptyN = EmptyN
chcs (NodeN a []) = (NodeN a [])
chcs (NodeN a (t1:[])) = if (/=) a "Prio"
                            then if (/=) a "||"
                                  then (NodeN a [chcs t1])
                                 else (NodeN a (chc t1))
                           else (NodeN a [chcs t1])
chcs (NodeN a (t1:[t2]))= if (/=) a "||"
                                   then (NodeN a ((chcs t1):([chcs t2])))
                                   else (NodeN a ((chcs t1):(chc t2)))

cmp :: NTree (String) -> [NTree (String)]
cmp EmptyN = []
cmp (NodeN a []) = [NodeN a []]
cmp (NodeN a [t1]) = [NodeN a [t1]]
cmp (NodeN a (t1:[t2])) =
    if (/=) a ";"
     then ([cmps t1 , (cmps t2)])
    else ((cmps t1):(cmp t2))

chc :: NTree (String) -> [NTree (String)]
chc EmptyN = []
chc (NodeN a []) = [NodeN a []]
chc (NodeN a [t1]) = [NodeN a [t1]]
chc (NodeN a (t1:[t2])) =
    if (/=) a "||"
     then ([chcs t1 , (chcs t2)])
    else ((chcs t1):(chc t2))

consume = tree2eval . chcs . cmps . ntree . fst . head . parse flux

tree2eval :: NTree String -> String
tree2eval EmptyN = ""
tree2eval (NodeN a t) = case a of
  ";"       -> "compo(" ++ eval (t)  ++ ")"
  "||"      -> "choice(" ++ eval (t) ++ ")"
  "Prio"    -> x t 
  "A"       -> "assume(" ++ x t ++ ")"
  "F"       -> "force("  ++ x t ++ ")"
  "H"       -> "havoc(["  ++ x t ++ "])"
  "$"       -> "skip()"
  "vector"  -> x t ++ ", " ++ y t
  ":="      -> "attrib([" ++ x t ++ "], [" ++ y t ++ "])"
  " + "     -> x t ++ " + " ++ y t
  " - "     -> x t ++ " - " ++ y t
  " * "     -> x t ++ " * " ++ y t
  " div "   -> x t ++ " div " ++ y t
  _         -> a
x = tree2eval . head
y = x . tail
eval :: [NTree String] -> String
eval  [] = ""
eval  [t1] = tree2eval t1
eval (t1:ts) = tree2eval t1 ++ ", " ++ eval ts
-- end translation
