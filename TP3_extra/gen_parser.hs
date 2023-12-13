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
  return x)))
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
ntree :: BTree (String) -> NTree (String)
ntree Empty = error "empty input?"
ntree (Node a Empty Empty) = NodeN a []
ntree (Node a Empty t2) = NodeN a [ntree t2]
ntree (Node a t1 Empty) = NodeN a [ntree t1]
ntree (Node a t1 t2) = NodeN a ((ntree t1) : [(ntree t2)])

cmps_chcs :: NTree (String) -> NTree (String)
cmps_chcs EmptyN = error "Empty?"
cmps_chcs (NodeN a []) = (NodeN a [])
cmps_chcs (NodeN a (t1:[])) = NodeN a [t1]
cmps_chcs (NodeN a (t1:[t2]))= if (/=) a ";"
                                then if (/=) a "||"
                                      then (NodeN a (t1:[t2]))
                                     else chc (NodeN a (t1:[t2]))
                               else cmp (NodeN a (t1:[t2]))

cmp :: NTree (String) -> NTree String
cmp EmptyN = error "Empty?"
cmp (NodeN a []) = NodeN a []
cmp (NodeN a [t1]) = NodeN a [t1]
cmp (NodeN a (((NodeN b l1)):[NodeN c l2])) =
    if (/=) b ";"
     then if (/=) c ";"
           then (NodeN a 
                       (
                          (cmps_chcs (NodeN b l1))
                         :[(cmps_chcs (NodeN c l2))]
                       ))
          else (NodeN a (
                          (NodeN b l1)
                         :(cm (NodeN c l2))
                        ))
    else (NodeN a (
                    (cm (NodeN b l1)) 
                      ++
                    [cmps_chcs (NodeN c l2)]
                  ))

chc :: NTree (String) -> NTree (String)
chc EmptyN = error "Empty?"
chc (NodeN a []) = NodeN a []
chc (NodeN a [t1]) = NodeN a [t1]
chc (NodeN a (((NodeN b l1)):[NodeN c l2])) =
    if (/=) b "||"
     then if (/=) c "||"
           then (NodeN a (
                            (cmps_chcs (NodeN b l1))
                           :[(cmps_chcs (NodeN c l2))]
                         ))
          else (NodeN a (
                           (NodeN b l1)
                          :(ch (NodeN c l2))
                        ))
    else (NodeN a (
                     (ch (NodeN b l1))
                        ++
                     [cmps_chcs (NodeN c l2)]
                   ))

cm :: NTree String -> [NTree String]
cm (NodeN a ((NodeN b l1):l2) = if 
ch :: NTree String -> [NTree String]
ch (NodeN _ _) = []
consume = tree2eval . cmps_chcs . ntree . fst . head . parse flux

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
