module Gen_Parser where

import Parsing

data BTree a = Empty | Node a (BTree a) (BTree a)
                     deriving (Show)


flux :: Parser (BTree String)
flux = (>>=) command (\x ->
  (>>=) (string " ; ") (\_ ->
  (>>=) flux (\y ->
  return (Node ";" x y))))
       <|> (>>=) (char '(') (\_ -> (>>=) flux (\x -> (>>=) (char ')') (\_ ->
  (>>=) (string "||") (\_ ->
  (>>=) (char '(') (\_ -> (>>=) flux (\y -> (>>=) ( char ')') (\_ ->
  return (Node "||" x y))))))))
       <|> (>>=) (string "skip") (\_ ->
  return (Node "$" Empty Empty))
command :: Parser (BTree String)
command = (>>=) (char '{') (\_ ->
  (>>=) flux (\x ->
  (>>=) (char '}') (\_ ->
  return (Node "Prio" x Empty))))
      <|> (>>=) (string "assume ") (\_ ->
  (>>=) ident_t (\x ->
  return (Node "A" x Empty)))
      <|> (>>=) (string "force "
                <|> string "assert ") (\_ ->
  (>>=) ident_t (\x ->
  return (Node "F" x Empty)))
      <|> (>>=) (string "havoc ") (\_ ->
  (>>=) ident_t (\x ->
  return (Node "H" x Empty)))
      <|> (>>=) (string "havoc ") (\h ->
  (>>=) vector (\x ->
  return (Node "H" x Empty)))
vector = (>>=) (char '{') (\_ ->
  (>>=) vector_ids (\x ->
  return x))
vector_ids = (>>=) ident_t (\id ->
  (>>=) vector_more (\x ->
  return (Node "vector" id x)))
  <|> (>>=) ident_t (\id ->
  (>>=) (char '}') (\_ ->
  return id))
vector_more = (>>=) comma_t (\_ ->
  (>>=) vector_ids (\x ->
  return x))
comma_t = char ','
ident_t = (>>=) ident (\id ->
  return (Node "ID" (Node id Empty Empty) Empty))
