-- Copyright 01-Jan-2024 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | JSON management.

module Kut.Js
  ( wb,
    rb,
    wi,
    ri,
    wl,
    rl,
    wd,
    rd,
    ws,
    rs,
    wa,
    ra,
    wo,
    ro
  ) where

import qualified Kut.Str as Str
import qualified Kut.Map as Map

-- UTILITIES

msg :: String -> String -> String
msg template js = Str.replace "%" (take 50 js) template

skipBlanks :: String -> String
skipBlanks [] = []
skipBlanks s@(x:xs)
  | x <= ' ' = skipBlanks xs
  | otherwise = s

skipString :: String -> String -> (String, String)
skipString s' s = skip [] s
  where
  skip _ [] = error $ msg "'%' Bad String (End quotes missing)" s'
  skip r ('\\':'"':xs) = skip ('"':'\\':r) xs
  skip r ('"': xs) = ('"':(reverse ('"':r)), skipBlanks xs)
  skip r (x:xs) = skip (x:r) xs

skipContainer :: Bool -> String -> String -> (String, String)
skipContainer isObject s' s = skip isObject [] s
  where
  skip _ _ [] = error $ msg "'%' Bad Array (End square missing)" s'
  skip False r (']':xs) = ('[':(reverse (']':r)), skipBlanks xs)
  skip True r ('}':xs) = ('{':(reverse ('}':r)), skipBlanks xs)
  skip isO r ('"':xs) = let (js, rest) = skipString s' xs
                        in skip isO ((reverse js) ++ r) rest
  skip False r ('[':xs) = let (js, rest) = skipContainer False s' xs
                          in skip False ((reverse js) ++ r) rest
  skip True r ('{':xs) = let (js, rest) = skipContainer True s' xs
                         in skip True ((reverse js) ++ r) rest
  skip isO r (x:xs) = skip isO (x:r) xs

skipAtomic :: String -> (String, String)
skipAtomic s = skip [] s
  where
  skip r [] = (reverse r, [])
  skip r js@(x:xs)
    | x == ',' || x == ']' || x == '}' = (reverse r, js)
    | otherwise = skip (x:r) xs

-- PUBLIC INTERFACE

-- | wb v
--
-- Writes a boolean value.
wb :: Bool -> String
wb v = if v then "true" else "false"

-- | rb
--
--  Reads a boolean value.
rb :: String -> Bool
rb s = if s' == "true" then True
  else if s' == "false" then False
  else error $ msg "'%' is not a valid JSON Bool" s'
  where
    s' = Str.trim s

-- | wi v
--
--  Writes a Int value
wi :: Int -> String
wi v = show (v::Int)

-- | ri s
--
-- | Reads an Int value
ri :: String -> Int
ri = round . rd

-- | wl v
--
-- Writes an Integer (long) value
wl :: Integer -> String
wl v = show (v::Integer)

-- | rl s
--
-- | Reads an Integer (long) value
rl :: String -> Integer
rl = round . rd

-- | wd v
--
-- Writes a Double value
wd :: Double -> String
wd v = show (v::Double)

-- | rd s
--
-- | Reads an Double value
rd :: String -> Double
rd = read . Str.trim

-- | ws v
--
-- Writes a String value
ws :: String -> String
ws v = "\"" ++ escape [] v ++ "\""
  where
    escape r [] = reverse r
    escape r (x:xs) = case x of
      '"' -> escape ('\\' : '"' : r) xs
      '\\' -> escape ('\\' : '\\' : r) xs
      '\n' -> escape ('n' : '\\' : r) xs
      '\t' -> escape ('t' : '\\' : r) xs
      '\r' -> escape ('r' : '\\' : r) xs
      '\b' -> escape ('b' : '\\' : r) xs
      '\f' -> escape ('f' : '\\' : r) xs
      ch -> escape (ch:r) xs

-- | rs s
--
-- | Reads a String value
rs :: String -> String
rs s =
  let s' = Str.trim s
  in case s' of
    [] -> msg "'%' Bad String (Start quotes missing)" s'
    (x:xs) ->
      if x /= '"' then error $ msg "'%' Bad String (Start quotes missing)" s'
      else unescape s' [] xs
  where
    unescape s' _ [] = error $ msg "'%' Bad String (End quotes missing)" s'
    unescape _ r "\"" = reverse r
    unescape s' _ ('"':_) = error $ msg "'%' Extra characters in String" s'
    unescape s' r ('\\':'u':d1:d2:d3:d4:xs) =
      let dgs = [d1, d2, d3, d4]
      in if all (isHex) dgs
        then unescape s' (toEnum ((read ("0x" ++ dgs))::Int):r) xs
        else error $ msg "'%' Bad String (Bad unicode escape)" s'
    unescape s' r ('\\':x:xs) =
      case x of
        '"' -> unescape s' ('"':r) xs
        '\\' -> unescape s' ('\\':r) xs
        'n' -> unescape s' ('\n':r) xs
        't' -> unescape s' ('\t':r) xs
        'r' -> unescape s' ('\r':r) xs
        'b' -> unescape s' ('\b':r) xs
        'f' -> unescape s' ('\f':r) xs
        _ -> error $ msg "'%' Bad String (Bad escape sequence)" s'
    unescape s' r (x:xs) = unescape s' (x:r) xs

    isHex ch = (ch >= '0' && ch <= '9') ||
      (ch >= 'a' && ch <= 'f') ||
      (ch >= 'A' && ch <= 'F')

-- | wa v
--
-- Writes a List<JSON> value
wa :: [String] -> String
wa v = ('[':Str.join "," v) ++ "]"

-- | ra s
--
-- | Reads a List<JSON> value
ra :: String -> [String]
ra s =
  let s' = Str.trim s
  in case s' of
    [] -> error $ msg "'%' Bad Array (Start square missing)" s'
    "[]" -> []
    (x:xs) ->
      if x /= '[' then error $ msg "'%' Bad Array (Start square missing)" s'
      else
        let (es, rest) = readElement s' [] xs
        in case rest of
          [] -> reverse es
          _ -> error $ msg "'%' Extra characters in Array" s'
  where
    readElement s' _ [] = error $ msg "'%' symbol ']' is missing" s'
    readElement s' es ('"':xs) = readElementEnd s' es $ skipString s' xs
    readElement s' es ('[':xs) = readElementEnd s' es $ skipContainer False s' xs
    readElement s' es ('{':xs) = readElementEnd s' es $ skipContainer True s' xs
    readElement s' es js@(x:xs)
      | (x <= ' ') = readElement s' es xs
      | otherwise = readElementEnd s' es $ skipAtomic js

    readElementEnd s' _ ([], _) = error $ msg "'%' missing value in Array" s'
    readElementEnd s' es (js, (',':xs)) = readElement s' (js:es) xs
    readElementEnd _ es (js, (']':xs)) = (js:es, xs)
    readElementEnd s' _ _ = error $ msg "'%' symbol ']' or ',' is missing" s'

-- | wo v
--
-- Writes a Dictionary with JSON values. Duplicates old keys are removed.
wo :: Map.D -> String
wo v = ('{':Str.join "," (map we v)) ++ "}"
  where
    we (k, js) = ws(k) ++ (':':js)

-- | ro v
--
-- Reads a Dictionary with JSON values.
ro :: String -> Map.D
ro s =
  let s' = Str.trim s
  in case s' of
    [] -> error $ msg "'%' Bad Object (Start bracket missing)" s'
    "{}" -> []
    (x:xs) ->
      if x /= '{' then error $ msg "'%' Bad Object (Start bracket missing)" s'
      else
        let (es, rest) = readElement s' [] xs
        in case rest of
          [] -> reverse es
          _ -> error $ (msg "'%' Extra characters in Object" s')
  where
    readElement s' _ [] = error $ msg "'%' symbol '}' is missing" s'
    readElement s' es (' ':xs) = readElement s' es xs
    readElement s' es ('"':xs) = let (k, rest) = skipString s' xs
                                 in case rest of
                                  (':':rest') -> readRest s' es (rs k) rest'
                                  _ -> error $ msg "'%' symbol ':' is missing" s'
    readElement s' _ _ = error $ msg "'%' object key is missing" s'

    readRest s' _ _ [] = error $ msg "'%' symbol '}' is missing" s'
    readRest s' es k ('"':xs) = readElementEnd s' es k $ skipString s' xs
    readRest s' es k ('[':xs) = readElementEnd s' es k $ skipContainer False s' xs
    readRest s' es k ('{':xs) = readElementEnd s' es k $ skipContainer True s' xs
    readRest s' es k js@(x:xs)
      | (x <= ' ') = readRest s' es k xs
      | otherwise = readElementEnd s' es k $ skipAtomic js

    readElementEnd s' _  _ ([], _) = error $ msg "'%' missing value in Object" s'
    readElementEnd s' es k (js, (',':xs)) = readElement s' ((k, js):es) xs
    readElementEnd _ es k (js, ('}':xs)) = ((k, js):es, xs)
    readElementEnd s' _ _ _ = error $ msg "'%' symbol '}' or ',' is missing" s'

