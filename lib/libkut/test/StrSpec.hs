-- Copyright 27-Jan-2020 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

module StrSpec (strTest) where

import qualified Kut.Str as Str

import Kut.Test

strTest :: IO ()
strTest = do
  putStrLn "Str test"

  --putStrLn "  -split"
  teq (Str.split ",," "") [""]
  teq (Str.split ",," "a") ["a"]
  teq (Str.split ",," "abc") ["abc"]
  teq (Str.split ",," "a,,") ["a", ""]
  teq (Str.split ",," ",,abc") ["", "abc"]
  teq (Str.split ",," "1,,abc") ["1", "abc"]
  teq (Str.split ",," ",,1,,abc") ["", "1", "abc"]
  teq (Str.split ",," ",,1,,abc,,") ["", "1", "abc", ""]
  teq (Str.split ",," "z,,1,,abc,,yy") ["z", "1", "abc", "yy"]

  --putStrLn "  -break"
  teq (Str.break ",," "") ("", "")
  teq (Str.break ",," "a") ("a", "")
  teq (Str.break ",," "abc") ("abc", "")
  teq (Str.break ",," "a,,") ("a", ",,")
  teq (Str.break ",," ",,abc") ("", ",,abc")
  teq (Str.break ",," "1,,abc") ("1", ",,abc")
  teq (Str.break ",," ",,1,,abc") ("", ",,1,,abc")
  teq (Str.break ",," ",,1,,abc,,") ("", ",,1,,abc,,")
  teq (Str.break ",," "z,,1,,abc,,yy") ("z", ",,1,,abc,,yy")

  --putStrLn "  -breakLast"
  teq (Str.breakLast ",," "") ("", "")
  teq (Str.breakLast ",," "a") ("", "a")
  teq (Str.breakLast ",," "abc") ("", "abc")
  teq (Str.breakLast ",," "a,,") ("a,,", "")
  teq (Str.breakLast ",," ",,abc") (",,", "abc")
  teq (Str.breakLast ",," "1,,abc") ("1,,", "abc")
  teq (Str.breakLast ",," ",,1,,abc") (",,1,,", "abc")
  teq (Str.breakLast ",," ",,1,,abc,,") (",,1,,abc,,", "")
  teq (Str.breakLast ",," "z,,1,,abc,,yy") ("z,,1,,abc,,", "yy")

  --putStrLn "  -join"
  teq (Str.join "--" []) ""
  teq (Str.join "--" ["a"]) "a"
  teq (Str.join "--" ["a", "b"]) "a--b"
  teq (Str.join "--" ["a", "b", "cde"]) "a--b--cde"

  --putStrLn "  -replace"
  teq (Str.replace "--" "" "") ""
  teq (Str.replace "--" "" "a") "a"
  teq (Str.replace "--" "" "a--b") "ab"
  teq (Str.replace "--" "" "a--b--c-de") "abc-de"
  teq (Str.replace "--" "+" "") ""
  teq (Str.replace "--" "+" "a") "a"
  teq (Str.replace "--" "+" "a--b") "a+b"
  teq (Str.replace "--" "+" "a--b--c-de") "a+b+c-de"
  teq (Str.replace "--" "+-*" "") ""
  teq (Str.replace "--" "+-*" "a") "a"
  teq (Str.replace "--" "+-*" "a--b") "a+-*b"
  teq (Str.replace "--" "+-*" "a--b--c-de") "a+-*b+-*c-de"
  teq (Str.replace "--" "--" "") ""
  teq (Str.replace "--" "--" "a") "a"
  teq (Str.replace "--" "--" "a--b") "a--b"
  teq (Str.replace "--" "--" "a--b--c-de") "a--b--c-de"

  --putStrLn "  -starts"
  tyes $ Str.starts "" ""
  tyes $ Str.starts "" "a"
  tyes $ Str.starts "" "abc"
  tnot $ Str.starts "a" ""
  tyes $ Str.starts "a" "a"
  tyes $ Str.starts "a" "abc"
  tnot $ Str.starts "c" ""
  tnot $ Str.starts "c" "a"
  tnot $ Str.starts "c" "abc"
  tnot $ Str.starts "ab" ""
  tnot $ Str.starts "ab" "a"
  tyes $ Str.starts "ab" "abc"
  tnot $ Str.starts "abc" ""
  tnot $ Str.starts "abc" "a"
  tyes $ Str.starts "abc" "abc"

  --putStrLn "  -ends"
  tyes $ Str.ends "" ""
  tyes $ Str.ends "" "a"
  tyes $ Str.ends "" "abc"
  tnot $ Str.ends "a" ""
  tyes $ Str.ends "a" "a"
  tnot $ Str.ends "a" "abc"
  tnot $ Str.ends "c" ""
  tnot $ Str.ends "c" "a"
  tyes $ Str.ends "c" "abc"
  tnot $ Str.ends "ab" ""
  tnot $ Str.ends "ab" "a"
  tnot $ Str.ends "ab" "abc"
  tnot $ Str.ends "abc" ""
  tnot $ Str.ends "abc" "a"
  tyes $ Str.ends "abc" "abc"

  --putStrLn "  -ltrim"
  teq (Str.ltrim "") ""
  teq (Str.ltrim "a") "a"
  teq (Str.ltrim "abc") "abc"
  teq (Str.ltrim " ") ""
  teq (Str.ltrim " a") "a"
  teq (Str.ltrim " abc") "abc"
  teq (Str.ltrim "a ") "a "
  teq (Str.ltrim "abc ") "abc "
  teq (Str.ltrim "   ") ""
  teq (Str.ltrim "   a") "a"
  teq (Str.ltrim "   abc") "abc"
  teq (Str.ltrim "a   ") "a   "
  teq (Str.ltrim "abc   ") "abc   "
  teq (Str.ltrim " a ") "a "
  teq (Str.ltrim " abc ") "abc "
  teq (Str.ltrim "   a   ") "a   "
  teq (Str.ltrim "   abc   ") "abc   "

  --putStrLn "  -rtrim"
  teq (Str.rtrim "") ""
  teq (Str.rtrim "a") "a"
  teq (Str.rtrim "abc") "abc"
  teq (Str.rtrim " ") ""
  teq (Str.rtrim " a") " a"
  teq (Str.rtrim " abc") " abc"
  teq (Str.rtrim "a ") "a"
  teq (Str.rtrim "abc ") "abc"
  teq (Str.rtrim "   ") ""
  teq (Str.rtrim "   a") "   a"
  teq (Str.rtrim "   abc") "   abc"
  teq (Str.rtrim "a   ") "a"
  teq (Str.rtrim "abc   ") "abc"
  teq (Str.rtrim " a ") " a"
  teq (Str.rtrim " abc ") " abc"
  teq (Str.rtrim "   a   ") "   a"
  teq (Str.rtrim "   abc   ") "   abc"

  --putStrLn "  -trim"
  teq (Str.trim "") ""
  teq (Str.trim "a") "a"
  teq (Str.trim "abc") "abc"
  teq (Str.trim " ") ""
  teq (Str.trim " a") "a"
  teq (Str.trim " abc") "abc"
  teq (Str.trim "a ") "a"
  teq (Str.trim "abc ") "abc"
  teq (Str.trim "   ") ""
  teq (Str.trim "   a") "a"
  teq (Str.trim "   abc") "abc"
  teq (Str.trim "a   ") "a"
  teq (Str.trim "abc   ") "abc"
  teq (Str.trim " a ") "a"
  teq (Str.trim " abc ") "abc"
  teq (Str.trim "   a   ") "a"
  teq (Str.trim "   abc   ") "abc"

  --putStrLn "  -toUpper"
  teq (Str.toUpper "") ""
  teq (Str.toUpper "a") "A"
  teq (Str.toUpper "abc") "ABC"
  teq (Str.toUpper "A") "A"
  teq (Str.toUpper "ABC") "ABC"
  teq (Str.toUpper "AbCd") "ABCD"

  --putStrLn "  -toLower"
  teq (Str.toLower "") ""
  teq (Str.toLower "a") "a"
  teq (Str.toLower "abc") "abc"
  teq (Str.toLower "A") "a"
  teq (Str.toLower "ABC") "abc"
  teq (Str.toLower "AbCd") "abcd"

  --putStrLn "  -sub (1)"
  teq (Str.sub 0 0 "") ""
  teq (Str.sub 1 0 "") ""
  teq (Str.sub 2 0 "") ""
  teq (Str.sub 5 0 "") ""
  teq (Str.sub (-1) 0 "") ""
  teq (Str.sub (-2) 0 "") ""
  teq (Str.sub (-5) 0 "") ""
  teq (Str.sub 0 1 "") ""
  teq (Str.sub 0 2 "") ""
  teq (Str.sub 0 5 "") ""
  teq (Str.sub 0 (-1) "") ""
  teq (Str.sub 0 (-2) "") ""
  teq (Str.sub 0 (-5) "") ""
  teq (Str.sub 1 1 "") ""
  teq (Str.sub 2 1 "") ""
  teq (Str.sub 5 1 "") ""
  teq (Str.sub (-1) 1 "") ""
  teq (Str.sub (-2) 1 "") ""
  teq (Str.sub (-5) 1 "") ""
  teq (Str.sub 1 2 "") ""
  teq (Str.sub 1 5 "") ""
  teq (Str.sub 1 (-1) "") ""
  teq (Str.sub 1 (-2) "") ""
  teq (Str.sub 1 (-5) "") ""
  teq (Str.sub 2 2 "") ""
  teq (Str.sub 5 2 "") ""
  teq (Str.sub (-1) 2 "") ""
  teq (Str.sub (-2) 2 "") ""
  teq (Str.sub (-5) 2 "") ""
  teq (Str.sub 2 5 "") ""
  teq (Str.sub 2 (-1) "") ""
  teq (Str.sub 2 (-2) "") ""
  teq (Str.sub 2 (-5) "") ""
  teq (Str.sub 5 5 "") ""
  teq (Str.sub (-1) 5 "") ""
  teq (Str.sub (-2) 5 "") ""
  teq (Str.sub (-5) 5 "") ""
  teq (Str.sub 5 (-1) "") ""
  teq (Str.sub 5 (-2) "") ""
  teq (Str.sub 5 (-5) "") ""
  teq (Str.sub (-1) (-1) "") ""
  teq (Str.sub (-2) (-1) "") ""
  teq (Str.sub (-5) (-1) "") ""
  teq (Str.sub (-1) (-2) "") ""
  teq (Str.sub (-1) (-5) "") ""
  teq (Str.sub (-2) (-2) "") ""
  teq (Str.sub (-5) (-2) "") ""
  teq (Str.sub (-2) (-5) "") ""
  teq (Str.sub (-5) (-5) "") ""

  --putStrLn "  -sub (2)"
  teq (Str.sub 0 0 "a") ""
  teq (Str.sub 1 0 "a") ""
  teq (Str.sub 2 0 "a") ""
  teq (Str.sub 5 0 "a") ""
  teq (Str.sub (-1) 0 "a") ""
  teq (Str.sub (-2) 0 "a") ""
  teq (Str.sub (-5) 0 "a") ""
  teq (Str.sub 0 1 "a") "a"
  teq (Str.sub 0 2 "a") "a"
  teq (Str.sub 0 5 "a") "a"
  teq (Str.sub 0 (-1) "a") ""
  teq (Str.sub 0 (-2) "a") ""
  teq (Str.sub 0 (-5) "a") ""
  teq (Str.sub 1 1 "a") ""
  teq (Str.sub 2 1 "a") ""
  teq (Str.sub 5 1 "a") ""
  teq (Str.sub (-1) 1 "a") "a"
  teq (Str.sub (-2) 1 "a") "a"
  teq (Str.sub (-5) 1 "a") "a"
  teq (Str.sub 1 2 "a") ""
  teq (Str.sub 1 5 "a") ""
  teq (Str.sub 1 (-1) "a") ""
  teq (Str.sub 1 (-2) "a") ""
  teq (Str.sub 1 (-5) "a") ""
  teq (Str.sub 2 2 "a") ""
  teq (Str.sub 5 2 "a") ""
  teq (Str.sub (-1) 2 "a") "a"
  teq (Str.sub (-2) 2 "a") "a"
  teq (Str.sub (-5) 2 "a") "a"
  teq (Str.sub 2 5 "a") ""
  teq (Str.sub 2 (-1) "a") ""
  teq (Str.sub 2 (-2) "a") ""
  teq (Str.sub 2 (-5) "a") ""
  teq (Str.sub 5 5 "a") ""
  teq (Str.sub (-1) 5 "a") "a"
  teq (Str.sub (-2) 5 "a") "a"
  teq (Str.sub (-5) 5 "a") "a"
  teq (Str.sub 5 (-1) "a") ""
  teq (Str.sub 5 (-2) "a") ""
  teq (Str.sub 5 (-5) "a") ""
  teq (Str.sub (-1) (-1) "a") ""
  teq (Str.sub (-2) (-1) "a") ""
  teq (Str.sub (-5) (-1) "a") ""
  teq (Str.sub (-1) (-2) "a") ""
  teq (Str.sub (-1) (-5) "a") ""
  teq (Str.sub (-2) (-2) "a") ""
  teq (Str.sub (-5) (-2) "a") ""
  teq (Str.sub (-2) (-5) "a") ""
  teq (Str.sub (-5) (-5) "a") ""

  --putStrLn "  -sub (3)"
  teq (Str.sub 0 0 "abc") ""
  teq (Str.sub 1 0 "abc") ""
  teq (Str.sub 2 0 "abc") ""
  teq (Str.sub 5 0 "abc") ""
  teq (Str.sub (-1) 0 "abc") ""
  teq (Str.sub (-2) 0 "abc") ""
  teq (Str.sub (-5) 0 "abc") ""
  teq (Str.sub 0 1 "abc") "a"
  teq (Str.sub 0 2 "abc") "ab"
  teq (Str.sub 0 5 "abc") "abc"
  teq (Str.sub 0 (-1) "abc") "ab"
  teq (Str.sub 0 (-2) "abc") "a"
  teq (Str.sub 0 (-5) "abc") ""
  teq (Str.sub 1 1 "abc") ""
  teq (Str.sub 2 1 "abc") ""
  teq (Str.sub 5 1 "abc") ""
  teq (Str.sub (-1) 1 "abc") ""
  teq (Str.sub (-2) 1 "abc") ""
  teq (Str.sub (-5) 1 "abc") "a"
  teq (Str.sub 1 2 "abc") "b"
  teq (Str.sub 1 5 "abc") "bc"
  teq (Str.sub 1 (-1) "abc") "b"
  teq (Str.sub 1 (-2) "abc") ""
  teq (Str.sub 1 (-5) "abc") ""
  teq (Str.sub 2 2 "abc") ""
  teq (Str.sub 5 2 "abc") ""
  teq (Str.sub (-1) 2 "abc") ""
  teq (Str.sub (-2) 2 "abc") "b"
  teq (Str.sub (-5) 2 "abc") "ab"
  teq (Str.sub 2 5 "abc") "c"
  teq (Str.sub 2 (-1) "abc") ""
  teq (Str.sub 2 (-2) "abc") ""
  teq (Str.sub 2 (-5) "abc") ""
  teq (Str.sub 5 5 "abc") ""
  teq (Str.sub (-1) 5 "abc") "c"
  teq (Str.sub (-2) 5 "abc") "bc"
  teq (Str.sub (-5) 5 "abc") "abc"
  teq (Str.sub 5 (-1) "abc") ""
  teq (Str.sub 5 (-2) "abc") ""
  teq (Str.sub 5 (-5) "abc") ""
  teq (Str.sub (-1) (-1) "abc") ""
  teq (Str.sub (-2) (-1) "abc") "b"
  teq (Str.sub (-5) (-1) "abc") "ab"
  teq (Str.sub (-1) (-2) "abc") ""
  teq (Str.sub (-1) (-5) "abc") ""
  teq (Str.sub (-2) (-2) "abc") ""
  teq (Str.sub (-5) (-2) "abc") "a"
  teq (Str.sub (-2) (-5) "abc") ""
  teq (Str.sub (-5) (-5) "abc") ""

  --putStrLn "  -left"
  teq (Str.left 0 "") ""
  teq (Str.left 1 "") ""
  teq (Str.left 2 "") ""
  teq (Str.left 5 "") ""
  teq (Str.left (-1) "") ""
  teq (Str.left (-2) "") ""
  teq (Str.left (-5) "") ""
  teq (Str.left 0 "a") ""
  teq (Str.left 1 "a") "a"
  teq (Str.left 2 "a") "a"
  teq (Str.left 5 "a") "a"
  teq (Str.left (-1) "a") ""
  teq (Str.left (-2) "a") ""
  teq (Str.left (-5) "a") ""
  teq (Str.left 0 "abc") ""
  teq (Str.left 1 "abc") "a"
  teq (Str.left 2 "abc") "ab"
  teq (Str.left 5 "abc") "abc"
  teq (Str.left (-1) "abc") "ab"
  teq (Str.left (-2) "abc") "a"
  teq (Str.left (-5) "abc") ""

  --putStrLn "  -right"
  teq (Str.right 0 "") ""
  teq (Str.right 1 "") ""
  teq (Str.right 2 "") ""
  teq (Str.right 5 "") ""
  teq (Str.right (-1) "") ""
  teq (Str.right (-2) "") ""
  teq (Str.right (-5) "") ""
  teq (Str.right 0 "a") "a"
  teq (Str.right 1 "a") ""
  teq (Str.right 2 "a") ""
  teq (Str.right 5 "a") ""
  teq (Str.right (-1) "a") "a"
  teq (Str.right (-2) "a") "a"
  teq (Str.right (-5) "a") "a"
  teq (Str.right 0 "abc") "abc"
  teq (Str.right 1 "abc") "bc"
  teq (Str.right 2 "abc") "c"
  teq (Str.right 5 "abc") ""
  teq (Str.right (-1) "abc") "c"
  teq (Str.right (-2) "abc") "bc"
  teq (Str.right (-5) "abc") "abc"

  --putStrLn "  -Utf8"
  teq ((Str.fromUtf8 . Str.toUtf8) "cañón") "cañón"

  putStrLn "  Finished"
