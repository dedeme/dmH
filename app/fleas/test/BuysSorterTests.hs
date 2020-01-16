module BuysSorterTests (buysSorterTests) where

import qualified BuysSorter as Bs
import Control.Exception.Base

alist :: [(Bool -> String -> String, Bool)] -> String
alist ls = foldl (\acc (f, v) ->  (f v "") ++ acc) "" ls

buysSorterTests :: IO ()
buysSorterTests = do
  putStrLn "BuysSorter test"
  let b0 = Bs.new 0
  let bs01 = ((Bs.add 0 3) . (Bs.add 1 0) . (Bs.add 2 2) . (Bs.add 3 1))  b0
  let b1 = Bs.new 3
  let bs11 = ((Bs.add 0 3) . (Bs.add 1 0) . (Bs.add 2 2) . (Bs.add 3 1))  b1
  let b2 = Bs.new 1
  let bs21 = ((Bs.add 0 3) . (Bs.add 1 0) . (Bs.add 2 2) . (Bs.add 3 1))  b2
  let bs22 = ((Bs.add 0 2) . (Bs.add 1 0) . (Bs.add 2 2) . (Bs.add 3 3))  b2

  putStrLn $ alist [
    (assert, Bs.list bs01 == []),
    (assert, Bs.list bs11 == [0, 2, 3]),
    (assert, Bs.list bs21 == [0]),
    (assert, Bs.list bs22 == [3])
    ] ++
    "    Finished"
