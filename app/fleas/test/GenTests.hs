module GenTests (genTests) where

import qualified Gen as Gen
import qualified Global as G
import Control.Exception.Base

alist :: [(Bool -> String -> String, Bool)] -> String
alist ls = foldl (\acc (f, v) ->  (f v "") ++ acc) "" ls

genTests :: IO ()
genTests = do
  let g = Gen.new
  g1 <- Gen.mutate g
  let days = Gen.realDays g1
  let bstr = Gen.buyStrip g1
  let sstr = Gen.sellStrip g1
  putStrLn "Gen test"
  putStrLn $ alist [
    (assert, g == Gen.fromJs (Gen.toJs g)),
    (assert, g /= g1),
    (assert, days >= G.minDays && days < G.maxDays),
    (assert, bstr >= 0 && bstr < G.maxStrip),
    (assert, sstr >= 0 && sstr < G.maxStrip)
    ] ++
    "    Finished"
