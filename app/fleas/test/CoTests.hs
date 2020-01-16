module CoTests (coTests) where

import qualified Co as Co
import Control.Exception.Base

alist :: [(Bool -> String -> String, Bool)] -> String
alist ls = foldl (\acc (f, v) ->  (f v "") ++ acc) "" ls

coTests :: IO ()
coTests = do
  putStrLn "Co test"
  let co1 = Co.new 3
  let (co2, r2) = Co.add 15 co1
  let (co3, r3) = Co.add 14 co2
  let (co4, r4) = Co.add 12 co3
  let (co5, r5) = Co.add (-1) co4
  let (co6, r6) = Co.add 16 co5
  let (co7, r7) = Co.add 14 co6
  let (co8, r8) = Co.add 14 co7
  let (co9, r9) = Co.add 13 co8
  let (co10, r10) = Co.add 10 co9
  let (co11, d11) = Co.down 0.2 co10
  let (co12, r12) = Co.add 12 co11
  let (co13, r13) = Co.add 12 co12
  let (co14, r14) = Co.add 15 co13
  let (co15, d15) = Co.up 0.2 co14

  putStrLn $ alist [
    (assert, Co.lastClose co1 == -1),
    (assert, Co.ref co1 == -1),
    (assert, not $ Co.buying co1),
    (assert, not r2),
    (assert, Co.lastClose co2 == 15),
    (assert, Co.ref co2 == -1),
    (assert, not $ Co.buying co2),
    (assert, not r3),
    (assert, Co.lastClose co3 == 14),
    (assert, Co.ref co3 == -1),
    (assert, not $ Co.buying co3),
    (assert, r4),
    (assert, Co.lastClose co4 == 12),
    (assert, Co.ref co4 == 15),
    (assert, not $ Co.buying co4),
    (assert, not r5),
    (assert, Co.lastClose co5 == -1),
    (assert, Co.ref co5 == 15),
    (assert, not $ Co.buying co5),
    (assert, r6),
    (assert, Co.lastClose co6 == 16),
    (assert, Co.ref co6 == 15),
    (assert, not $ Co.buying co6),
    (assert, r7),
    (assert, Co.lastClose co7 == 14),
    (assert, Co.ref co7 == 15),
    (assert, not $ Co.buying co7),
    (assert, r8),
    (assert, Co.lastClose co8 == 14),
    (assert, Co.ref co8 == 16),
    (assert, not $ Co.buying co8),
    (assert, r9),
    (assert, Co.lastClose co9 == 13),
    (assert, Co.ref co9 == 16),
    (assert, not $ Co.buying co9),
    (assert, r10),
    (assert, Co.lastClose co10 == 10),
    (assert, Co.ref co10 == 16),
    (assert, not $ Co.buying co10),
    (assert, let (_, df) = Co.down 0.5 co10 in df < 0),
    (assert, let (_, df) = Co.down 0.25 co10 in df > 0),
    (assert, let (_, df1) = Co.down 0.20 co10
                 (_, df2) = Co.down 0.25 co10
             in  df1 > df2),
    (assert, d11 > 0),
    (assert, Co.lastClose co11 == 10),
    (assert, Co.ref co11 == 14),
    (assert, Co.buying co11),
    (assert, r12),
    (assert, Co.lastClose co12 == 12),
    (assert, Co.ref co12 == 13),
    (assert, Co.buying co12),
    (assert, r13),
    (assert, Co.lastClose co13 == 12),
    (assert, Co.ref co13 == 10),
    (assert, Co.buying co13),
    (assert, r14),
    (assert, Co.lastClose co14 == 15),
    (assert, Co.ref co14 == 10),
    (assert, Co.buying co14),
    (assert, let (_, df) = Co.up 0.5 co14 in df < 0),
    (assert, let (_, df) = Co.up 0.25 co14 in df > 0),
    (assert, let (_, df1) = Co.up 0.20 co14
                 (_, df2) = Co.up 0.25 co14
             in  df1 > df2),
    (assert, d15 > 0),
    (assert, Co.lastClose co15 == 15),
    (assert, Co.ref co15 == 12),
    (assert, not $ Co.buying co15)
    ] ++
    "    Finished"
