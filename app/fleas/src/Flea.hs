-- Copyright 14-Nov-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Flea and Fresult types

module Flea (
  Flea,
  fleaNew,
  fleaInit,
  fleaMutate,
  fleaToJs,
  fleaFromJs,
  Fresult (..),
  rsToJs,
  rsFromJs,
  fleaProcess
  ) where

import Debug.Trace
import Data.Array
import qualified Dm.Quote as Q
import Dm.Quote (Quote)
import qualified Dm.Pf as Pf
import qualified Dm.Sell as Sell
import qualified Dm.Buy as Buy
import qualified Dm.Js as Js
import Dm.Js (JSValue)
import qualified Gen as Gen
import Gen (Gen)
import qualified Co as Co
import Co (Co)
import qualified Global as G
import qualified BuysSorter as Bs

-- Flea --------------------------------------------------------------

-- | Flea type
data Flea = Flea {
  id :: String,
  gen :: Gen,
  cos :: [Co]
} deriving (Show)

-- | @'fleaNew' id@ - Creates a new Flea
fleaNew :: String -> IO Flea
fleaNew id = do
  g <- Gen.mutate $ Gen.new
  return $ Flea id g []

-- | @'fleaInit' companiesNumber fl@ - Initializes a /fl/ at the beginning of
--                                     'fleaProcess'.
fleaInit :: Int -> Flea -> Flea
fleaInit companiesNumber (Flea id gen _) =
  Flea id gen $ take companiesNumber $ repeat $ Co.new $ Gen.realDays gen

-- | @'fleaMutate' fl@ - Creates a new Flea from an old one.
fleaMutate :: String -> Flea -> IO Flea
fleaMutate id (Flea _ gen cos) = Gen.mutate gen >>= \g -> return $ Flea id g []

-- | @'fleaToJs' fl@ - Parses /fl/ to JSON.
fleaToJs :: Flea -> JSValue
fleaToJs (Flea id gen _) = Js.wList [Js.wString id, Gen.toJs gen]

-- | @'fleaFromJs' js@ - Retrieves a Flea JSONized.
fleaFromJs :: JSValue -> Flea
fleaFromJs js = let [id, gen] = Js.rList js
                in  Flea (Js.rString id) (Gen.fromJs gen) []

-- Fresult -----------------------------------------------------------

-- | Result of a cycle
data Fresult = Fresult {
  -- | @'rsFlea' rs@ - Returns the flea of /rs/
  rsFlea :: Flea,
  -- | @'rsAssets' rs@ - Returns the assets of /rs/
  rsAssets :: Double,
  -- | @'rsBuys' rs@ - Returns the number of buys of /rs/
  rsBuys :: Int,
  -- | @'rsSells' rs@ - Returns the number of sells of /rs/
  rsSells :: Int
} deriving (Show)

-- | @'rsToJs' rs@ - Parses /rs/ to JSON.
rsToJs :: Fresult -> JSValue
rsToJs (Fresult fl a b s) =
  Js.wList [fleaToJs fl, Js.wDouble a, Js.wInt b, Js.wInt s]

-- | @'rsFromJs' js@ - Retrieves a Fresult JSONized.
rsFromJs :: JSValue -> Fresult
rsFromJs js = let [fl, a, b, s] = Js.rList js
              in  Fresult (fleaFromJs fl) (Js.rDouble a)
                          (Js.rInt b) (Js.rInt s)

-- process -----------------------------------------------------------

process1 :: Array Int Quote -> Flea -> Flea
process1 qs (Flea id gen cos)  =
  let cos' = map add $ zip (elems qs) cos
  in  Flea id gen cos'
  where
    add (q, co) = let close = Q.close q
                      (co0, operate) = Co.add close co
                      (co1, _) = if operate
                                 then if Co.buying co
                                      then Co.up (Gen.buyStrip gen) co0
                                      else Co.down (Gen.sellStrip gen) co0
                                 else (co0, 0)
                  in co1

prSells :: Array Int Quote -> [Int] -> Double -> Pf.Pf -> Int ->
           (Int, Double, Pf.Pf, [Int])
prSells aqs sells cash pf nsells = prSells' sells [] cash pf nsells
  where
  prSells' [] sells' cash pf nsells = (nsells, cash, pf, sells')
  prSells' (sell:rest) sells' cash pf nsells = case Pf.get pf sell of
    Nothing -> prSells' rest sells' cash pf nsells
    Just (stocks, _) ->
      let open = Q.open (aqs ! sell)
      in  if open > 0
          then
            let income = Sell.income stocks open
                pf' = Pf.remove sell pf
            in  prSells' rest sells' (cash + income) pf' (nsells + 1)
          else
            prSells' rest (sell:sells') cash pf nsells

prBuys :: Array Int Quote -> [Int] -> Double -> Pf.Pf -> Int ->
           (Int, Double, Pf.Pf)
prBuys aqs buys cash pf nbuys = prBuys' buys cash pf nbuys
  where
  prBuys' [] cash pf nbuys = (nbuys, cash, pf)
  prBuys' (buy:rest) cash pf nbuys =
    let open = Q.open (aqs ! buy)
    in  if open > 0
        then
          let (stocks, cost) = Buy.cost G.bet open
              pf' = Pf.add buy stocks open pf
          in  prBuys' rest (cash - cost) pf' (nbuys + 1)
        else
          prBuys' rest cash pf nbuys

prOperations :: Array Int Quote -> Flea -> Bs.BuysSorter -> [Int] ->
                (Flea, Bs.BuysSorter, [Int])
prOperations qs (Flea id gen cos) bs ss =
  let (cos', _, bs', ss') = foldr add ([], 0, bs, ss) $ zip (elems qs) cos
  in  (Flea id gen cos', bs', ss')
  where
    add (q, co) (cos, nick, bs, ss) =
      let close = Q.close q
          (co0, operate) = Co.add close co
          (co1, p) = if operate
                     then if Co.buying co
                          then Co.up (Gen.buyStrip gen) co0
                          else Co.down (Gen.sellStrip gen) co0
                     else (co0, -1)
      in  if p >= 0
          then
            if Co.buying co
            then (co1:cos, (nick + 1), Bs.add nick p bs, ss)
            else (co1:cos, (nick + 1), bs, nick:ss)
          else (co1:cos, (nick + 1), bs, ss)

process2 :: Array Int Quote -> Int -> Flea -> [Int] ->  [Int] -> Double ->
            Pf.Pf -> Int -> Int ->
            (Flea, [Int], [Int], Double, Pf.Pf, Int, Int)
process2 aqs coN fl buys sells cash pf nbuys nsells =
  let (nsells', cash1, pf1, sells1) = prSells aqs sells cash pf nsells
      (nbuys', cash2, pf2) = prBuys aqs buys cash1 pf1 nbuys
      buysSorter = Bs.new $ truncate (cash2 / G.betPlusFees)
      (fl', buysSorter', sells') = prOperations aqs fl buysSorter sells1
  in  (fl', Bs.list buysSorter', sells', cash2, pf2, nbuys', nsells')

settlement :: Array Int Quote -> Double -> Pf.Pf -> Double
settlement qs cash pf = cash + (sum $ map fval (Pf.nicks pf))
  where
  fval nk = case Pf.get pf nk of
            Just (st, pr) ->
              let lastPrice = Q.close (qs ! nk)
              in  if lastPrice < 0 then fromIntegral st * pr
                                   else fromIntegral st * lastPrice
            Nothing -> error ((show nk) ++ " missing in portfolio.")

-- | @'process' quotes fleas@ - Processes a cycle
fleaProcess :: [Array Int Quote] -> Int -> Flea -> Fresult
fleaProcess qs companiesNumber fl =
  let (qs', fl') = cycleInit qs (fleaInit companiesNumber fl) 0 G.daysStandBy
  in  cycleProcess qs' fl' [] [] G.initialCapital Pf.new 0 0
  where
  cycleInit qs fl days maxDays = cInit qs fl days
    where
    cInit qs@(aqs:rest) fl days
      | days == maxDays = (qs, fl)
      | otherwise = cInit rest (process1 aqs fl) (days + 1)

  cycleProcess [] fl buys sells cash pf nbuys nsells =
    Fresult fl (settlement (last qs) cash pf) nbuys nsells
  cycleProcess (dqs:rest) fl buys sells cash pf nbuys nsells =
    let (fl', buys', sells', cash', pf', nbuys', nsells') =
          process2 dqs companiesNumber fl buys sells cash pf nbuys nsells
    in  cycleProcess rest fl' buys' sells' cash' pf' nbuys' nsells'
