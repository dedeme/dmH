-- Copyright 13-Nov-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Best fleas selector

module Selector (select) where

import Debug.Trace
import Text.Printf
import qualified Data.List as List
import Dm.Quote (Quote)
import qualified Dm.Quote as Q
import Io (Qdata(..), stop, writeGroup)
import qualified Global as G
import Flea

cycling :: [Fresult] -> Qdata -> Int -> Int -> IO [Fresult]
cycling rss qdata gr cycle = do
  rss0 <- case rss of
    [] -> do
      fl <- (fleaNew "0")
      return [Fresult fl 0 0 0]
    r -> return r
  let newN = G.fleasPerGroup - length rss0
  let base = take newN $ List.cycle rss0
  (rss1, _) <- List.foldl' addRss (return (rss, 0)) base
  let (rss2, sm, n) = List.foldl' avgFilter ([], 0, 0) rss1
  let avg = sm / n
  let rss3 = filter (\(Fresult _ assets _ _) -> assets > avg) rss2
  putStrLn $ printf "Group: %d, Cycle: %d, Avg: %.2f\n" gr cycle avg
  return rss3
  where
  maxSells = round $ fromIntegral (gr - G.maxDays) / fromIntegral G.maxSells
  minSells = round $ fromIntegral (gr - G.minDays) / fromIntegral G.minSells
  addRss ioRss (Fresult fl _ _ _) = do
    (rss, n) <- ioRss
    fl' <- fleaMutate ((show cycle) ++ "-" ++ (show n)) fl
    let r = fleaProcess (Io.quotes qdata) (Io.qnicks qdata) fl'
    return (r:rss, n + 1)
  avgFilter (ls, sm, n) rs@(Fresult _ assets _ sells) =
    if sells >= minSells && sells <= maxSells
    then (rs:ls, sm + assets, n + 1)
    else (ls, sm, n)

group :: [Fresult] -> Qdata -> Int -> Int -> IO Bool
group rss qdata gr cycle
  | cycle > G.cycles = do
      Io.writeGroup (show gr) rss
      return True
  | otherwise = do
      rss' <- cycling rss qdata gr cycle
      stop <- Io.stop
      if stop
      then return False
      else group rss' qdata gr (cycle + 1)

groups :: Qdata -> [Int] -> IO ()
groups qdata [] = return ()
groups qdata (g:grs) = do
  cont <- group [] qdata g 1
  if cont
  then groups qdata grs
  else return ()

select :: Qdata -> IO ()
select qdata@(Qdata _ days _) = groups qdata list
  where
  list = takeWhile (\n -> n < days) $
    map (\n -> G.daysFirstGroup + n * G.daysNextGroup) [0..]


