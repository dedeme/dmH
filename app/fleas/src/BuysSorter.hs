-- Copyright 15-Nov-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Sorter of buy nicks by ponderations

module BuysSorter (
  BuysSorter,
  new,
  add,
  list
  ) where

-- | BuysSorter type
data BuysSorter = BuysSorter {
  buys :: [(Int, Double)], -- [(nick, ponderation)]
  maxSize :: Int,
  currentSize :: Int
}

-- | @'new' size@ - Creates a BuysSorter of /size/ elements
new :: Int -> BuysSorter
new size = BuysSorter [] size 0

-- | @'add' nick ponderation bs@ - Adds a new entry to /bs/
add :: Int -> Double -> BuysSorter -> BuysSorter
add nick ponderation bs@(BuysSorter buys max current)
  | max > current = BuysSorter ((nick, ponderation):buys) max (current + 1)
  | otherwise = BuysSorter (ins nick ponderation buys) max current
  where
  ins nk p [] = []
  ins nk p ((nk', p'):rest)
    | p > p' = (nk, p):(ins nk' p' rest)
    | otherwise = (nk', p'):(ins nk p rest)

-- | @'list' bs@ - Retuns a list with selected nicks.
list :: BuysSorter -> [Int]
list (BuysSorter buys _ _) = map (\(nk, _) -> nk) buys
