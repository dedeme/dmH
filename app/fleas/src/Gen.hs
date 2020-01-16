-- Copyright 14-Nov-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Gen type

module Gen (
  Gen,
  days,
  buy,
  sell,
  realDays,
  buyStrip,
  sellStrip,
  new,
  mutate,
  toJs,
  fromJs
  )where

import Data.Array
import qualified Dm.Rnd as Rnd
import qualified Global as G
import qualified Dm.Js as Js
import Dm.Js (JSValue)

data Gen = Gen {
  -- | @'days' g@ - days
  days :: Double,
  -- | @'buy' g@ - buy strip
  buy :: Double,
  -- | @'sell' g@ - sell strip
  sell :: Double
} deriving (Eq, Show)

-- | @'new'@ - Creates a new Gen with values 0.5 0.5 0.5
new :: Gen
new = Gen 0.5 0.5 0.5

-- | @'mutate' g@ - Mutates /g/
mutate :: Gen -> IO Gen
mutate (Gen d b s) = do
  d' <- mt d
  b' <- mt b
  s' <- mt s
  return $ Gen d' b' s'
  where
  mt n = do
    rnd <- Rnd.d
    let mul = G.mutationMultiplier * (rnd * 2 - 1)
    if mul <= 0
    then return $ n * (1 + mul)
    else return $ n + (1 - n) * mul

-- | @'realDays' g@ - is equals to:
--
-- @
-- days g * ('G.maxDays' - 'G.minDays') + 'G.minDays'
-- @
realDays :: Gen -> Int
realDays g = truncate (days g * (fromIntegral (G.maxDays - G.minDays))) +
  G.minDays

-- | @'buyStrip' g@ - is equals to:
--
-- @
-- buy g * 'G.maxStrip'
-- @
buyStrip :: Gen -> Double
buyStrip g = buy g * G.maxStrip

-- | @'sellStrip' g@ - is equals to:
--
-- @
-- sell g * 'G.maxStrip'
-- @
sellStrip :: Gen -> Double
sellStrip g = sell g * G.maxStrip

-- | @'toJs' g@ - Parses /g/ to JSON.
toJs :: Gen -> JSValue
toJs (Gen d b s) = Js.wList [Js.wDouble d, Js.wDouble b, Js.wDouble s]

-- | @'fromJs' js@ - Retrieves a Gen JSONized.
fromJs :: JSValue -> Gen
fromJs js = let [d, b, s] = Js.rList js
            in  Gen (Js.rDouble d) (Js.rDouble b) (Js.rDouble s)
