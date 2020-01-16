-- Copyright 14-Nov-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Company closes data
--
--  Working:
--
--  (1) Before process closes 'new' must be called.
--  2. Every day 'add' must be called. It returns a new 'Co' and /True/ if
--     there are valid values to call 'up' or 'down'. (There are not values ==
--     -1)
--  3. After /2/ 'up' or 'down' must be called if 'buying' is /True/ or /False/
--     repectively.

module Co (
  Co(..),
  new,
  add,
  up,
  down
  )where

import Debug.Trace

data Co = Co {
  closes :: [Double],
  lastClose :: Double,
  ref :: Double,
  buying :: Bool
  } deriving (Show)

-- | @'new' days@ - Creates a new 'Co' at the beginning of each cycle.
new :: Int -> Co
new days = Co (take days (repeat (-1))) (-1) (-1) False

-- | @'add' close co@ - Adds a new close and returns a new 'Co' and /True/ if
--                      there are valid values to call 'up' or 'down'. (There
--                      are not values == -1)
add :: Double -> Co -> (Co, Bool)
add close (Co closes _ ref buying) =
  let cls@(begin:_) = (tail closes) ++ [close]
  in  if ref > 0
      then let newref = if buying then if begin < ref then begin else ref
                                  else if begin > ref then begin else ref
           in  (Co cls close newref buying, close > 0)
      else if begin > 0 then (Co cls close begin buying, close > 0)
                        else (Co cls close ref buying, False)

-- | @'up' strip co@ - Returns a new 'Co' if 'lastClose' is greater than
--                     /strip/ and its difference value or (/co/, -1)
up :: Double -> Co -> (Co, Double)
up strip co@(Co closes@(begin:_) lastClose ref _) =
  let dif = (lastClose - ref) / ref
  in  if dif > strip
      then (Co closes lastClose begin False, dif / strip)
      else (co, -1)

-- | @'down' strip co@ - Returns a new 'Co' if 'lastClose' is less than
--                       /strip/ and its difference value or (/co/, -1)
down :: Double -> Co -> (Co, Double)
down strip co@(Co closes@(begin:_) lastClose ref _) =
  let dif = (ref - lastClose) / ref
  in  if dif > strip
      then (Co closes lastClose begin True, dif / strip)
      else (co, -1)

