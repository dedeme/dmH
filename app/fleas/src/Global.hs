-- Copyright 13-Nov-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Global constans

module Global (
  home,
  quotesDataPath,
  cycles,
  fleasPerGroup,
  daysStandBy,
  maxDays,
  minDays,
  maxStrip,
  daysFirstGroup,
  daysNextGroup,
  initialCapital,
  bet,
  betPlusFees,
  minSells,
  maxSells,
  mutationMultiplier
  ) where

import qualified Dm.File as File
import Dm.File ((</>))
import qualified Dm.Fees as Fees

-- | Home path
home :: IO String
home = do
  h <- File.home
  return $ h </> ".dmHApp/fleas"

-- | Quotes data path
quotesDataPath :: String
quotesDataPath = "/dm/wwwcgi/dmcgi/Quotes/data"

-- | Number of cycle to finish a process
cycles :: Int
cycles = 150

-- | Number of fleas per days group
fleasPerGroup :: Int
fleasPerGroup = 2000

-- | Days waiting before operating
daysStandBy :: Int
daysStandBy = 125

-- | Max days for calculations
maxDays :: Int
maxDays = 120

-- | Min days for calculations
minDays :: Int
minDays = 20

-- | Max strip for caculations
maxStrip :: Double
maxStrip = 0.2

-- | Quotes days for the first group of fleas
daysFirstGroup :: Int
daysFirstGroup = 350

-- | Quotes days for the next group of fleas
daysNextGroup :: Int
daysNextGroup = 100

-- | Fleas initial capital for each cycle
initialCapital :: Double
initialCapital = 150000.0

-- | Bet
bet :: Double
bet = 15000.0

-- | Bet + fees
betPlusFees :: Double
betPlusFees = bet + Fees.app bet

-- | Minimum operations to survive (divisor: (days - maxDays) / MinSells)
minSells :: Int
minSells = 9

-- | Maximun operations to survive (divisor: (days - maxDays) / maxSells)
maxSells :: Int
maxSells = 6

-- | Range of mutation
mutationMultiplier :: Double
mutationMultiplier = 0.3

