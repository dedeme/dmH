-- Copyright 13-Nov-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Bests study

module Bests (study) where

import Dm.Quote (Quote)
import qualified Dm.Quote as Q
import Io (Qdata(..))

study :: Qdata -> IO ()
study (Qdata qs qdays qnicks) = do
  return ()
