-- Copyright 13-Nov-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Main module

module Main where

import System.Environment
import qualified Io  as Io
import qualified Selector as Selector
import qualified Bests as Bests

help :: IO ()
help = putStrLn $
  "fleas. v201810\n" ++
  "\n" ++
  "Usage:\n" ++
  "  fleas\n" ++
  "    Calulates only one cycle\n" ++
  "    (e.g. fleas)\n" ++
  "  fleas t <time_minutes>\n" ++
  "    Cycling <time_minutes>\n" ++
  "    (e.g. fleas t 3)\n" ++
  "  fleas clean\n" ++
  "    Delete all data and set cycle to 0\n" ++
  "    (e.g. fleas clean)\n" ++
  "  fleas stop\n" ++
  "    Send a stop signal when fleas was called with 'fleas t'\n" ++
  "    (e.g. fleas stop)\n" ++
  "  fleas unlock\n" ++
  "    Remove the lock used to control double runs\n" ++
  "    (e.g. fleas unlock)\n" ++
  "  fleas help\n" ++
  "    Show this message\n" ++
  "    (e.g. fleas help)\n"

ifs :: [(Bool, IO ())] -> IO ()
ifs [] = return ()
ifs ((v, act):xs) = if v then act else ifs xs

main :: IO ()
main = do
  Io.homeInit
  args <- getArgs
  let unlock = args == ["unlock"]
  let stop = args == ["stop"]
  lock <- Io.lock
  ifs [
      (unlock, do
        Io.unlock
        return ()),
      (stop, do
        Io.sendStop
        return ()),
      (lock, return ()),
      (True, do
        qdata <- Io.dataInit
--        print $ length $ Io.quotes qdata
        Selector.select qdata
        Bests.study qdata
        help
        Io.unlock)
    ]

