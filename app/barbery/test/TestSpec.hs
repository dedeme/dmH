-- Copyright 02-Feb-2020 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

module TestSpec (testTest) where

import Dm.Test


testTest :: IO ()
testTest = do
  putStrLn "Test test"

  teq "a" "a"

  putStrLn "  Finished"
