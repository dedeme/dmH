module LibSpec (libTest) where

import qualified Lib.Lib as Lib
import Control.Exception.Base


libTest :: IO ()
libTest = do
  putStrLn "Lib test"
  putStrLn $
    (assert (Lib.someFunc == "someFunc") "") ++
    (assert (Lib.someFunc /= "someFuncx") "") ++
    "    Finished"
