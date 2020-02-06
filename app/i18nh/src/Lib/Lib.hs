module Lib.Lib
    ( someFunc
    ) where

import qualified Dm.B64 as B64

someFunc :: String
someFunc = case B64.decode $ B64.encode "someFunc" of
            Left _ -> "Error"
            Right s -> s
