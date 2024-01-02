-- Copyright 01-Jan-2024 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

module B64Spec (b64Test) where

import qualified Kut.Str as Str
import qualified Kut.B64 as B64
import qualified Data.ByteString as Bs

import Kut.Test

b64Test :: IO ()
b64Test = do
  putStrLn "B64 test"

  let b64 = B64.encode (Str.toUtf8 "Cañónç䍆")
  let b640 = B64.encode (Str.toUtf8 "")
  let bss = Bs.pack [120..129]

  teq (Str.fromUtf8 b64) "Q2HDscOzbsOn5I2G"
  teq (Str.fromUtf8 b640) ""
  teq (Str.fromUtf8 (B64.decode b64)) "Cañónç䍆"
  teq (Str.fromUtf8 (B64.decode b640)) ""
  teq (B64.decode (B64.encode bss)) bss

  putStrLn "  Finished"
