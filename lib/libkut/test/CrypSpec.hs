-- Copyright 27-Jan-2020 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

{-# LANGUAGE OverloadedStrings #-}

module CrypSpec (crypTest) where

import qualified Kut.Cryp as Cryp
import qualified Kut.Str as Str
import qualified Data.ByteString as Bs

import Kut.Test

crypTest :: IO ()
crypTest = do
  putStrLn "Cryp test"

  k <- Cryp.genk 6
  teq (Bs.length k) 6
  teq (Cryp.key "deme" 6) "wiWTB9"
  teq (Cryp.key "Generaro" 5) "Ixy8I"
  teq (Cryp.key "Generara" 5) "0DIih"
  teq (Cryp.key "çaño" 8) "znS2qAbK"
  teq (Cryp.cryp "01" "abc") "s7t0bQ=="
  teq (Cryp.cryp (Str.toUtf8 "Cañón€%ç") "deme") "v12ftuzYeq2Xz7q7tLe8tNnHtqY="
  tyes $ crDcr "01" "abc"
  tyes $ crDcr "11" "abcd"
  tyes $ crDcr "" "abc"
  tyes $ crDcr "a" "c"
  tyes $ crDcr "ab c" "xxx"
  tyes $ crDcr "\n\ta€b c" "abc"

  putStrLn "  Finished"
  where
    crDcr tx k = Str.fromUtf8 (Cryp.decryp (Cryp.cryp (Str.toUtf8 tx) k) k)
                 == tx
