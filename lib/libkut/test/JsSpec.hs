-- Copyright 01-Jan-2024 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

module JsSpec (jsTest) where

import qualified Kut.Js as Js
import qualified Kut.Map as Map
--import qualified Kut.Sys as Sys

import Kut.Test

jsTest :: IO ()
jsTest = do
  putStrLn "Js test"
{-
  teq ((Js.rb . Js.wb) True) True
  teq ((Js.rb . Js.wb) False) False
  teq ((Js.ri . Js.wi) 12) 12
  teq ((Js.rd . Js.wd) (-12.4)) (-12.4)
  teq ((Js.rs . Js.ws) "") ""
  teq ((Js.rs . Js.ws) "abcñ\n") "abcñ\n"
  teq ((Js.ra . Js.wa) []) []
  teq ((Js.ra . Js.wa) [Js.wi 1, Js.ws "a"]) [Js.wi 1, Js.ws "a"]
  teq ((Js.ro . Js.wo) []) []
  teq ((Js.ro . Js.wo) [("1", Js.wi 1), ("2", Js.ws "a")])
                       [("1", Js.wi 1), ("2", Js.ws "a")]

  tyes $ Js.rb (" \b true \n ")
  tnot $ Js.rb (" \b false \n ")
  Sys.catch
    (tnot $ Js.rb (" \b faxlse \n "))
    (\_ -> return ())

  --putStrLn "  - Bool"
  teq (Js.wb True) "true"
  let rB = Js.ra "\n[ true ] "
  tyes $ Js.rb $ head rB


  --putStrLn "  - Int"
  teq (Js.wi (-32)) "-32"
  let rI = Js.ra "\n[ -112 ] "
  teq (Js.ri $ head rI) (-112)

  --putStrLn "  - Double"
  teq (Js.wd (3.20)) "3.2"
  let rD = Js.ra "\n[ -1.02 ] "
  teq (Js.rd $ head rD) (-1.02)

  --putStrLn "  - String"
  teq (Js.ws "") "\"\""
  let rS0 = Js.ra "\n[ \"\" ] "
  teq (Js.rs $ head rS0) ""
  teq (Js.ws "\tcañón\t") "\"\\tcañón\\t\""
  let rS1 = Js.ra "\n[ \"\\tcañón\\t\" ] "
  teq (Js.rs $ head rS1) "\tcañón\t"

  --putStrLn "  - Simple Array"
  teq (Js.wa []) "[]"
  let rSA0 = Js.ra "\n[ [] ] "
  teq (Js.ra $ head rSA0) []
  teq (Js.wa [Js.wi 1]) "[1]"
  let rSA1 = Js.ra "\n[ [1] ] "
  teq (Js.ri $ head (Js.ra $ head rSA1)) 1
  teq (Js.wa [Js.wi 1, Js.ws "ab"]) "[1,\"ab\"]"
  let rSA2 = Js.ra "\n[ [1, \"ab\"] ] "
  teq (Js.ri $ head (Js.ra $ head rSA2)) 1
  teq (Js.rs $ head $ tail (Js.ra $ head rSA2)) "ab"

  --putStrLn "  - Simple Object"
  teq (Js.wo []) "{}"
  let rSO0 = Js.ra "\n[ {} ] "
  teq (Js.ro $ head rSO0) []
  teq (Js.wo [("one", Js.wi 1)]) "{\"one\":1}"
  let rSO1 = Js.ra "\n[ {\"one\":1} ] "
  teq (Js.ri $ Map.getOk "one" (Js.ro $ head rSO1)) 1
  teq (Js.wo [("one", Js.wi 1), ("two", Js.ws "ab")]) "{\"one\":1,\"two\":\"ab\"}"
  let rSO2 = Js.ra "\n[ {\"one\":1,\"two\":\"ab\"} ] "
  teq (Js.ri $ Map.getOk "one" (Js.ro $ head rSO2)) 1
  teq (Js.rs $ Map.getOk "two" (Js.ro $ head rSO2)) "ab"
-}
  --putStrLn "  - Complex Object"
  let o = Js.wo [
            ("isFoo", Js.wb True),
            ("students",
              Js.wa [
                      Js.wo [
                              ("name", Js.ws "Jane"),
                              ("age", Js.wi 18)
                            ],
                      Js.wo [
                              ("name", Js.ws "Peter"),
                              ("age", Js.wi 29)
                            ]
                    ]),
            ("teacher",
              Js.wo [
                      ("name", Js.ws "Ángel Yáñez"),
                      ("age", Js.wi 115)
                    ])
          ]
  teq o $ "{\"isFoo\":true," ++
           "\"students\":" ++
           "[{\"name\":\"Jane\",\"age\":18},{\"name\":\"Peter\",\"age\":29}]," ++
           "\"teacher\":{\"name\":\"Ángel Yáñez\",\"age\":115}}"

  let allData = Js.ro o
  tyes (Js.rb (Map.getOk "isFoo" allData))
  let stds = Js.ra $ Map.getOk "students" allData
  let st1 = Js.ro $ head stds
  teq (Js.rs (Map.getOk "name" st1)) "Jane"
  teq (Js.ri (Map.getOk "age" st1)) 18
  let st2 = Js.ro $ head $ tail stds
  teq (Js.rs (Map.getOk "name" st2)) "Peter"
  teq (Js.ri (Map.getOk "age" st2)) 29
  let tch = Js.ro $ Map.getOk "teacher" allData
  teq (Js.rs (Map.getOk "name" tch)) "Ángel Yáñez"
  teq (Js.ri (Map.getOk "age" tch)) 115

  putStrLn "  Finished"
