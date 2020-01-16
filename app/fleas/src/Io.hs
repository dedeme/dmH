-- Copyright 13-Nov-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Io operations

module Io (
  Qdata(..),
  homeInit,
  dataInit,
  lock,
  unlock,
  sendStop,
  stop,
  writeGroup
  ) where

import Data.Array
import qualified Data.List as List
import qualified Data.ByteString as Bs
import qualified Data.ByteString.UTF8 as U8
import qualified Dm.File as File
import qualified Dm.Js as Js
import qualified Dm.Date as Date
import qualified Dm.Quote as Q
import Dm.File ((</>), ByteString)
import Dm.Quote (Quote)
import qualified Global as G
import Flea

data Qdata = Qdata {quotes :: [Array Int Quote], qdays :: Int, qnicks :: Int}

homeInit :: IO ()
homeInit = G.home >>= File.mkDir

dataInit :: IO (Qdata)
dataInit = do
  home <- G.home
  File.write
    (home </> "conf.db")
    (Js.toStr $ Js.wMap [
      ("max_days", Js.wInt G.maxDays),
      ("min_days", Js.wInt G.minDays),
      ("max_strip", Js.wDouble G.maxStrip)
      ])
  (model, nicks) <- rnicks
  qmodel <- rqnick model
  let ndays = length qmodel
  qs <- rqnicks (return [qmodel]) ndays nicks
  let qnicks = (length nicks) + 1
  let qs0 = List.transpose qs
  let qs' = map (\ls -> listArray (0, qnicks - 1) ls) qs0
  return Qdata {quotes = qs', qdays = ndays, qnicks = qnicks}
  where
  rnicks = do
    str <- File.read (G.quotesDataPath </> "nicks.db")
    let [_, modelJs, nksJs] = (Js.rList . Js.fromStr) str
    let modelId = Js.rString modelJs
    let (model, nicks) = foldl (\(m, ns) row ->
                                  let [idJs, nJs, _, selJs] = Js.rList row
                                  in if Js.rString idJs == modelId
                                     then (Js.rString nJs, ns)
                                     else if Js.rBool selJs
                                          then (m, (Js.rString nJs):ns)
                                          else (m, ns)
                                  ) ("", []) $ Js.rList nksJs
    let md = if model == "" then head nicks else model
    return (md, nicks)
  toQuote l = let [_, op, cl, mx, mn, v, _] = Bs.split 58 l
              in  Q.new (pr op) (pr cl) (pr mx) (pr mn) (pri v)
              where
                pr = read . U8.toString
                pri = read . U8.toString
  rqnick nk = do
    bs <- File.readBs (G.quotesDataPath </> "quotes" </> (nk ++ ".db"))
    return $ map toQuote $ reverse $ filter (not . Bs.null) $ Bs.split 10 bs
  rqnicks r _ [] = r
  rqnicks r n (nk:nks) =
    rqnicks (do
              rls <- r
              qs <- rqnick nk
              return $ (complete qs):rls
              ) n nks
    where
      complete ls = if length ls == n
                    then ls
                    else complete ((Q.new (-1) (-1) (-1) (-1) (-1)):ls)

writeGroup :: String -> [Fresult] -> IO ()
writeGroup g rss = putStrLn $ "Group " ++ g ++ "\n" ++
                    (foldl (\acc r -> acc ++ (Js.toStr(rsToJs r))) "" $
                      take 10 rss)
{-
writeGroup g rss = do
  home <- G.home
  let dir = home </> g
  File.mkDir dir
  day <- Date.now
  File.write (dir </> (Date.toStr day)) (Js.toStr (toJs rss))
  where
    toJs rss = Js.wList $ map (\rs -> Fresult.toJs rs) rss
-}
-- App management ----------------------------------------------------

lock :: IO Bool
lock = do
  home <- G.home
  let f = home </> "lock"
  ex <- File.exists f
  if ex
  then do
    putStrLn "Fleas already is running.\nOtherwise type:\nfleas unlock\n"
    return True
  else do
    File.write f ""
    return False

unlock :: IO ()
unlock = do
  home <- G.home
  let f = home </> "lock"
  File.del f

sendStop :: IO ()
sendStop = do
  home <- G.home
  File.write (home </> "stop") ""

stop :: IO (Bool)
stop = do
  home <- G.home
  let f = home </> "stop"
  ex <- File.exists f
  if ex
  then do
    File.del f
    return True
  else return False
