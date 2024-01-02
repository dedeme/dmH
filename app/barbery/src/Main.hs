-- Copyright 02-Feb-2020 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

module Main where

import Control.Concurrent.MVar
import Data.IORef
import Data.List (find)
import qualified Dm.Time as Time
import qualified Dm.Thread as Thread
import qualified Dm.Actor as Actor
import qualified Dm.Rnd as Rnd
import Dm.Test

programDuration :: Int
programDuration = 30000 -- milliseconds

data BarberySt = PreOpen | Open | Closed deriving Eq

data Model = Model
  { endTime :: Time.T
  , timeIsOver :: Bool
  , barberySt :: BarberySt
  , busy :: Bool
  , clOrder :: [Client]
  , seats :: [Maybe Client]
  }

data Request = TTimeOver
             | BOpen
             | BClose
             | BCut
             | CIsClosed
             | CNewClient Barber Client
             deriving Eq

data Response = None
              | BTimeOver Bool
              | BCutting Bool
              | CClosed Bool
              deriving Eq

msg :: IORef Model -> String -> IO ()
msg model m = do
  md <- readIORef model
  putStrLn $ "--------------------------\nBarbery "
               ++ (if barberySt md == Open then "Open" else "Close")
               ++ ", Order: " ++ show (clOrder md)
               ++ "\n Seats: "  ++ showSeats (seats md) ++ " >> " ++ m
  where
    showSeats ss = foldl showSeat "[|" ss ++ "]"
    showSeat seed (Just cl) = seed ++ show cl ++ "|"
    showSeat seed _ = seed ++ "   |"

-- BARBER
type Barber = MVar ()

newBarber :: IO Barber
newBarber = newEmptyMVar

barberAwake :: Barber -> IO ()
barberAwake b = putMVar b ()

barberToSleep :: Barber ->  IO ()
barberToSleep = takeMVar

-- CLIENT
data Client = Client { clientId :: Int }
instance Eq Client where
  (==) c1 c2 = clientId c1 == clientId c2
instance Show Client where
  show c = let s = "00" ++ show (clientId c) in "C" ++ drop (length s - 3) s

clientSeed :: Client
clientSeed = Client { clientId = 0 }

nextClient :: Client -> Client
nextClient cl = cl { clientId = clientId cl + 1 }

main :: IO ()
main = do
  putStrLn "start"
  ac <- Actor.new
  now <- Time.now
  model <- newIORef $ Model
                { endTime = Time.add programDuration now
                , timeIsOver = False
                , barberySt = PreOpen
                , busy = False
                , clOrder = []
                , seats = [Nothing, Nothing, Nothing, Nothing]
                }

  barber <- newEmptyMVar :: IO Barber
  clientRf <- newIORef clientSeed

  timerTh <- timerRun barber model ac
  barberTh <- barberRun barber model ac
  clientMakerTh <- clientMakerRun barber model clientRf ac

  Thread.join timerTh
  Thread.join barberTh
  Thread.join clientMakerTh

  msg model "Program end"


-- TIMER
timerRun :: Barber -> IORef Model -> Actor.T -> IO Thread.T
timerRun barber modelRf ac = Thread.joinable run
  where
    run = do
      model <- readIORef modelRf
      now <- Time.now
      Thread.sleep 1000
      if now < endTime model then run
      else update modelRf TTimeOver ac >> barberAwake barber >> return ()


-- BARBER
barberRun :: Barber -> IORef Model -> Actor.T -> IO Thread.T
barberRun barber modelRf ac = Thread.joinable run
  where
    up = update modelRf
    run = do
      now <- Time.now
      preOpenTime <- Rnd.i 5000
      Thread.sleep $ preOpenTime + 5000
      up BOpen ac
      activity
    activity = do
      barberToSleep barber
      model <- readIORef modelRf
      if timeIsOver model then up BClose ac >> return ()
      else cutting >> activity
    cutting = do
      rp <- up BCut ac
      if rp == BCutting False then return ()
      else do
        Thread.sleep 2000
        model <- readIORef modelRf
        if timeIsOver model then up BClose ac >> cutting
        else cutting

-- CLIENTS
clientMakerRun :: Barber -> IORef Model -> IORef Client -> Actor.T ->
                  IO Thread.T
clientMakerRun barber modelRf clRf ac = Thread.joinable run
  where
    run = do
      t <- Rnd.i 500
      Thread.sleep  $ t + 1000
      rp <- update modelRf CIsClosed ac
      if rp == CClosed False
        then do
          oldCl <- readIORef clRf
          let cl = nextClient oldCl
          writeIORef clRf cl
          msg modelRf $ "Client " ++ (show cl) ++ " created"
          update modelRf (CNewClient barber cl) ac
          run
        else return ()

-- SITS
clientTakeSeat :: Client -> Model -> IO (Maybe Model)
clientTakeSeat cl model = do
  ss <- Rnd.shuffle [0, 1, 2, 3]
  case find (\n -> ((seats model) !! n) == Nothing) ss of
    Nothing -> return Nothing
    Just n ->
      return $ Just $ model
                        { clOrder = cl:(clOrder model)
                        , seats = let (r, l) = splitAt n (seats model)
                                  in  case l of
                                        [] -> r ++ [Just cl]
                                        (_:l) -> r ++ ((Just cl):l)
                        }

barberTakeClient :: Model -> Maybe (Client, Model)
barberTakeClient model =
  case clOrder model of
    [] -> Nothing
    ls -> let (clOrder', (cl:_)) = splitAt (length ls - 1) ls
              seats' = map (\c ->
                              case c of
                                Just cl' | cl' /= cl -> Just cl'
                                _ -> Nothing
                            ) $ seats model
          in  Just (cl, model { clOrder = clOrder', seats = seats' })

-- UPDATE
update :: IORef Model -> Request -> Actor.T -> IO Response
update modelRf rq ac = Actor.send ac process
  where
    putMsg = msg modelRf

    process = case rq of
      TTimeOver -> ttimeOver
      BOpen -> bOpen
      BClose -> bClose
      BCut -> bCut
      CIsClosed -> cIsClosed
      CNewClient barber cl -> cNewClient barber cl

    ttimeOver = do
      model <- readIORef modelRf
      writeIORef modelRf $ model { timeIsOver = True }
      putMsg "Time is over"
      return None

    bOpen = do
      model <- readIORef modelRf
      writeIORef modelRf $ model { barberySt = Open }
      putMsg "Barbery is open"
      return None

    bClose = do
      model <- readIORef modelRf
      if barberySt model == Closed then return None
      else do
        writeIORef modelRf $ model { barberySt = Closed }
        putMsg "Barbery is closed"
        return None

    bCut = do
      model <- readIORef modelRf
      let r = barberTakeClient model
      case r of
        Just (cl, m) -> do
          writeIORef modelRf $ m { busy = True}
          putMsg $ "Cutting to " ++ show cl
          return $ BCutting True
        _ -> do
          writeIORef modelRf $ model { busy = False}
          putMsg "No client"
          return $ BCutting False

    cIsClosed = do
      model <- readIORef modelRf
      if barberySt model == Closed
        then do
          putMsg "Client Maker stoped"
          return $ CClosed True
        else return $ CClosed False

    cNewClient barber cl = do
      model <- readIORef modelRf
      if barberySt model == Open
        then do
          mbModel <- clientTakeSeat cl model
          case mbModel of
            Just m -> do
              do
                if (length (clOrder m) == 1) && not (busy m)
                  then barberAwake barber
                  else return ()
              writeIORef modelRf m
              putMsg $ show cl ++ " take a seat"
              return None
            _ -> do
              putMsg $ show cl ++ " go"
              return None
        else do
          putMsg $ show cl ++ " go"
          return None
