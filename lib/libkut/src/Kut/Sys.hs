-- Copyright 01-Jan-2024 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | System utilities.

module Kut.Sys
  ( catch
  )where

import qualified Control.Exception as Exc

-- | catch tryFn catchFn
--
-- Tries to execute /tryFn/ and if it fails excecute /catchFn/ passing the
-- error message.
catch :: IO a -> (String -> IO a) -> IO a
catch tryFn catchFn =
  Exc.catch tryFn $ \e -> do
                          let msg = show (e :: Exc.SomeException)
                          catchFn msg
