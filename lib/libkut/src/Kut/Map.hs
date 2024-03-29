-- Copyright 02-Jan-2023 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Immutable map.
--
-- This object is an unsorted list of (String, a). When a old key is
-- reassigned, a new entry is added in a new /Map/.
-- Functions 'pack' and 'delete' also creates new /Map/s.

module Kut.Map
  ( T
  , D
  , get
  , getOk
  , put
  , delete
  , pack
  , keys
  , values
  ) where

-- | Definition
type T a = [(String, a)]

-- | Map of String
type D = T String

-- | get key map
--
-- Gets a value from its key
get :: String -> T a -> Maybe a
get = lookup

-- | getOk key map
--
-- Gets a value from its key, raising an error if /key/ is not found.
getOk :: String -> T a -> a
getOk key m = case lookup key m of
  Just v -> v
  _ -> error $ "'" ++ key ++ "' not found in Map"

-- | put key value m
--
-- Adds a pair key-value. If key already exists, the former one is not deleted
-- until calling 'pack', 'keys' or 'values'
put :: String -> a -> T a -> T a
put key value m = (key, value) : m

-- | delete key m
--
-- Removes /key/
delete :: String -> T a -> T a
delete key m = filter (\(k, _) -> k /= key) m

-- | pack m
--
-- Removes former duplicate keys.
pack :: T a -> T a
pack m = pack' [] m
  where
  pack' r [] = r
  pack' r (e@(k, _):rest) =
    if any (\(kr, _) -> kr == k) r then pack' r rest
                                   else pack' (e:r) rest

-- | keys m
--
-- Returns keys of /m/.
keys :: T a -> [String]
keys = map (\(k, _) -> k) . pack

-- | values m
--
-- Returns values of /m/.
values :: T a -> [a]
values = map (\(_, v) -> v) . pack
