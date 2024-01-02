-- Copyright 14-Feb-2020 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | String functions.

module Kut.Str
  ( split
  , Kut.Str.break
  , breakLast
  , join
  , replace
  , starts
  , ends
  , ltrim
  , rtrim
  , trim
  , toUpper
  , toLower
  , sub
  , left
  , right
  , toUtf8
  , fromUtf8
  )where

import qualified Data.ByteString.UTF8 as U8
import qualified Data.Text as Text
import qualified Data.List as List

-- | split sep s
--
-- Split /s/ in pieces.
--
-- /sep/ can not be "".
split :: String -> String -> [String]
split sep s = map Text.unpack $ Text.splitOn (Text.pack sep) (Text.pack s)

-- | break sep s
--
-- Breack /s/ in two pieces using the first ocurrence of /sep/.
--
-- /sep/ can not be "".
break :: String -> String -> (String, String)
break sep s = let (l, r) = Text.breakOn (Text.pack sep) (Text.pack s)
              in  (Text.unpack l, Text.unpack r)

-- | breakLast sep s
--
-- Breack /s/ in two pieces using the last occurrence of /sep/.
--
-- /sep/ can not be "".
breakLast :: String -> String -> (String, String)
breakLast sep s = let (l, r) = Text.breakOnEnd (Text.pack sep) (Text.pack s)
              in  (Text.unpack l, Text.unpack r)

-- | join sub ss
--
-- Concatenate /ss/ using /sep/.
join :: String -> [String] -> String
join sep ss = List.intercalate sep ss

-- | replace sub repl s
--
-- Replace every ocurrence of /sub/ by /repl/ in /s/.
--
-- /ssub/ can not be "".
replace :: String -> String -> String -> String
replace ssub repl s = join repl $ split ssub s

-- | starts sub s
--
-- Returns 'True' if /s/ starts with /sub/.
starts :: String -> String -> Bool
starts ssub s = Text.isPrefixOf  (Text.pack ssub) (Text.pack s)

-- | ends sub s
--
-- Returns 'True' if /s/ ends with /sub/.
ends :: String -> String -> Bool
ends ssub s = Text.isSuffixOf (Text.pack ssub) (Text.pack s)

-- | ltrim s
--
-- Removes starting spaces in /s/.
ltrim :: String -> String
ltrim = dropWhile (<= ' ')

-- | rtrim s
--
-- Removes trailing spaces in /s/.
rtrim :: String -> String
rtrim = reverse . (dropWhile (<= ' ')) . reverse

-- | trim s
--
-- Removes starting and trailing spaces in /s/.
trim :: String -> String
trim = ltrim . rtrim

-- | toUpper s
--
-- Returns /s/ in uppercase.
toUpper :: String -> String
toUpper = Text.unpack . Text.toUpper . Text.pack

-- | toLower s
--
-- Returns /s/ in lowercase.
toLower :: String -> String
toLower = Text.unpack . Text.toLower . Text.pack

-- | sub start end s
--
-- Returns characters of /s/ between /start/ (inclusive) and /end/ (exclusive).
--
-- If /start/ or /end/ are negatives, its value is changed to 'length + value'.
sub :: Int -> Int -> String -> String
sub start end s = drop (sig start) $ take (sig end) s
  where
    sig :: Int -> Int
    sig n = if n >= 0 then n else length s + n

-- | left n s
--
-- Equals to @'sub' 0 n@.
left :: Int -> String -> String
left ix s = let ix' = if ix >= 0 then ix else length s + ix
            in  take ix' s

-- | right ix s
--
-- Equals to @'sub' ix (length s)@.
right :: Int -> String -> String
right ix s = let ix' = if ix >= 0 then ix else length s + ix
             in  drop ix' s

-- | toUtf8 s
--
-- Returns /s/ codified as UTF8.
toUtf8 :: String -> U8.ByteString
toUtf8 = U8.fromString

-- | fromUtf8 us
--
-- Returns the UTF8 codified String /us/ as a normal one.
fromUtf8 :: U8.ByteString -> String
fromUtf8 = U8.toString
