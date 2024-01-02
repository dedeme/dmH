-- Copyright 01-Jan-2024 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | B64 management.
--
-- Alphabet: "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

module Kut.B64
  ( encode,
    decode
  ) where

import qualified Data.ByteString.UTF8 as U8
import Data.ByteString.Base64 (encode, decodeLenient)

-- | Decode a base64-encoded string. This function is lenient in following the
-- specification from RFC 4648, and will not generate parse errors no matter
-- how poor its input.
decode :: U8.ByteString -> U8.ByteString
decode = decodeLenient

