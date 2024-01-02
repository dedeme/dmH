-- Copyright 02-Jan-2023 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | File utilities.

module Kut.File
  ( tmp,
    home,
    cwd,
    cd,
    mkDir,
    dir,
    exists,
    isDirectory,
    isLink,
    del,
    rename,
    copy,
    link,
    size,
    --modified,
    appendBs,
    writeBs,
    readBs,
    append,
    write,
    Kut.File.read,
    parent,
    base,
    name,
    extension
  ) where

import qualified System.Directory as Dir
import qualified Data.ByteString as Bs
import qualified System.FilePath.Posix as Fp

import qualified Kut.Cryp as Cryp
import qualified Kut.Str as Str
-- import qualified Dm.Time as Time

-- | tmp tpl
--
-- Returns a new not existent file path whose template is: @/tmp/tplxxxxxxxxxx@.
tmp :: String -> IO FilePath
tmp tpl = do
  k <- Cryp.genk 10
  let r = "/tmp/" ++ tpl ++ (Str.fromUtf8 k)
  x <- exists r
  if x then tmp tpl else return r

-- | home
--
-- Returns the home directory.
home :: IO FilePath
home = Dir.getHomeDirectory

-- | cwd
--
-- Returns the current directory.
cwd :: IO FilePath
cwd = do
  r <- Dir.getCurrentDirectory
  Dir.makeAbsolute r

-- | cd path
--
-- Changes the current directory to /path/.
cd :: FilePath -> IO ()
cd = Dir.setCurrentDirectory

-- | mkDir path
--
-- Creates a directory in /path/ if it does not exist.
--
-- This function creates parent directories if it is need.
mkDir :: FilePath -> IO ()
mkDir = Dir.createDirectoryIfMissing True

-- | dir path
--
-- Returns a list of all entries in dir without special entries (. and ..)
dir :: FilePath -> IO [FilePath]
dir = Dir.listDirectory

-- | exists path
--
-- Returns 'True' if /path/ exists in file system.
exists :: FilePath -> IO Bool
exists = Dir.doesPathExist

-- | isDirectory path
--
-- Returns 'True' if /path/ exists and is a directory.
isDirectory :: FilePath -> IO Bool
isDirectory = Dir.doesDirectoryExist

-- | isLink path
--
-- Returns 'True' if /path/ exists and is a Symlink.
isLink :: FilePath -> IO Bool
isLink path = do
  ex <- exists path
  if ex
  then Dir.pathIsSymbolicLink path
  else return False

-- | del path
--
-- Removes /path/ from file system.
--
-- If /path/ does not exist, it does nothing.
del :: FilePath -> IO ()
del = Dir.removePathForcibly

-- | rename old new
--
-- Renames /old/ as /new/ if /new/ does not exist. Otherwise it fails.
rename :: FilePath -> FilePath -> IO ()
rename old new = do
  ex <- exists new
  if ex
  then fail $ "'" ++ new ++ "' already exists"
  else Dir.renamePath old new

-- | copy old new
--
-- Copies /old/ in /new/. /old/ must be a file.
--
-- If /new/ exists and is a file, it is overwritten. But if /new/ is a
-- directory, it must exist.
copy :: FilePath -> FilePath -> IO ()
copy = Dir.copyFile

-- | link path symbol
--
-- Creates a symbolic link from /symbol/ to /path/.
link :: FilePath -> FilePath -> IO ()
link path symbol = do
  isDir <- isDirectory path
  if isDir
  then Dir.createDirectoryLink path symbol
  else Dir.createFileLink path symbol

-- | size path
--
-- Returns the size of /path/.
size :: FilePath -> IO Integer
size = Dir.getFileSize

--- modified path
--- Returns the last modified time of 'path'
--modified :: FilePath -> IO Time.T
--modified path = Time.fromUtc <$> Dir.getAccessTime path

-- | appendBs path bs
--
-- Appends /bs/ to /path/.
appendBs :: FilePath -> Bs.ByteString -> IO ()
appendBs = Bs.appendFile

-- | writeBs path bs
--
-- Writes /bs/ in /path/.
writeBs :: FilePath -> Bs.ByteString -> IO ()
writeBs = Bs.writeFile

-- | readBs path
--
-- Reads /path/.
readBs :: FilePath -> IO Bs.ByteString
readBs = Bs.readFile

-- | append path s
--
-- Appends /s/ to /path/ in UTF-8
append :: FilePath -> String -> IO ()
append path = (Bs.appendFile path) . Str.toUtf8

-- | write path s
--
-- Writes /s/ in /path/ in UTF-8
write :: FilePath -> String -> IO ()
write path = (Bs.writeFile path) . Str.toUtf8

-- | read path
--
-- Read /path/ which was written in UTF-8.
read :: FilePath -> IO String
read path = Str.fromUtf8 <$> Bs.readFile path

-- | parent path
--
-- Returns the parent of /path/.
parent :: FilePath -> FilePath
parent = Fp.takeDirectory

-- | baseName path
--
-- Returns the base name of /path/.
base :: FilePath -> String
base = Fp.takeBaseName

-- | name path
--
-- Returns the name of /path/.
name :: FilePath -> String
name = Fp.takeFileName

-- | extension path
--
-- Returns the extension of /path/ with dot.
extension :: FilePath -> String
extension = Fp.takeExtension


