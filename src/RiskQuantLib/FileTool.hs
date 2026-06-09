module RiskQuantLib.FileTool (
  listDir,
  readFileWithUtf8,
  ensureDir,
  writeFileEnsuringDir
) where

import System.IO (withFile, utf8, IOMode (ReadMode, WriteMode), openFile, hSetEncoding, hClose)
import System.Directory (createDirectoryIfMissing, getDirectoryContents)
import System.FilePath (takeDirectory)
import Control.DeepSeq (deepseq)
import Data.Text (Text)
import Data.Text.IO (hGetContents, hPutStr)

listDir :: FilePath -> IO [FilePath]
listDir = getDirectoryContents

readFileWithUtf8 :: FilePath -> IO Text
readFileWithUtf8 filePath = do
  inputHandle <- openFile filePath ReadMode
  hSetEncoding inputHandle utf8
  x <- hGetContents inputHandle
  x `deepseq` hClose inputHandle
  return x

ensureDir :: FilePath -> IO ()
ensureDir = createDirectoryIfMissing True . takeDirectory

writeFileEnsuringDir :: FilePath -> Text -> IO ()
writeFileEnsuringDir path content = do
  ensureDir path
  withFile path WriteMode $ \handle -> do
    hSetEncoding handle utf8
    hPutStr handle content
