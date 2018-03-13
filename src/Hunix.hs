{-# LANGUAGE OverloadedStrings #-}
module Hunix where

import List
import System.Process
import System.IO.Strict

import System.Directory
import System.FilePath
import Data.Char(isSpace)
import Control.Monad
import System.Exit

-- import Data.ByteString.Lazy(pack)
-- data Path = Path {
--   extractPath :: [String] }

-- instance Show Path where
--   show (Path fp) = join "/" fp

-- -- instance Read Path where
  

-- toPath = Path . filter (not.isEmpty) . splitOn (=='/')

-- basename = last . extractPath
-- dirname  = dropEnd 1 . extractPath

        
-- exec cmd args = ((\(_,Just hout,_,_) -> hout) <$> createProcess (proc cmd args){std_out=CreatePipe})
--           >>= hGetContents
        
exec cmd args = readCreateProcessWithExitCode (proc cmd args) ""
  >>= \(exitcode, stdout, stderr) -> case exitcode of
                                       ExitSuccess -> return stdout
                                       _ -> error $ unlines [cmd, stdout, stderr]



-- size of a file or directory (recursively, uses `du`)
fsize :: Num a => String -> IO a
fsize = (fromInteger.(read::String->Integer).head.words <$>) . exec "du" . ("-s":) . (:[])

pwd = init <$> exec "pwd" [] -- init to strip off the \n


fileSize :: String -> IO Integer
fileSize = (read <$>) . exec "stat" . (:["-c","%s"])

uploadFile = (last . lines <$>)
  . exec "scp_website_with_date" . (:[])

getFileMimeType :: String -> IO [Char]
getFileMimeType = (extract <$>) . exec "file" . (:["--mime-type"])
  where extract = trim . tail . dropWhile (/=':')

encode_base64 = exec "base64" . (:[])

mvi from to = do
  exists <- doesFileExist to
  if exists
  then putStrLn $ to ++ "already Exists!"
  else renameFile from to

mkdir_noexists path = go 0
  where go k = doesDirectoryExist p
               >>= \b -> if b
                         then go (k+1)
                         else (do createDirectory p
                                  return p)
          where p = path ++ if k>0 then '_' : show k else ""
