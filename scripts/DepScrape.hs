module DepScrape where

import System.Directory
import Prelude as P
import Control.Monad
import Data.List
import Control.Exception
import Control.DeepSeq
import Text.Read
import Data.Maybe
import Data.Set as Set

getDeps path = do
	P.putStrLn "running script"
	files <- getDirectoryContents path
	deps <- forM (P.filter ((== "og.") . P.take 3 . P.reverse) files) $ \file -> do
		P.putStrLn file
		fmap extractDeps $ P.readFile $ path ++ "/" ++ file
	let allDeps =  Set.toList $ Set.fromList $ catMaybes $ P.map readMaybe $ P.concat deps
	P.putStrLn $ intercalate "\n" allDeps
	return ()

extractDeps content =
	P.takeWhile (/= ")") $ P.drop 2 $ P.dropWhile (/= "import") $ words content
