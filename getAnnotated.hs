import Data.Char
import Data.List
import System.IO
import Data.List.Split
import Data.Maybe
import Control.Monad
import qualified Data.Map as M
import qualified Data.Set as S
import System.Directory
import System.FilePath

inDir = ""--HAS TO BE ADDED BEFORE COMPILING AND EXECUTING; example: "/home/user/Documents/training_set/"
outDir = ""--HAS TO BE ADDED BEFORE COMPILING AND EXECUTING; example: "/home/user/Documents/training_set_keyphrases/"
stopWordFile = ""--HAS TO BE ADDED BEFORE COMPILING AND EXECUTING; example: "stopwords.txt"

takeNumber = 1000 :: Int

getTextOnly :: String -> String
getTextOnly s = head $ splitOn "</slugline>" $ last $ splitOn "<slugline>" s

splitText :: String -> String -> String -> [String]
splitText chars word []     = [word]
splitText []    _    _      = error "invalid stop char sequence"
splitText chars word (t:ts)
  | elem t chars = [word] ++ splitText chars "" ts
  | not (isAlpha t || isSpace t) = splitText chars word ts
  | otherwise    = splitText chars (word ++ [toLower t]) ts

makeTuples :: S.Set String -> String -> [[String]]
makeTuples stopW = filter (removeStop stopW) . concat . map (filter (\ss -> ss /= [] && length ss <= 4) . map (stem . words) . splitText "!\"'()+.,;:?"
  "") . splitText "|" "" 

stem2 :: String -> String
stem2 "" = error "stemming empty string"
stem2 s
  | i >= 5 && i > div (length s) 2 = take i s
  | otherwise                      = s
  where i = if fi == [] then 10 else maximum fi
        fi = findIndices (\a -> elem a "aeiouAEIOU") s

stem :: [String] -> [String]
stem (x:[]) = [stem2 x]
stem xs     = map (take 5) xs

main = do
  sws <- readFile stopWordFile
  let stopW = S.fromList $ lines sws
  fns <- getDirectoryContents inDir
  let a = take takeNumber $ filterFiles fns
  forM a (processFiles stopW)

processFiles stopW fn = do
  s <- readFile fn
  let kws = makeTuples stopW $ getTextOnly s
  let nfn = outDir ++ (snd $ splitFileName fn)
  writeFile nfn $ show kws

filterFiles [] = error "empty folder"
filterFiles fs = map (\f -> inDir ++ f) $ sort $ filter (\f -> isSuffixOf ".xml" f) fs

removeStop :: S.Set String -> [String] -> Bool
removeStop stopW (a:[]) = not (S.member a stopW)
removeStop stopW (a:b:[]) = not (S.member a stopW) && not (S.member b stopW)
removeStop stopW (a:b:c:[]) = not (S.member a stopW) && not (S.member c stopW)
removeStop stopW (a:b:c:d:[]) = not (S.member a stopW) && not (S.member d stopW)
removeStop stopW x = error $ show x
