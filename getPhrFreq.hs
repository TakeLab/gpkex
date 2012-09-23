import Data.Char
import Data.List
import System.IO
import Data.List.Split
import qualified Data.Map as M
import qualified Data.Set as S
import System.Directory

inDir = ""--HAS TO BE ADDED BEFORE COMPILING AND EXECUTING; example: "/home/user/Documents/document_set/"
stopWordFile = ""--HAS TO BE ADDED BEFORE COMPILING AND EXECUTING; example: "stopwords.txt"
morphLexicon = ""--HAS TO BE ADDED BEFORE COMPILING AND EXECUTING; example: "morphLexicon.txt"

stemN = 5 :: Int
takeNumber = 960 :: Int

getDict :: String -> M.Map String [String] -> M.Map String [String]
getDict s m = M.insert fi le m
  where w = words s
        le = filter (\w' -> (isUpper $ head w') && length w' == 1) $ tail w
        fi = map toLower $ head w

getLemma :: String -> M.Map String String -> M.Map String String
getLemma s m = M.insert fi le m
  where w = words s
        fi = map toLower $ head w
        le = map (\c -> toLower c) $ head $ tail $ w

lemmatize :: [String] -> M.Map String String -> [String]
lemmatize ws m = map (\w -> solve w (M.lookup w m)) ws
  where solve w (Nothing) = w
        solve _ (Just a)  = a


getComb :: [String] -> [String] -> M.Map String [String] -> [String]
getComb [] l _ = l
getComb ss [] m = getComb (tail ss) el m
  where el = fin $ M.lookup (head ss) m
        fin (Just x) = x
        fin (Nothing) = ["X"]
getComb ss l m = getComb (tail ss) con m
  where el = fin $ M.lookup (head ss) m
        fin (Just x) = x
        fin (Nothing) = ["X"]
        con = concat $ map (\el' -> map (\l' -> l' ++ el') l) el

posPat = ["N","AN","NN","X","NSN","V"]

filterPOS :: [String] -> M.Map String [String] -> Bool
filterPOS phr m = any (\pat' -> elem pat' posPat) pat
  where pat = getComb phr [] m

takeTuple :: [String] -> [[String]]
takeTuple [] = []
takeTuple w
 | l >= 4    = [take 4 w] ++ [take 3 w] ++ [take 2 w] ++ [[head w]] ++ (takeTuple $ tail w)
 | l == 3    = [w] ++ [take 2 w] ++ [[head w]] ++ (takeTuple $ tail w)
 | l >= 2    = [w] ++ [[head w]] ++ [tail w]
 | otherwise = [w]
  where l = length w

splitText :: String -> String -> [String]
splitText word []     = [word]
splitText word (t:ts)
  | not (isAlpha t || isSpace t || isDigit t || elem t "-'\"") = [word] ++ splitText "" ts
  | isDigit t || elem t "-'\"" = splitText word ts
  | isSpace t = splitText (word ++ " ") ts
  | otherwise = splitText (word ++ [toLower t]) ts

makeTuples :: String -> [[String]]
makeTuples = concat . map takeTuple . map words . splitText "" . filter (\x -> not (elem x "€—£§«»<@♦¬°►•[_{„¥©>^~■®▼]"))

--krati riječ
stem2 :: String -> String
stem2 "" = error "stemming empty string"
stem2 s
  | i >= stemN && i > div (length s) 2 = take i s
  | otherwise                      = s
  where i = if fi == [] then 10 else maximum fi
        fi = findIndices (\a -> elem a "aeiouAEIOU") s

stem :: [String] -> [String]
stem (x:[]) = [stem2 x]
stem xs     = map (take 5) xs

getWordFreq :: [[String]] -> M.Map [String] Int
getWordFreq = foldl (\map key -> M.insert (stem key) 1 map) M.empty

getTextOnly :: String -> String
getTextOnly s = ttl ++ " . " ++ txt
  where txt = head $ splitOn "</body>" $ last $ splitOn "<body>" s
        ttl = head $ splitOn "</title>" $ last $ splitOn "<title>" s

filterFiles [] = error "empty folder"
filterFiles fs = map (\f -> inDir ++ f) $ sort $ filter (\f -> isSuffixOf ".xml" f) fs

getFileText stopW lem lemmas m' fn = do
  s <- readFile fn
  m <- m'
  let nm = getWordFreq $ map (\ph -> lemmatize ph lemmas) $ filter (\ph -> filterPOS ph lem) $ filter (removeStop stopW) $ makeTuples $ getTextOnly s
  return $ M.unionWith (+) m nm

main = do
  sws <- readFile stopWordFile
  let stopW = S.fromList $ lines sws
  pos <- readFile morphLexicon
  let lem = foldl (\m a' -> (getDict a' m)) M.empty $ lines pos
  let lemmas = foldl (\m a' -> (getLemma a' m)) M.empty $ lines pos
  fns <- getDirectoryContents inDir
  let a = take takeNumber $ filterFiles fns
  let b = foldl (getFileText stopW lem lemmas) emptyMap a
  b' <- b
  writeFile "phraseFreq.txt" $ show $ M.toList b'

emptyMap :: IO (M.Map [String] Int)
emptyMap = return M.empty


removeStop stopW (a:[]) = not (S.member a stopW)
removeStop stopW (a:b:[]) = not (S.member a stopW) && not (S.member b stopW)
removeStop stopW (a:b:c:[]) = not (S.member a stopW) && not (S.member c stopW)
removeStop stopW (a:b:c:d:[]) = not (S.member a stopW) && not (S.member d stopW)
