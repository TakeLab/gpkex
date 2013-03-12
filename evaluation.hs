{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}

import GenProg
import Data.Generics
import Control.Monad
import Control.Monad.Random
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Char
import Data.List
import Data.Maybe
import System.Directory
import Data.List.Split


inDirT = ""--HAS TO BE ADDED BEFORE COMPILING AND EXECUTING; example: "/home/user/Documents/ksm/"
inDirP = ""--HAS TO BE ADDED BEFORE COMPILING AND EXECUTING; example: "/home/user/Documents/document_test_set/"
stopWordFile = ""--HAS TO BE ADDED BEFORE COMPILING AND EXECUTING; example: "stopwords.txt"
morphLexicon = ""--HAS TO BE ADDED BEFORE COMPILING AND EXECUTING; example: "morphLexicon.txt"


stemN = 5 :: Int
rareNumber = 10 :: Int
takeNumber = 300 :: Int
takeSample = 60 :: Int

--------------------------
--GAP
--------------------------

sortByOcc2 :: [[[String]]] -> [(Int,[String])]
sortByOcc2 xs = reverse $ sort $ nub $ map (\x -> nOfOcc x) xs'
  where nOfOcc x = (length $ filter (\x' -> x' == x) xs',x)
        xs'      = map stem $ concat xs

gap :: [[String]] -> [(Int,[String])] -> Float
gap myRes realRes = 1 / lowerSum * sum1
  where lowerSum = avgY (map fst realRes) (length realRes)
        avgY ys 1 = fromIntegral $ head ys
        avgY ys i = (fromIntegral $ sum $ take i ys) / (fromIntegral i) + avgY ys (i-1)
        myR = map evaluate myRes
        evaluate s = fromIntegral $ sum $ map fst $ filter (\(num, str) -> str == s) realRes
        sum3 k = sum $ take k myR
        myR' = zip [1..] myR
        sum2 (i,x) | x == 0 = 0
                   | otherwise = 1 / (fromIntegral i) * (fromIntegral $ sum3 i)
        sum1 = sum $ map sum2 myR'

--------------------------
--KENDAL TAU
--------------------------
sortByOcc :: [[[String]]] -> [(Int,[String])]
sortByOcc xs = zip [1..] $ reverse $ map snd $ sort $ nub $ map (\x -> nOfOcc x) xs'
  where nOfOcc x = (length $ filter (\x' -> x' == x) xs',x)
        xs'      = map stem $ concat xs


kendallTau :: [(Int,[String])] -> [(Int,[String])] -> Int -> Float
kendallTau tau1 tau2 k = (sum $ map cmp komb') / maxV
  where tau1' = take k tau1
        tau2' = take k tau2
        tau1'' = M.fromList $ map (\(x,y) -> (y,x)) tau1'
        tau2'' = M.fromList $ map (\(x,y) -> (y,x)) tau2'
        oba   = nub $ (map snd tau1') ++ (map snd tau2')
        komb  = concat $ map (\t1 -> map (\t2 -> (t1,t2)) oba) oba
        remDup (x:[]) = if (fst x) == (snd x) then [] else [x]
        remDup (x:xs) = if elem (snd x,fst x) xs || (fst x) == (snd x) then remDup xs else [x] ++ remDup xs
        komb' = remDup komb
        maxV  = fromIntegral $ length komb'
        p = 0.5
        solve (Nothing) = error "kendall tau, not found in tau list"
        solve (Just a)  = a
        cmp (i,j) | M.member i tau1'' && M.member j tau1'' && M.notMember i tau2'' && M.notMember j tau2'' = p
                  | M.member i tau2'' && M.member j tau2'' && M.notMember i tau1'' && M.notMember j tau1'' = p
                  | M.member i tau1'' && M.notMember j tau1'' && M.notMember i tau2'' && M.member j tau2'' = 1
                  | M.notMember i tau1'' && M.member j tau1'' && M.member i tau2'' && M.notMember j tau2'' = 1
                  | M.member i tau1'' && M.member j tau1'' && M.member i tau2'' && M.notMember j tau2'' = if solve (M.lookup j tau1'') < solve (M.lookup i tau1'') then 1 else 0
                  | M.member i tau1'' && M.member j tau1'' && M.notMember i tau2'' && M.member j tau2'' = if solve (M.lookup i tau1'') < solve (M.lookup j tau1'') then 1 else 0
                  | M.member i tau1'' && M.notMember j tau1'' && M.member i tau2'' && M.member j tau2'' = if solve (M.lookup j tau2'') < solve (M.lookup i tau2'') then 1 else 0
                  | M.notMember i tau1'' && M.member j tau1'' && M.member i tau2'' && M.member j tau2'' = if solve (M.lookup i tau2'') < solve (M.lookup j tau2'') then 1 else 0
                  | otherwise = if solve (M.lookup i tau1'') > solve (M.lookup j tau1'') && solve (M.lookup i tau2'') > solve (M.lookup j tau2'') || solve (M.lookup i tau1'') < solve (M.lookup j tau1'') && solve (M.lookup i tau2'') < solve (M.lookup j tau2'') then 0 else 1

--------------------------
--LEVELS OF AGREEMENT
--------------------------

strong :: [[[String]]] -> [[String]]
strong ss = nub $ filter isStrong ss'
  where isStrong s = (foldl (\br s' -> if s' == s then br + 1 else br) 1 ss') >= 5
        ss'        = concat ss

weak :: [[[String]]] -> [[String]]
weak ss = nub $ filter isWeak ss'
  where isWeak s = (foldl (\br s' -> if s' == s then br + 1 else br) 1 ss') >= 2
        ss'      = concat ss

---

getMAP :: [[String]] -> [(Int,[String])] -> Float
getMAP realRes myRes = (fst suma) / (fromIntegral $ snd suma)
  where myRes' = map snd myRes
        suma = foldl (\(n,j) res -> if elem res myRes' then (n + (precAtI (pomLis res) realRes myRes'), j + 1) else (n,j)) (0,0) realRes
        pomLis res = fst $ head $ filter (\(_,r) -> r == res) myRes

---

precAtI :: Int -> [[String]] -> [[String]] -> Float
precAtI i realRes myRes = (fromIntegral $ length $ filter (\m -> elem m myRes') realRes) / (fromIntegral i)
  where myRes' = take i myRes

recAtI :: Int -> [[String]] -> [[String]] -> Float
recAtI i realRes myRes = if length realRes == 0 then 1 else (fromIntegral $ length $ filter (\m -> elem m myRes') realRes) / (fromIntegral $ length realRes)
  where myRes' = take i myRes

---

getMRR :: [[String]] -> [(Int,[String])] -> Float
getMRR realRes myRes = (sum pomLis) / (fromIntegral $ length realRes')
  where myRes' = map snd myRes
        realRes' = filter (\r -> elem r myRes') realRes
        pomLis = map (\r -> 1.0 / (fromIntegral $ fst $ head $ filter (\(_,r') -> r == r') myRes)) realRes'

---

rPrecision :: [[String]] -> [(Int,[String])] -> Float
rPrecision realRes myRes = precAtI (length realRes') realRes' myRes'
  where myRes' = map snd myRes
        realRes' = filter (\r -> elem r myRes') realRes

--------------------------
--BENEVOLENT
--------------------------

benevolent :: [[[String]]] -> ([[String]],[[String]])
benevolent ss = (nub ss', nub $ filter isBen ss')
  where len = length ss
        ss' = concat ss
        isBen s = (length $ filter (\x -> x == s) ss') >= len

benevolentF1 :: ([[String]],[[String]]) -> [(Int,[String])] -> Float
benevolentF1 (pRes, rRes) myRes = (2 * p * r) / (p + r)
  where myRes' = map snd myRes
        p      = precAtI 10 pRes myRes'
        r      = recAtI 10 rRes myRes'

benevolentF2 :: ([[String]],[[String]]) -> [(Int,[String])] -> Float
benevolentF2 (pRes, rRes) myRes = (5 * p * r) / (4 * p + r)
  where myRes' = map snd myRes
        p      = precAtI 10 pRes myRes'
        r      = recAtI 10 rRes myRes'

---------------------------------------------------------------------------------

--------------------------
--GENPROG
--------------------------
data E = Plus E E
       | Minus E E
       | Times E E
       | Div E E
       | Log E
       | Inv E
       | TimTen E
       | DivTen E
       | Tf
       | Idf
       | Tfidf
       | First
       | Last
       | NumberF
       | NumberS
       | NumberT
       | Length
       | Rare
       | Title
       deriving (Typeable,Data,Eq,Show,Read)

eval :: E -> Candidate -> Maybe Float
eval (Plus e1 e2) c  = liftM2 (+) (eval e1 c) (eval e2 c)
eval (Minus e1 e2) c = liftM2 (-) (eval e1 c) (eval e2 c)
eval (Times e1 e2) c = liftM2 (*) (eval e1 c) (eval e2 c)
eval (Div e1 e2) c | ok        = liftM2 (/) x1 x2
                   | otherwise = Nothing
  where (x1,x2) = (eval e1 c,eval e2 c)
        ok = x2 /= Just 0
eval (Log e1) c | ok        = liftM  (log) x1
                | otherwise = Nothing
  where x1 = eval e1 c
        ok = x1 > Just 0
eval (Inv e1) c | ok        = liftM  (1.0 / ) x1
                | otherwise = Nothing
  where x1 = eval e1 c
        ok = x1 /= Just 0
eval (TimTen e1) c = liftM (10.0 *) (eval e1 c)
eval (DivTen e1) c = liftM (/ 10.0) (eval e1 c)
eval Tf c      = Just (cTF c)
eval Idf c     = Just (cIDF c)
eval Tfidf c   = Just (cTFIDF c)
eval First c   = Just (cFirst c)
eval Last c    = Just (cLast c)
eval NumberF c = Just (cF c)
eval NumberS c = Just (cS c)
eval NumberT c = Just (cT c)
eval Length c  = Just (cNumber c)
eval Rare c    = Just (cRare c)
eval Title c   = Just (cTitle c)

data Phrase = Phrase {
  pWords :: [String],
  pPos   :: [Int] } deriving (Show, Read, Eq, Ord)

data Candidate = Candidate {
  cTF     :: Float,
  cIDF    :: Float,
  cTFIDF  :: Float,
  cFirst  :: Float,
  cLast   :: Float,
  cF      :: Float,
  cS      :: Float,
  cT      :: Float,
  cNumber :: Float,
  cRare   :: Float,
  cTitle  :: Float,
  cOrig   :: String } deriving (Show, Read, Eq, Ord)

--------------------------------------------------------------

getDict :: String -> M.Map String [String] -> M.Map String [String]
getDict s m = M.insert fi le m
  where w = words s
        le = filter (\w' -> (isUpper $ head w') && length w' == 1) $ tail w
        fi = map toLower $ head w

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

removeStop :: S.Set String -> [String] -> Bool
removeStop stopW (a:[]) = not (S.member a stopW)
removeStop stopW (a:b:[]) = not (S.member a stopW) && not (S.member b stopW)
removeStop stopW (a:b:c:[]) = not (S.member a stopW) && not (S.member c stopW)
removeStop stopW (a:b:c:d:[]) = not (S.member a stopW) && not (S.member d stopW)

getTextOnly :: String -> String
getTextOnly s = head $ splitOn "</body>" $ last $ splitOn "<body>" s

getTitle :: String -> String
getTitle s = head $ splitOn "</title>" $ last $ splitOn "<title>" s

makeTuples' :: String -> [String]
makeTuples' = stem . words

getKeywords :: String ->  [[[String]]]
getKeywords s = map getKW . tail $ splitOn "<keywords " s
  where getKW st = map (makeTuples' . map toLower . head . splitOn "</keyword>") . tail $ splitOn "<keyword>" st

getKeys :: String -> ([[String]], [[String]])
getKeys s = (uniq, allk)
  where kw = getKeywords s
        kw' = concat kw
        uniq = foldl (\l k -> if elem k l then l else k : l) [] kw'
        allk = foldl (\a b -> filter (\a' -> elem a' b) a) (head kw) (tail kw)

getPhrases :: [[String]] -> M.Map [String] Phrase
getPhrases s = foldl (\map (i,c) -> M.insertWith addPhrase (stem c) (Phrase c [i]) map) M.empty cand
  where cand = zip [1..] s
        addPhrase (Phrase c1 is) (Phrase c2 is') = Phrase c1 (is' ++ is)

onlyOneword :: M.Map [String] Phrase -> M.Map [String] Int -> Int -> S.Set (Float, String)
onlyOneword m dfm l = M.foldWithKey (\k o s -> S.insert (tfidf k o,w o) s) S.empty okW
  where onlyOne (x:[]) = True
        onlyOne _      = False
        okW            = M.filter (\(Phrase w _) -> onlyOne w) m
        w (Phrase (x:[]) _ )  = x
        p (Phrase _      l')  = fromIntegral $ length l'
        tfidf k o             = ((p o) / (fromIntegral l)) * log ((fromIntegral $ takeNumber) / (fromIntegral $ getdfm k $ M.lookup k dfm))
        getdfm k (Nothing)    = 1
        getdfm k (Just a)     = a

makeCandidates :: M.Map [String] Phrase -> S.Set String -> M.Map [String] Int -> [[String]] -> Int -> M.Map [String] Candidate
makeCandidates phrases toponeword dfm title doclen = M.mapWithKey (\k a -> mkC k a) phrases
  where tf (Phrase _ l)       = (fromIntegral $ length l) / (fromIntegral doclen)
        idf k                 = (fromIntegral $ takeNumber) / (fromIntegral $ getdfm k $ M.lookup k dfm)
        getdfm k (Nothing)    = 1
        getdfm k (Just a)     = a
        cfirst (Phrase _ l)   = fromIntegral $ head l
        clast (Phrase _ l)    = fromIntegral $ last l
        oneTh                 = div doclen 3
        first (Phrase _ l)    = fromIntegral $ length $ filter (\l' -> l' <= oneTh) l
        secon (Phrase _ l)    = fromIntegral $ length $ filter (\l' -> l' > oneTh && l' <= (2*oneTh)) l
        third (Phrase _ l)    = fromIntegral $ length $ filter (\l' -> l' > (2*oneTh)) l
        numb k                = fromIntegral $ length k
        rare k                | length k == 1 = fromIntegral $ length $ filter (\k' -> S.member k' toponeword) k
                              | otherwise     = fromIntegral $ length $ filter (\k' -> S.member k' toponeword') k
        toponeword'           = S.map (\str -> take 5 str) toponeword
        titl k                = if elem k title then 1 else 0
        orig (Phrase w _)     = unwords w
        mkC k a = Candidate (tf a) (idf k) ((tf a) * log(idf k)) (cfirst a) (clast a) (first a) (secon a) (third a) (numb k) (rare k) (titl k) (orig a)
        
takeBest :: S.Set (Float, String) -> S.Set String
takeBest s = S.fromList $ map (\(f,s') -> s') $ take rareNumber $ reverse $ S.toList s

rankCandidates :: (Candidate -> Maybe Float) -> M.Map [String] Candidate -> Int -> [[String]]
rankCandidates evF m i = take i $ map snd l
  where l = sort $ M.foldWithKey (\k c s -> (solve (evF c), k) : s) [] m
        solve (Nothing) = 1000000
        solve (Just a ) = a

filterFiles [] = error "empty folder"
filterFiles fs = sort $ filter (\f -> isSuffixOf ".xml" f) fs

filterFiles' [] = error "empty folder"
filterFiles' fs = sort $ filter (\f -> isSuffixOf ".txt" f) fs

sumUp xs = (sfoo prv, sfoo dru, sfoo tre, sfoo cet, sfoo pet, sfoo ses, sfoo sed, sfoo osm, sfoo dev, sfoo des, sfoo jed, sfoo dva, tri)
  where prv = map (\(x,_,_,_,_,_,_,_,_,_,_,_,_) -> x) xs
        dru = map (\(_,x,_,_,_,_,_,_,_,_,_,_,_) -> x) xs
        tre = map (\(_,_,x,_,_,_,_,_,_,_,_,_,_) -> x) xs
        cet = map (\(_,_,_,x,_,_,_,_,_,_,_,_,_) -> x) xs
        pet = map (\(_,_,_,_,x,_,_,_,_,_,_,_,_) -> x) xs
        ses = map (\(_,_,_,_,_,x,_,_,_,_,_,_,_) -> x) xs
        sed = map (\(_,_,_,_,_,_,x,_,_,_,_,_,_) -> x) xs
        osm = map (\(_,_,_,_,_,_,_,x,_,_,_,_,_) -> x) xs
        dev = map (\(_,_,_,_,_,_,_,_,x,_,_,_,_) -> x) xs
        des = map (\(_,_,_,_,_,_,_,_,_,x,_,_,_) -> x) xs
        jed = map (\(_,_,_,_,_,_,_,_,_,_,x,_,_) -> x) xs
        dva = map (\(_,_,_,_,_,_,_,_,_,_,_,x,_) -> x) xs
        tri = map (\(_,_,_,_,_,_,_,_,_,_,_,_,x) -> x) xs
        sfoo ys = (sum $ filter (\y -> not (isNaN y)) ys) / (fromIntegral $ length $ filter (\y -> not (isNaN y)) ys)

---------------------------------------------------------------------

main = do
  fnt <- getDirectoryContents inDirT
  let fns = filterFiles' fnt
  res <- forM fns main'
  print "===================="
  print $ sumUp res

main' fn1 = do
  fn2 <- getDirectoryContents inDirP
  sws <- readFile stopWordFile
  pos <- readFile morphLexicon
  let lem = foldl (\m a' -> (getDict a' m)) M.empty $ lines pos
  let stopW = S.fromList $ lines sws
  s' <- readFile "phraseFreq.txt"
  tree' <- readFile (inDirT ++ fn1)
  let tree'' = read $ tree' :: (String,Float)
  let tree = read $ fst $ tree'' :: E
  let pfq = M.fromList $ read s'
  let a = take takeSample $ filterFiles fn2
  res <- forM a (processFiles stopW lem tree pfq)
  print $ sumUp res
  return $ sumUp res
  

processFiles stopW lem tree pfq fn = do
  text <- readFile (inDirP ++ fn)
  let ttl = getTitle text
  let txt = ttl ++ " " ++ getTextOnly text
  let realRes = getKeys text
  let phr = getPhrases $ filter (\ph -> filterPOS ph lem) $ filter (removeStop stopW) $ makeTuples txt
  let len = M.size phr
  let owp = onlyOneword phr pfq len
  let can = makeCandidates phr (takeBest owp) pfq (filter (removeStop stopW) $ makeTuples ttl) len
  let mRes = rankCandidates (eval tree) can 1000
  let mRes' = zip [1..] mRes
  let kenList = sortByOcc $ getKeywords text
  let kenTau = kendallTau mRes' kenList (length kenList)
  let weakAg = weak $ getKeywords text
  let strongAg = strong $ getKeywords text
  let weakMAP = getMAP weakAg mRes'
  let strongMAP = getMAP strongAg mRes'
  let weakMRR = getMRR weakAg mRes'
  let strongMRR = getMRR strongAg mRes'
  let pAt10W = precAtI 10 weakAg (map snd mRes')
  let pAt10S = precAtI 10 strongAg (map snd mRes')
  let rPrecW = rPrecision weakAg mRes'
  let rPrecS = rPrecision strongAg mRes'
  let benRes = benevolent $ getKeywords text
  let benF1 = benevolentF1 benRes mRes'
  let benF2 = benevolentF2 benRes mRes'
  let gapList = sortByOcc2 $ getKeywords text
  let gapRes = gap mRes gapList
  let nOfW  = 10 * (precAtI 10 (nub $ concat $ getKeywords text) (map snd mRes'))
  let ispis = (kenTau,weakMAP,strongMAP,weakMRR,strongMRR,pAt10W,pAt10S,rPrecW,rPrecS,benF1,benF2,nOfW,gapRes)
  return ispis
