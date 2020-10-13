{-=Filtering-Analysis-Tool (FAT): A Haskell-based solution to=-}
{-=analyze filtering schemes applied to tab delimited data.=-}
{-=Author: Matthew Mosior=-}
{-=Version: 2.0=-}
{-=Synopsis:  This Haskell Script will take in=-}
{-=a tsv file and provide a in-depth view of=-}
{-=the user-defined filtering schema provided.=-}
{-=This module will provide xlsx definitions.=-}


{-Lanuguage Extension.-}

{-# LANGUAGE MultiWayIf #-}

{----------------------}


{-Module.-}

module SpecificFilters where

{--------}


{-Imports-}

import Control.Arrow as CA
import Control.Monad as CM
import Data.Char as DC
import Data.List as DL
import Data.List.Split as DLS
import Data.Tuple as DTuple
import Text.Regex as TR
import Text.Regex.TDFA as TRP

{---------}


{-specificFilters functions.-}

--isSubsetOf -> This function will
--be used in the stripHeader function.
xs `isSubsetOf` ys = DL.any (`DL.elem` ys) xs

--isAlphaList -> This function will
--test a string for only alphabetic
--characters.
isAlphaList :: String -> Bool
isAlphaList xs = DL.all DC.isAlpha xs

--isNotAlphaList -> This function will
--test a String for non-alphabetic
--characters.
isNotAlphaList :: String -> Bool
isNotAlphaList xs = not (DL.all DC.isAlpha xs)

--mapTuple -> This function will
--map a function across all elements
--of a two-tuple.
mapTuple :: (b'->c') -> (b',b') -> (c',c')
mapTuple = CM.join (***)

--tuplifyTwo -> This function will
--turn a list of two elements into
--a two-tuple.
tuplifyTwo :: [a] -> (a,a)
tuplifyTwo [x,y] = (x,y)

--tripletFst -> This function will
--act as a fst but for a triplet.
tripletFst :: (String,Int,Int) -> String
tripletFst (x,y,z) = x

--tripletSnd -> This function will
--act as a snd but for a triplet.
tripletSnd :: (String,Int,Int) -> Int
tripletSnd (x,y,z) = y

--tripletFst -> This function will
--act to grab the third element of a triplet.
tripletThrd :: (String,Int,Int) -> Int
tripletThrd (x,y,z) = z

--quadrupletFst -> This function will
--act act to grab first element of a triplet.
quadrupletFst :: (String,Int,Int,String) -> String
quadrupletFst (a,b,c,d) = a

--quadrupletSnd -> This function will
--act act to grab second element of a triplet.
quadrupletSnd :: (String,Int,Int,String) -> Int
quadrupletSnd (a,b,c,d) = b

--quadrupletThrd -> This function will
--act act to grab third element of a triplet.
quadrupletThrd :: (String,Int,Int,String) -> Int
quadrupletThrd (a,b,c,d) = c

--quadrupletThrd -> This function will
--act act to grab last element of a triplet.
quadrupletFrth :: (String,Int,Int,String) -> String
quadrupletFrth (a,b,c,d) = d

--quadrupletTransform -> This function will
--transform a complex tuple into a 
--traditional quadruplet.
quadrupletTransform :: ((String,Int,Int),String) -> (String,Int,Int,String)
quadrupletTransform ((a,b,c),d) = (a,b,c,d)

--customListFilter -> This function will
--perform a custom, regex-based filtration
--using the nested predicate functions.
customListFilter :: [String] -> [[(String,Int,Int)]] -> [(String,Int,Int)]
customListFilter [] _      = []
customListFilter _  []      = []
customListFilter xs (y:ys) = if smallCustomPredicate (xs DL.!! 1) y
                                 then y
                                 else customListFilter xs ys
    where
        --Nested function definitions.--
        --smallCustomPredicate
        smallCustomPredicate :: String -> [(String,Int,Int)] -> Bool
        smallCustomPredicate []     _       = False
        smallCustomPredicate _      []      = False
        smallCustomPredicate x ((y,_,_):ys) = if (y == x :: Bool)
                                                  then True
                                                  else smallCustomPredicate x ys
        --------------------------------

--customOnlyDataFilter -> This function will
--perform a custom, regex-based partition
--using a custom predicate.
customOnlyDataFilter :: [String] -> [(String,Int,Int)] -> [(String,Int,Int)]
customOnlyDataFilter [] [] = []
customOnlyDataFilter [] _  = []
customOnlyDataFilter _  [] = []
customOnlyDataFilter xs ys = smallCustomFilter (xs DL.!! 1) ys
     where
         --Nested function definitions.--
         --smallCustomFilter
         smallCustomFilter :: String -> [(String,Int,Int)] -> [(String,Int,Int)]
         smallCustomFilter [] []     = []
         smallCustomFilter [] _      = []
         smallCustomFilter _  []     = []
         smallCustomFilter x  (y:ys) = if smallCustomPredicate x y
                                           then [y] ++ (smallCustomFilter x ys)
                                           else smallCustomFilter x ys
         --smallCustomPredicate
         smallCustomPredicate :: String -> (String,Int,Int) -> Bool
         smallCustomPredicate [] ([],_,_) = False
         smallCustomPredicate _  ([],_,_) = False
         smallCustomPredicate [] _        = False
         smallCustomPredicate x  (y,_,_)  = if (y == x :: Bool)
                                                then False
                                                else True
         --------------------------------

--customNotDataFilter -> This function will
--perform a custom, regex-based partition
--using a custom predicate.
customNotDataFilter :: [String] -> [(String,Int,Int)] -> [(String,Int,Int)]
customNotDataFilter [] [] = []
customNotDataFilter [] _  = []
customNotDataFilter _  [] = []
customNotDataFilter xs ys = smallCustomFilter (xs DL.!! 1) ys
    where
        --Nested function definitions.--
        --smallCustomFilter
        smallCustomFilter :: String -> [(String,Int,Int)] -> [(String,Int,Int)]
        smallCustomFilter [] []     = []
        smallCustomFilter [] _      = []
        smallCustomFilter _  []     = []
        smallCustomFilter x  (y:ys) = if smallCustomPredicate x y
                                         then [y] ++ (smallCustomFilter x ys)
                                         else smallCustomFilter x ys
        --smallCustomPredicate
        smallCustomPredicate :: String -> (String,Int,Int) -> Bool
        smallCustomPredicate [] ([],_,_) = False
        smallCustomPredicate _  ([],_,_) = False
        smallCustomPredicate [] _        = False
        smallCustomPredicate x  (y,_,_)  = if (y == x :: Bool)
                                               then True
                                               else False
        --------------------------------

--equalityListCheckBinary -> This function will
--serve to grab all elements for == filter.
equalityListCheckBinary :: [String] -> [(String,Int,Int)] -> [[(String,String)]]
equalityListCheckBinary _ []  = []
equalityListCheckBinary [] _  = []
equalityListCheckBinary xs (y:ys) = [smallEqualityListCheckBinary xs y] ++ (equalityListCheckBinary xs ys)
    where
        --Nested function definitions.--
        --smallEqualityListCheckBinary
        smallEqualityListCheckBinary :: [String] -> (String,Int,Int) -> [(String,String)]
        smallEqualityListCheckBinary [] (_,_,_)  = []
        smallEqualityListCheckBinary _  ([],_,_) = []
        smallEqualityListCheckBinary xs y        = if (tripletFst y) /= "NA" ||
                                                      (tripletFst y) /= "N/A"
                                                       then if smallPredicateBinary xs [tripletFst y]
                                                           then [(tripletFst y,"BINARYYES")]
                                                           else [(tripletFst y,"BINARYNO")]
                                                       else [(tripletFst y,"NA")]
        --smallPredicateBinary
        smallPredicateBinary :: [String] -> [String] -> Bool
        smallPredicateBinary [] _  = False
        smallPredicateBinary _  [] = False
        smallPredicateBinary xs ys = if ys `isSubsetOf` xs
                                         then True
                                         else False

        --------------------------------

--equalityListCheckTrinary -> This function will
--serve to grab all elements for == filter.
equalityListCheckTrinary :: [[String]] -> [(String,Int,Int)] -> [[(String,String)]]
equalityListCheckTrinary _ []  = []
equalityListCheckTrinary [] _  = []
equalityListCheckTrinary xs ys = [DL.map (\(a,b,c,d) -> (a,d)) 
                                  (correctListOrdering
                                   (DL.concat 
                                   (DL.filter (not . DL.null) 
                                    ([smallEqualityListCheckTrinaryHead (xs DL.!! 0) ys] 
                                  ++ [smallEqualityListCheckTrinaryMiddle (xs DL.!! 1) ys] 
                                  ++ [smallEqualityListCheckTrinaryTail (xs DL.!! 2) ys] 
                                  ++ [smallEqualityListCheckTrinaryNA ys])))
                                     (ys))]
    where
        --Nested function definitions.--
        --smallEqualityListCheckTrinaryHead
        smallEqualityListCheckTrinaryHead :: [String] -> [(String,Int,Int)] -> [(String,Int,Int,String)]
        smallEqualityListCheckTrinaryHead [] _      = []
        smallEqualityListCheckTrinaryHead _  []     = []
        smallEqualityListCheckTrinaryHead xs (y:ys) = (smallerEqualityListCheckTrinaryHead xs y)
                                                   ++ (smallEqualityListCheckTrinaryHead xs ys)
        --smallEqualityListCheckTrinaryHead
        smallerEqualityListCheckTrinaryHead :: [String] -> (String,Int,Int) -> [(String,Int,Int,String)]
        smallerEqualityListCheckTrinaryHead [] (_,_,_)  = []
        smallerEqualityListCheckTrinaryHead _  ([],_,_) = []
        smallerEqualityListCheckTrinaryHead xs y        = if (tripletFst y) /= "NA" ||
                                                             (tripletFst y) /= "N/A"
                                                              then if DL.any (\x -> DL.isInfixOf "<=" x) xs ||
                                                                      DL.any (\x -> DL.isInfixOf ">=" x) xs
                                                                  then if smallPredicateNumericTrinaryHead (DL.concat xs) 
                                                                                                           (tripletFst y)
                                                                      then [quadrupletTransform (y,"TRINARYHEAD")]
                                                                      else []
                                                                  else if smallPredicateCharTrinary xs [tripletFst y]
                                                                      then [quadrupletTransform (y,"TRINARYHEAD")] 
                                                                      else []
                                                              else []
        --smallEqualityListCheckTrinaryMiddle
        smallEqualityListCheckTrinaryMiddle :: [String] -> [(String,Int,Int)] -> [(String,Int,Int,String)]
        smallEqualityListCheckTrinaryMiddle [] _      = []
        smallEqualityListCheckTrinaryMiddle _  []     = []
        smallEqualityListCheckTrinaryMiddle xs (y:ys) = (smallerEqualityListCheckTrinaryMiddle xs y)
                                                     ++ (smallEqualityListCheckTrinaryMiddle xs ys)
        --smallEqualityListCheckTrinaryMiddle
        smallerEqualityListCheckTrinaryMiddle :: [String] -> (String,Int,Int) -> [(String,Int,Int,String)]
        smallerEqualityListCheckTrinaryMiddle [] (_,_,_)  = []
        smallerEqualityListCheckTrinaryMiddle _  ([],_,_) = []
        smallerEqualityListCheckTrinaryMiddle xs y        = if (tripletFst y) /= "NA" ||
                                                               (tripletFst y) /= "N/A"
                                                                then if DL.any (\x -> DL.isInfixOf "<=" x) xs ||
                                                                        DL.any (\x -> DL.isInfixOf ">=" x) xs
                                                                    then if smallPredicateNumericTrinaryMiddle (DL.concat xs) 
                                                                                                               (tripletFst y)
                                                                        then [quadrupletTransform (y,"TRINARYMIDDLE")]
                                                                        else []
                                                                    else if smallPredicateCharTrinary xs [tripletFst y]
                                                                        then [quadrupletTransform (y,"TRINARYMIDDLE")]
                                                                        else []
                                                                else []
        --smallEqualityListCheckTrinaryTail
        smallEqualityListCheckTrinaryTail :: [String] -> [(String,Int,Int)] -> [(String,Int,Int,String)]
        smallEqualityListCheckTrinaryTail [] _      = []
        smallEqualityListCheckTrinaryTail _  []     = []
        smallEqualityListCheckTrinaryTail xs (y:ys) = (smallerEqualityListCheckTrinaryTail xs y)
                                                   ++ (smallEqualityListCheckTrinaryTail xs ys)
        --smallEqualityListCheckTrinaryTail
        smallerEqualityListCheckTrinaryTail :: [String] -> (String,Int,Int) -> [(String,Int,Int,String)]
        smallerEqualityListCheckTrinaryTail [] (_,_,_)  = []
        smallerEqualityListCheckTrinaryTail _  ([],_,_) = []
        smallerEqualityListCheckTrinaryTail xs y        = if (tripletFst y) /= "NA" ||
                                                             (tripletFst y) /= "N/A"
                                                              then if DL.any (\x -> DL.isInfixOf "<=" x) xs ||
                                                                      DL.any (\x -> DL.isInfixOf ">=" x) xs
                                                                  then if smallPredicateNumericTrinaryTail (DL.concat xs) 
                                                                                                           (tripletFst y)
                                                                      then [quadrupletTransform (y,"TRINARYTAIL")]
                                                                      else []
                                                                  else if smallPredicateCharTrinary xs [tripletFst y]
                                                                      then [quadrupletTransform (y,"TRINARYTAIL")]
                                                                      else []
                                                              else []
        --smallEqualityListCheckTrinaryNA
        smallEqualityListCheckTrinaryNA :: [(String,Int,Int)] -> [(String,Int,Int,String)]
        smallEqualityListCheckTrinaryNA []     = []
        smallEqualityListCheckTrinaryNA (x:xs) = (smallerEqualityListCheckTrinaryNA x)
                                              ++ (smallEqualityListCheckTrinaryNA xs)
        --smallerEqualityListCheckTrinaryNA
        smallerEqualityListCheckTrinaryNA :: (String,Int,Int) -> [(String,Int,Int,String)]
        smallerEqualityListCheckTrinaryNA ([],_,_)  = []
        smallerEqualityListCheckTrinaryNA xs       = if (tripletFst xs) == "NA" ||
                                                        (tripletFst xs) == "N/A"
                                                         then [quadrupletTransform (xs,"NA")]
                                                         else []
        --smallPredicateNumericTrinaryHead
        smallPredicateNumericTrinaryHead :: String -> String -> Bool
        smallPredicateNumericTrinaryHead [] _  = False
        smallPredicateNumericTrinaryHead _  [] = False
        smallPredicateNumericTrinaryHead xs ys = if DL.isInfixOf "<=" xs &&
                                                    ((read ys) :: Double) <= 
                                                    (read (TR.subRegex (TR.mkRegex "<=") xs "") :: Double)
                                                     then True
                                                     else if DL.isInfixOf ">=" xs &&
                                                             ((read ys) :: Double) >=
                                                             (read (TR.subRegex (TR.mkRegex ">=") xs "") :: Double)
                                                         then True
                                                         else False 
        --smallPredicateNumericTrinaryMiddle
        smallPredicateNumericTrinaryMiddle :: String -> String -> Bool
        smallPredicateNumericTrinaryMiddle [] _  = False
        smallPredicateNumericTrinaryMiddle _  [] = False
        smallPredicateNumericTrinaryMiddle xs ys = if DL.isInfixOf "<=" xs &&
                                                      ((read ys) :: Double) >=
                                                      ((read :: String -> Double) (DL.head (DLS.splitOn "<=" xs))) &&
                                                      ((read ys) :: Double) <=
                                                      ((read :: String -> Double) (DL.last (DLS.splitOn "<=" xs)))
                                                       then True
                                                       else if DL.isInfixOf ">=" xs &&
                                                               ((read ys) :: Double) <=
                                                               ((read :: String -> Double) (DL.head (DLS.splitOn ">=" xs))) &&
                                                               ((read ys) :: Double) >=
                                                               ((read :: String -> Double) (DL.last (DLS.splitOn ">=" xs)))
                                                           then True
                                                           else False
        --smallPredicateNumericTrinaryTail
        smallPredicateNumericTrinaryTail :: String -> String -> Bool
        smallPredicateNumericTrinaryTail [] _  = False
        smallPredicateNumericTrinaryTail _  [] = False
        smallPredicateNumericTrinaryTail xs ys = if DL.isInfixOf "<=" xs &&
                                                    ((read ys) :: Double) >= 
                                                    (read (TR.subRegex (TR.mkRegex "<=") xs "") :: Double)
                                                     then True
                                                     else if DL.isInfixOf ">=" xs &&
                                                             ((read ys) :: Double) <=
                                                             (read (TR.subRegex (TR.mkRegex ">=") xs "") :: Double)
                                                         then True
                                                         else False
        --smallPredicateCharTrinary     
        smallPredicateCharTrinary :: [String] -> [String] -> Bool
        smallPredicateCharTrinary [] _  = False
        smallPredicateCharTrinary _  [] = False
        smallPredicateCharTrinary xs ys = if ys `isSubsetOf` xs
                                              then True
                                              else False
        --correctListOrdering
        correctListOrdering :: [(String,Int,Int,String)] -> [(String,Int,Int)] -> [(String,Int,Int,String)]
        correctListOrdering [] [] = []
        correctListOrdering _  [] = []
        correctListOrdering [] _  = []
        correctListOrdering xs (y:ys) = (DL.filter (\x -> quadrupletFst x == tripletFst y &&
                                                          quadrupletSnd x == tripletSnd y &&
                                                          quadrupletThrd x == tripletThrd y) xs) 
                                     ++ (correctListOrdering xs ys)



        --------------------------------

--specificFilters -> This function will
--applied the prepared specific filtration
--elucidated by filterFields.
specificFilters :: [[String]] -> [[(String,Int,Int)]] -> [[(String,String)]]
specificFilters [] [] = []
specificFilters [] _  = []
specificFilters _  [] = []
specificFilters (x:xs) ys = do
    --Grab the sublist of ys that matches x (on head).
    let matchedys = customListFilter x ys
    --Grab the entire portion of matchedys that isn't the column header.
    let onlydata = customOnlyDataFilter x matchedys
    --Grab the entire portion of matchedys that is the column header.
    let notdata = customNotDataFilter x matchedys
    --Grab !! 0 of x.
    let zerox = x DL.!! 0
    --Grab !! 2 of x.
    let onex = x DL.!! 2
    --Grab !! 3 of x.
    let twox = x DL.!! 3
    --Grab !! 4 of x.
    let threex = x DL.!! 4
    --Walk through all possibilities of entire field of delimiters.
    if | (zerox == "BINARY") &&
         (isNotAlphaList onex) &&
         (DL.elem '+' twox) &&
         (DL.isInfixOf ">=" threex) &&
         ((fst (tuplifyTwo (DLS.splitOn "," onex))) == "x") ->
         [((DL.map (\(x,_,_) -> (x,"HEADER")) notdata) ++
          (DL.map (\(y,_,_) -> if ',' `DL.elem` y
                                   then if ((DTuple.uncurry (+) (mapTuple (read :: String -> Int) (tuplifyTwo (DLS.splitOneOf ";:," y)))) >=
                                           (read (TR.subRegex (TR.mkRegex "^>=") threex ""))) then (y,"BINARYYES") else (y,"BINARYNO")
                                   else (y,"NA")) onlydata))] ++ (specificFilters xs ys)
       | (zerox == "BINARY") &&
         (isNotAlphaList onex) &&
         (DL.elem '/' twox) &&
         (DL.isInfixOf ">=" threex) &&
         ((fst (tuplifyTwo (DLS.splitOn "," onex))) == "x") ->
         [((DL.map (\(x,_,_) -> (x,"HEADER")) notdata) ++
          (DL.map (\(y,_,_) -> if ',' `DL.elem` y
                                   then if ((DTuple.uncurry (/) (mapTuple (read :: String -> Double) (tuplifyTwo (DLS.splitOneOf ";:," y)))) >=
                                           (read (TR.subRegex (TR.mkRegex "^>=") threex ""))) then (y,"BINARYYES") else (y,"BINARYNO")
                                   else (y,"NA")) onlydata))] ++ (specificFilters xs ys)
       | (zerox == "BINARY") &&
         (isNotAlphaList onex) &&
         (DL.elem '+' twox) &&
         (DL.isInfixOf ">=" threex) &&
         ((fst (tuplifyTwo (DLS.splitOn "," onex))) == "y") ->
         [((DL.map (\(x,_,_) -> (x,"HEADER")) notdata) ++
          (DL.map (\(y,_,_) -> if ',' `DL.elem` y
                                   then if ((DTuple.uncurry (+) (DTuple.swap (mapTuple (read :: String -> Int) (tuplifyTwo (DLS.splitOneOf ";:," y))))) >=
                                           (read (TR.subRegex (TR.mkRegex "^>=") threex ""))) then (y,"BINARYYES") else (y,"BINARYNO")
                                   else (y,"NA")) onlydata))] ++ (specificFilters xs ys)
       | (zerox == "BINARY") &&
         (isNotAlphaList onex) &&
         (DL.elem '/' twox) &&
         (DL.isInfixOf ">=" threex) &&
         ((fst (tuplifyTwo (DLS.splitOn "," onex))) == "y") ->
         [((DL.map (\(x,_,_) -> (x,"HEADER")) notdata) ++
          (DL.map (\(y,_,_) -> if ',' `DL.elem` y
                                   then if ((DTuple.uncurry (/) (DTuple.swap (mapTuple (read :: String -> Double) (tuplifyTwo (DLS.splitOneOf ";:," y))))) >=
                                           (read (TR.subRegex (TR.mkRegex "^>=") threex ""))) then (y,"BINARYYES") else (y,"BINARYNO")
                                   else (y,"NA")) onlydata))] ++ (specificFilters xs ys)
       | (zerox == "BINARY") &&
         (isNotAlphaList onex) &&
         (DL.elem '+' twox) &&
         (DL.isInfixOf "<=" threex) &&
         ((fst (tuplifyTwo (DLS.splitOn "," onex))) == "x") ->
         [((DL.map (\(x,_,_) -> (x,"HEADER")) notdata) ++
          (DL.map (\(y,_,_) -> if ',' `DL.elem` y
                                   then if ((DTuple.uncurry (+) (mapTuple (read :: String -> Int) (tuplifyTwo (DLS.splitOneOf ";:," y)))) <=
                                           (read (TR.subRegex (TR.mkRegex "^<=") threex ""))) then (y,"BINARYYES") else (y,"BINARYNO")
                                   else (y,"NA")) onlydata))] ++ (specificFilters xs ys)
       | (zerox == "BINARY") &&
         (isNotAlphaList onex) &&
         (DL.elem '/' twox) &&
         (DL.isInfixOf "<=" threex) &&
         ((fst (tuplifyTwo (DLS.splitOn "," onex))) == "x") ->
         [((DL.map (\(x,_,_) -> (x,"HEADER")) notdata) ++
          (DL.map (\(y,_,_) -> if ',' `DL.elem` y
                                   then if ((DTuple.uncurry (/) (mapTuple (read :: String -> Double) (tuplifyTwo (DLS.splitOneOf ";:," y)))) <=
                                           (read (TR.subRegex (TR.mkRegex "^<=") threex ""))) then (y,"BINARYYES") else (y,"BINARYNO")
                                   else (y,"NA")) onlydata))] ++ (specificFilters xs ys)
       | (zerox == "BINARY") &&
         (isNotAlphaList onex) &&
         (DL.elem '+' twox) &&
         (DL.isInfixOf "<=" threex) &&
         ((fst (tuplifyTwo (DLS.splitOn "," onex))) == "y") ->
         [((DL.map (\(x,_,_) -> (x,"HEADER")) notdata) ++
          (DL.map (\(y,_,_) -> if ',' `DL.elem` y
                                   then if ((DTuple.uncurry (+) (DTuple.swap (mapTuple (read :: String -> Int) (tuplifyTwo (DLS.splitOneOf ";:," y))))) <=
                                           (read (TR.subRegex (TR.mkRegex "^<=") threex ""))) then (y,"BINARYYES") else (y,"BINARYNO")
                                   else (y,"NA")) onlydata))] ++ (specificFilters xs ys)
       | (zerox == "BINARY") &&
         (isNotAlphaList onex) &&
         (DL.elem '/' twox) &&
         (DL.isInfixOf "<=" threex) &&
         ((fst (tuplifyTwo (DLS.splitOn "," onex))) == "y") ->
         [((DL.map (\(x,_,_) -> (x,"HEADER")) notdata) ++
          (DL.map (\(y,_,_) -> if ',' `DL.elem` y
                                   then if ((DTuple.uncurry (/) (DTuple.swap (mapTuple (read :: String -> Double) (tuplifyTwo (DLS.splitOneOf ";:," y))))) <=
                                           (read (TR.subRegex (TR.mkRegex "^<=") threex ""))) then (y,"BINARYYES") else (y,"BINARYNO")
                                   else (y,"NA")) onlydata))] ++ (specificFilters xs ys)
       | (zerox == "BINARY") &&
         (isNotAlphaList onex) &&
         (DL.elem '-' twox) &&
         (DL.isInfixOf ">=" threex) &&
         ((fst (tuplifyTwo (DLS.splitOn "," onex))) == "x") ->
         [((DL.map (\(x,_,_) -> (x,"HEADER")) notdata) ++
          (DL.map (\(y,_,_) -> if ',' `DL.elem` y
                                   then if ((DTuple.uncurry (-) (mapTuple (read :: String -> Int) (tuplifyTwo (DLS.splitOneOf ";:," y)))) >=
                                           (read (TR.subRegex (TR.mkRegex "^>=") threex ""))) then (y,"BINARYYES") else (y,"BINARYNO")
                                   else (y,"NA")) onlydata))] ++ (specificFilters xs ys)
       | (zerox == "BINARY") &&
         (isNotAlphaList onex) &&
         (DL.elem '-' twox) &&
         (DL.isInfixOf ">=" threex) &&
         ((fst (tuplifyTwo (DLS.splitOn "," onex))) == "y") ->
         [((DL.map (\(x,_,_) -> (x,"HEADER")) notdata) ++
          (DL.map (\(y,_,_) -> if ',' `DL.elem` y
                                   then if ((DTuple.uncurry (-) (DTuple.swap (mapTuple (read :: String -> Int) (tuplifyTwo (DLS.splitOneOf ";:," y))))) >=
                                           (read (TR.subRegex (TR.mkRegex "^>=") threex ""))) then (y,"BINARYYES") else (y,"BINARYNO")
                                   else (y,"NA")) onlydata))] ++ (specificFilters xs ys)
       | (zerox == "BINARY") &&
         (isNotAlphaList onex) &&
         (DL.elem '-' twox) &&
         (DL.isInfixOf "<=" threex) &&
         ((fst (tuplifyTwo (DLS.splitOn "," onex))) == "x") ->
         [((DL.map (\(x,_,_) -> (x,"HEADER")) notdata) ++
          (DL.map (\(y,_,_) -> if ',' `DL.elem` y
                                   then if ((DTuple.uncurry (-) (mapTuple (read :: String -> Int) (tuplifyTwo (DLS.splitOneOf ";:," y)))) <=
                                           (read (TR.subRegex (TR.mkRegex "^<=") threex ""))) then (y,"BINARYYES") else (y,"BINARYNO")
                                   else (y,"NA")) onlydata))] ++ (specificFilters xs ys)
       | (zerox == "BINARY") &&
         (isNotAlphaList onex) &&
         (DL.elem '-' twox) &&
         (DL.isInfixOf "<=" threex) &&
         ((fst (tuplifyTwo (DLS.splitOn "," onex))) == "y") ->
         [((DL.map (\(x,_,_) -> (x,"HEADER")) notdata) ++
          (DL.map (\(y,_,_) -> if ',' `DL.elem` y
                                   then if ((DTuple.uncurry (-) (DTuple.swap (mapTuple (read :: String -> Int) (tuplifyTwo (DLS.splitOneOf ";:," y))))) >=
                                           (read (TR.subRegex (TR.mkRegex "^<=") threex ""))) then (y,"BINARYYES") else (y,"BINARYNO")
                                   else (y,"NA")) onlydata))] ++ (specificFilters xs ys)
       | (zerox == "BINARY") &&
         (isNotAlphaList onex) &&
         (DL.elem '|' twox) &&
         (DL.isInfixOf ">=" threex) &&
         ((fst (tuplifyTwo (DLS.splitOn "," onex))) == "x") &&
         (snd (tuplifyTwo (DLS.splitOn "," onex)) == "_") ->
         [((DL.map (\(x,_,_) -> (x,"HEADER")) notdata) ++
          (DL.map (\(y,_,_) -> if ',' `DL.elem` y
                                   then if (((read :: String -> Int) (fst (tuplifyTwo (DLS.splitOneOf ";:," y)))) >=
                                           (read (TR.subRegex (TR.mkRegex "^>=") threex ""))) then (y,"BINARYYES") else (y,"BINARYNO")
                                   else (y,"NA")) onlydata))] ++ (specificFilters xs ys)
       | (zerox == "BINARY") &&
         (isNotAlphaList onex) &&
         (DL.elem '|' twox) &&
         (DL.isInfixOf ">=" threex) &&
         ((fst (tuplifyTwo (DLS.splitOn "," onex))) == "_") &&
         (snd (tuplifyTwo (DLS.splitOn "," onex)) == "y") ->
         [((DL.map (\(x,_,_) -> (x,"HEADER")) notdata) ++
          (DL.map (\(y,_,_) -> if ',' `DL.elem` y
                                   then if (((read :: String -> Int) (snd (tuplifyTwo (DLS.splitOneOf ";:," y)))) >=
                                           (read (TR.subRegex (TR.mkRegex "^>=") threex ""))) then (y,"BINARYYES") else (y,"BINARYNO")
                                   else (y,"NA")) onlydata))] ++ (specificFilters xs ys)
       | (zerox == "BINARY") &&
         (isNotAlphaList onex) &&
         (DL.elem '|' twox) &&
         (DL.isInfixOf "<=" threex) &&
         ((fst (tuplifyTwo (DLS.splitOn "," onex))) == "x") &&
         (snd (tuplifyTwo (DLS.splitOn "," onex)) == "_") ->
         [((DL.map (\(x,_,_) -> (x,"HEADER")) notdata) ++
          (DL.map (\(y,_,_) -> if ',' `DL.elem` y
                                   then if (((read :: String -> Int) (fst (tuplifyTwo (DLS.splitOneOf ";:," y)))) <=
                                           (read (TR.subRegex (TR.mkRegex "^<=") threex ""))) then (y,"BINARYYES") else (y,"BINARYNO")
                                   else (y,"NA")) onlydata))] ++ (specificFilters xs ys)
       | (zerox == "BINARY") &&
         (isNotAlphaList onex) &&
         (DL.elem '|' twox) &&
         (DL.isInfixOf "<=" threex) &&
         ((fst (tuplifyTwo (DLS.splitOn "," onex))) == "_") &&
         (snd (tuplifyTwo (DLS.splitOn "," onex)) == "y") ->
         [((DL.map (\(x,_,_) -> (x,"HEADER")) notdata) ++
          (DL.map (\(y,_,_) -> if ',' `DL.elem` y
                                   then if (((read :: String -> Int) (snd (tuplifyTwo (DLS.splitOneOf ";:," y)))) <=
                                           (read (TR.subRegex (TR.mkRegex "^<=") threex ""))) then (y,"BINARYYES") else (y,"BINARYNO")
                                   else (y,"NA")) onlydata))] ++ (specificFilters xs ys)
       | (zerox == "BINARY") &&
         (isAlphaList onex) &&
         (DL.elem '|' twox) &&
         (DL.isInfixOf ">=" threex) ->
         [((DL.map (\(x,_,_) -> (x,"HEADER")) notdata) ++
          ((DL.map (\(y,_,_) -> if not (y =~ "NA" :: Bool) ||
                                   not (y =~ "N/A" :: Bool)
                                    then if (((read :: String -> Double) y) >=
                                            ((read :: String -> Double) (TR.subRegex (TR.mkRegex "^>=") threex ""))) then (y,"BINARYYES") else (y,"BINARYNO")
                                    else (y,"NA")) onlydata)))] ++ (specificFilters xs ys)
       | (zerox == "BINARY") &&
         (isAlphaList onex) &&
         (DL.elem '|' twox) &&
         (DL.isInfixOf "<=" threex) ->
         [((DL.map (\(x,_,_) -> (x,"HEADER")) notdata) ++
          ((DL.map (\(y,_,_) -> if not (y =~ "NA" :: Bool) ||
                                   not (y =~ "N/A" :: Bool)
                                    then if (((read :: String -> Double) y) <=
                                            ((read :: String -> Double) (TR.subRegex (TR.mkRegex "^<=") threex ""))) then (y,"BINARYYES") else (y,"BINARYNO")
                                    else (y,"NA")) onlydata)))] ++ (specificFilters xs ys)
       | (zerox == "BINARY") &&
         (isAlphaList onex) &&
         (DL.elem '|' twox) &&
         (DL.isInfixOf "==" threex) ->
           [((DL.map (\(x,_,_) -> (x,"HEADER")) notdata) ++
            (DL.concat (equalityListCheckBinary (DLS.splitOneOf "%" (TR.subRegex (TR.mkRegex "^==") threex "")) onlydata)))] 
         ++ (specificFilters xs ys)
       | (zerox == "TRINARY") &&
         (isAlphaList onex) &&
         (DL.elem '|' twox) ->
           [((DL.map (\(x,_,_) -> (x,"HEADER")) notdata) ++
            (DL.concat (equalityListCheckTrinary (DL.map (DLS.splitOn "%") (DLS.splitOn "#" threex)) onlydata)))] 
         ++ (specificFilters xs ys)
       | otherwise -> specificFilters xs ys

{----------------------------}
