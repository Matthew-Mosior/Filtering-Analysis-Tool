{-=Filtering-Analysis-Tool (FAT): A Haskell-based solution to=-}
{-=analyze filtering schemes applied to tab delimited data.=-}
{-=Author: Matthew Mosior=-}
{-=Version: 1.0=-}
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


{-Import modules.-}

import Common

{----------------}


{-Imports-}

import Control.Arrow as CA
import Control.Monad as CM
import Data.Char as DC
import Data.Foldable as DF
import Data.List as DL
import Data.List.Split as DLS
import Data.Maybe as DMaybe
import Data.Tuple as DTuple
import Data.Tree as DTree
import Text.Regex as TR
import Text.Regex.TDFA as TRP

{---------}


{-Custom specificFilters Datatype and related functions.-}

--Define SpecificFilter data tree.
data SFFilter a = SFRequirement (Ffields -> Bool)
                | Ilist a

--Define helper data tree functions for SpecificFilters.
iffsf :: (Ffields -> Bool) -> Forest (SFFilter a) -> Tree (SFFilter a)
iffsf = Node . SFRequirement

addilist :: a -> Tree (SFFilter a)
addilist = flip Node [] . Ilist

--Define tree search function.
specificFiltersDecide :: Ffields -> Tree (SFFilter a) -> Maybe a
specificFiltersDecide x (Node (Ilist y) _) = Just y
specificFiltersDecide x (Node (SFRequirement p) subtree)
    | p x = asum $ map (specificFiltersDecide x) subtree
    | otherwise = Nothing

--Define binaryCompareType.
binaryCompareType :: Ffields -> Bool
binaryCompareType xs = if | (extractCompareType xs) == "BINARY"
                          -> True
                          | otherwise 
                          -> False

--Define trinaryCompareType.
trinaryCompareType :: Ffields -> Bool
trinaryCompareType xs = if | (extractCompareType xs) == "TRINARY"
                           -> True
                           | otherwise 
                           -> False

--Define isNotAlphaListCompareFields
isNotAlphaListCompareFieldType :: Ffields -> Bool
isNotAlphaListCompareFieldType xs = if | isNotAlphaList (extractCompareFieldType xs)
                                       -> True
                                       | otherwise 
                                       -> False 

--Define isAlphaListCompareFields
isAlphaListCompareFieldType :: Ffields -> Bool
isAlphaListCompareFieldType xs = if | isAlphaList (extractCompareFieldType xs)
                                    -> True
                                    | otherwise
                                    -> False

--Define elemPlusCompareOperator
elemPlusCompareOperator :: Ffields -> Bool
elemPlusCompareOperator xs = if | DL.elem '+' (extractCompareOperator xs) 
                                -> True
                                | otherwise 
                                -> False

--Define elemMinusCompareOperator
elemMinusCompareOperator :: Ffields -> Bool
elemMinusCompareOperator xs = if | DL.elem '-' (extractCompareOperator xs)
                                 -> True
                                 | otherwise
                                 -> False

--Define elemDivisionSignCompareOperator
elemDivisionSignCompareOperator :: Ffields -> Bool
elemDivisionSignCompareOperator xs = if | DL.elem '/' (extractCompareOperator xs)
                                        -> True
                                        | otherwise
                                        -> False

--Define elemPipeCompareOperator
elemPipeCompareOperator :: Ffields -> Bool
elemPipeCompareOperator xs = if | DL.elem '|' (extractCompareOperator xs)
                                -> True
                                | otherwise
                                -> False

--Define isInfixOfLessThanCompareString
isInfixOfLessThanCompareString :: Ffields -> Bool
isInfixOfLessThanCompareString xs = if | DL.isInfixOf ">=" (extractCompareString xs)
                                       -> True
                                       | otherwise
                                       -> False

--Define isInfixOfGreaterThanCompareString
isInfixOfGreaterThanCompareString :: Ffields -> Bool
isInfixOfGreaterThanCompareString xs = if | DL.isInfixOf "<=" (extractCompareString xs)
                                          -> True
                                          | otherwise
                                          -> False

--Define isInfixOfEqualSignCompareString
isInfixOfEqualSignCompareString :: Ffields -> Bool
isInfixOfEqualSignCompareString xs = if | DL.isInfixOf "==" (extractCompareString xs)
                                        -> True
                                        | otherwise
                                        -> False

--checkCompareFieldTypeCommaFstX
checkCompareFieldTypeCommaFstX :: Ffields -> Bool
checkCompareFieldTypeCommaFstX xs = if | ((fst (tuplifyTwo (DLS.splitOn "," (extractCompareFieldType xs)))) == "x")
                                       -> True
                                       | otherwise
                                       -> False

--checkCompareFieldTypeCommaFstY
checkCompareFieldTypeCommaFstY :: Ffields -> Bool
checkCompareFieldTypeCommaFstY xs = if | ((fst (tuplifyTwo (DLS.splitOn "," (extractCompareFieldType xs)))) == "y")
                                       -> True
                                       | otherwise
                                       -> False

--checkCompareFieldTypeCommaSndY
checkCompareFieldTypeCommaSndY :: Ffields -> Bool
checkCompareFieldTypeCommaSndY xs = if | ((snd (tuplifyTwo (DLS.splitOn "," (extractCompareFieldType xs)))) == "y")
                                       -> True
                                       | otherwise
                                       -> False

--checkCompareFieldTypeCommaFstUnderscore
checkCompareFieldTypeCommaFstUnderscore :: Ffields -> Bool
checkCompareFieldTypeCommaFstUnderscore xs = if | ((fst (tuplifyTwo (DLS.splitOn "," (extractCompareFieldType xs)))) == "_")
                                                -> True
                                                | otherwise
                                                -> False

--checkCompareFieldTypeCommaSndUnderscore
checkCompareFieldTypeCommaSndUnderscore :: Ffields -> Bool
checkCompareFieldTypeCommaSndUnderscore xs = if | ((snd (tuplifyTwo (DLS.splitOn "," (extractCompareFieldType xs)))) == "_")
                                                -> True
                                                | otherwise
                                                -> False

{--------------------------------------------------------}

{-specificFilters functions.-}


--customListFilter -> This function will
--perform a custom, regex-based filtration
--using the nested predicate functions.
customListFilter :: String -> [[(String,Int,Int)]] -> [(String,Int,Int)]
customListFilter [] _      = []
customListFilter _  []     = []
customListFilter xs (y:ys) = if smallCustomPredicate xs y
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
customOnlyDataFilter :: String -> [(String,Int,Int)] -> [(String,Int,Int)]
customOnlyDataFilter [] [] = []
customOnlyDataFilter [] _  = []
customOnlyDataFilter _  [] = []
customOnlyDataFilter xs ys = smallCustomFilter xs ys
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
customNotDataFilter :: String -> [(String,Int,Int)] -> [(String,Int,Int)]
customNotDataFilter [] [] = []
customNotDataFilter [] _  = []
customNotDataFilter _  [] = []
customNotDataFilter xs ys = smallCustomFilter xs ys
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
equalityListCheckBinary :: [String] -> [(String,Int,Int)] -> [[(String,Int,Int,String)]]
equalityListCheckBinary _ []  = []
equalityListCheckBinary [] _  = []
equalityListCheckBinary xs (y:ys) = [smallEqualityListCheckBinary xs y] ++ (equalityListCheckBinary xs ys)
    where
        --Nested function definitions.--
        --smallEqualityListCheckBinary
        smallEqualityListCheckBinary :: [String] -> (String,Int,Int) -> [(String,Int,Int,String)]
        smallEqualityListCheckBinary [] (_,_,_)  = []
        smallEqualityListCheckBinary _  ([],_,_) = []
        smallEqualityListCheckBinary xs y        = if (tripletFst y) /= "NA" ||
                                                      (tripletFst y) /= "N/A"
                                                       then if smallPredicateBinary xs [tripletFst y]
                                                           then [quadrupletTransform (y,"BINARYYES")]
                                                           else [quadrupletTransform (y,"BINARYNO")]
                                                       else [quadrupletTransform (y,"NA")]
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
equalityListCheckTrinary :: [[String]] -> [(String,Int,Int)] -> [[(String,Int,Int,String)]]
equalityListCheckTrinary _ []  = []
equalityListCheckTrinary [] _  = []
equalityListCheckTrinary xs ys = [(correctListOrdering
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
specificFilters :: [String] -> [[(String,Int,Int)]] -> [[(String,Int,Int,String)]]
specificFilters [] [] = []
specificFilters [] _  = []
specificFilters _  [] = []
specificFilters (x:xs) ys = do
    --Search the specificfilterstree decision tree on ffields.
    (DMaybe.fromJust (specificFiltersDecide ffields specificfilterstree)) ++ (specificFilters xs ys)
        where
            --Split x on ?:~.
            ffieldsplit = DLS.splitOneOf "?:~" x
            --Saturate Ffields data type with ffieldsplit.
            ffields = Ffields { comparetype      = ffieldsplit DL.!! 0
                              , comparefield     = ffieldsplit DL.!! 1
                              , comparefieldtype = ffieldsplit DL.!! 2
                              , compareoperator  = ffieldsplit DL.!! 3
                              , comparestring    = ffieldsplit DL.!! 4 }
            --Grab the sublist of ys that matches x (on head).
            matchedys = customListFilter (extractCompareField ffields) ys
            --Grab the entire portion of matchedys that isn't the column header.
            onlydata = customOnlyDataFilter (extractCompareField ffields) matchedys
            --Grab the entire portion of matchedys that is the column header.
            notdata = customNotDataFilter (extractCompareField ffields) matchedys
            --Define specificfilterstree data (decision) tree.
            specificfilterstree =
                iffsf (const True) [
                    iffsf (binaryCompareType) [
                        iffsf (isNotAlphaListCompareFieldType) [
                            iffsf (elemPlusCompareOperator) [
                                iffsf (isInfixOfLessThanCompareString) [
                                    iffsf (checkCompareFieldTypeCommaFstX) [
                                        addilist ([((DL.map (\x -> quadrupletTransform (x,"HEADER")) notdata) ++
                                                   (DL.map (\allys@(y,_,_) -> 
                                                       if ',' `DL.elem` y
                                                           then if ((DTuple.uncurry (+) (mapTuple (read :: String -> Int) 
                                                                   (tuplifyTwo (DLS.splitOneOf ";:," y)))) >=
                                                                   (read (TR.subRegex (TR.mkRegex "^>=") (extractCompareString ffields) "")))
                                                               then quadrupletTransform (allys,"BINARYYES")
                                                               else quadrupletTransform (allys,"BINARYNO")
                                                           else quadrupletTransform (allys,"NA")) onlydata))]) 
                                    ],
                                    iffsf (checkCompareFieldTypeCommaFstY) [
                                        addilist ([((DL.map (\x -> quadrupletTransform (x,"HEADER")) notdata) ++
                                                   (DL.map (\allys@(y,_,_) -> 
                                                       if ',' `DL.elem` y
                                                           then if ((DTuple.uncurry (+) 
                                                                   (DTuple.swap (mapTuple (read :: String -> Int) 
                                                                   (tuplifyTwo (DLS.splitOneOf ";:," y))))) >=
                                                                   (read (TR.subRegex (TR.mkRegex "^>=") (extractCompareString ffields) "")))
                                                               then quadrupletTransform (allys,"BINARYYES")
                                                               else quadrupletTransform (allys,"BINARYNO")
                                                           else quadrupletTransform (allys,"NA")) onlydata))])
                                    ]
                                ], 
                                iffsf (isInfixOfGreaterThanCompareString) [
                                    iffsf (checkCompareFieldTypeCommaFstX) [
                                        addilist ([((DL.map (\x -> quadrupletTransform (x,"HEADER")) notdata) ++
                                                   (DL.map (\allys@(y,_,_) -> 
                                                       if ',' `DL.elem` y
                                                           then if ((DTuple.uncurry (+) (mapTuple (read :: String -> Int) 
                                                                   (tuplifyTwo (DLS.splitOneOf ";:," y)))) <=
                                                                   (read (TR.subRegex (TR.mkRegex "^<=") (extractCompareString ffields) "")))
                                                               then quadrupletTransform (allys,"BINARYYES")
                                                               else quadrupletTransform (allys,"BINARYNO")
                                                           else quadrupletTransform (allys,"NA")) onlydata))])
                                    ],
                                    iffsf (checkCompareFieldTypeCommaFstY) [
                                        addilist ([((DL.map (\x -> quadrupletTransform (x,"HEADER")) notdata) ++
                                                   (DL.map (\allys@(y,_,_) -> 
                                                       if ',' `DL.elem` y
                                                           then if ((DTuple.uncurry (+) 
                                                                   (DTuple.swap (mapTuple (read :: String -> Int) 
                                                                   (tuplifyTwo (DLS.splitOneOf ";:," y))))) <=
                                                                   (read (TR.subRegex (TR.mkRegex "^<=") (extractCompareString ffields) "")))
                                                               then quadrupletTransform (allys,"BINARYYES")
                                                               else quadrupletTransform (allys,"BINARYNO")
                                                           else quadrupletTransform (allys,"NA")) onlydata))])
                                    ]
                                ] 
                            ],  
                            iffsf (elemMinusCompareOperator) [
                                iffsf (isInfixOfLessThanCompareString) [
                                    iffsf (checkCompareFieldTypeCommaFstX) [
                                        addilist ([((DL.map (\x -> quadrupletTransform (x,"HEADER")) notdata) ++
                                                   (DL.map (\allys@(y,_,_) -> 
                                                       if ',' `DL.elem` y
                                                           then if ((DTuple.uncurry (-) (mapTuple (read :: String -> Int) 
                                                                   (tuplifyTwo (DLS.splitOneOf ";:," y)))) >=
                                                                   (read (TR.subRegex (TR.mkRegex "^>=") (extractCompareString ffields) "")))
                                                               then quadrupletTransform (allys,"BINARYYES")
                                                               else quadrupletTransform (allys,"BINARYNO")
                                                           else quadrupletTransform (allys,"NA")) onlydata))])
                                    ],
                                    iffsf (checkCompareFieldTypeCommaFstY) [
                                        addilist ([((DL.map (\x -> quadrupletTransform (x,"HEADER")) notdata) ++
                                                   (DL.map (\allys@(y,_,_) -> 
                                                       if ',' `DL.elem` y
                                                           then if ((DTuple.uncurry (-) 
                                                                   (DTuple.swap (mapTuple (read :: String -> Int) 
                                                                   (tuplifyTwo (DLS.splitOneOf ";:," y))))) >=
                                                                   (read (TR.subRegex (TR.mkRegex "^>=") (extractCompareString ffields) "")))
                                                               then quadrupletTransform (allys,"BINARYYES")
                                                               else quadrupletTransform (allys,"BINARYNO")
                                                           else quadrupletTransform (allys,"NA")) onlydata))])
                                    ]
                                ],                              
                                iffsf (isInfixOfGreaterThanCompareString) [
                                    iffsf (checkCompareFieldTypeCommaFstX) [
                                        addilist ([((DL.map (\x -> quadrupletTransform (x,"HEADER")) notdata) ++
                                                   (DL.map (\allys@(y,_,_) -> 
                                                       if ',' `DL.elem` y
                                                           then if ((DTuple.uncurry (-) (mapTuple (read :: String -> Int) 
                                                                   (tuplifyTwo (DLS.splitOneOf ";:," y)))) <=
                                                                   (read (TR.subRegex (TR.mkRegex "^<=") (extractCompareString ffields) "")))
                                                               then quadrupletTransform (allys,"BINARYYES")
                                                               else quadrupletTransform (allys,"BINARYNO")
                                                           else quadrupletTransform (allys,"NA")) onlydata))])
                                    ],
                                    iffsf (checkCompareFieldTypeCommaFstY) [
                                        addilist ([((DL.map (\x -> quadrupletTransform (x,"HEADER")) notdata) ++
                                                   (DL.map (\allys@(y,_,_) -> 
                                                       if ',' `DL.elem` y
                                                           then if ((DTuple.uncurry (-) 
                                                                   (DTuple.swap (mapTuple (read :: String -> Int) 
                                                                   (tuplifyTwo (DLS.splitOneOf ";:," y))))) >=
                                                                   (read (TR.subRegex (TR.mkRegex "^<=") (extractCompareString ffields) "")))
                                                               then quadrupletTransform (allys,"BINARYYES")
                                                               else quadrupletTransform (allys,"BINARYNO")
                                                           else quadrupletTransform (allys,"NA")) onlydata))])
                                    ]
                                ]
                            ],  
                            iffsf (elemDivisionSignCompareOperator) [
                                iffsf (isInfixOfLessThanCompareString) [
                                    iffsf (checkCompareFieldTypeCommaFstX) [
                                        addilist ([((DL.map (\x -> quadrupletTransform (x,"HEADER")) notdata) ++
                                                   (DL.map (\allys@(y,_,_) -> 
                                                       if ',' `DL.elem` y
                                                           then if ((DTuple.uncurry (/) (mapTuple (read :: String -> Double) 
                                                                   (tuplifyTwo (DLS.splitOneOf ";:," y)))) >=
                                                                   (read (TR.subRegex (TR.mkRegex "^>=") (extractCompareString ffields) "")))
                                                               then quadrupletTransform (allys,"BINARYYES")
                                                               else quadrupletTransform (allys,"BINARYNO")
                                                           else quadrupletTransform (allys,"NA")) onlydata))])
                                    ],
                                    iffsf (checkCompareFieldTypeCommaFstY) [
                                        addilist ([((DL.map (\x -> quadrupletTransform (x,"HEADER")) notdata) ++
                                                   (DL.map (\allys@(y,_,_) -> 
                                                       if ',' `DL.elem` y
                                                           then if ((DTuple.uncurry (/) 
                                                                   (DTuple.swap (mapTuple (read :: String -> Double) 
                                                                   (tuplifyTwo (DLS.splitOneOf ";:," y))))) >=
                                                                   (read (TR.subRegex (TR.mkRegex "^>=") (extractCompareString ffields) "")))
                                                               then quadrupletTransform (allys,"BINARYYES")
                                                               else quadrupletTransform (allys,"BINARYNO")
                                                           else quadrupletTransform (allys,"NA")) onlydata))])
                                    ]
                                ],
                                iffsf (isInfixOfGreaterThanCompareString) [
                                    iffsf (checkCompareFieldTypeCommaFstX) [
                                        addilist ([((DL.map (\x -> quadrupletTransform (x,"HEADER")) notdata) ++
                                                   (DL.map (\allys@(y,_,_) -> 
                                                       if ',' `DL.elem` y
                                                           then if ((DTuple.uncurry (/) (mapTuple (read :: String -> Double) 
                                                                   (tuplifyTwo (DLS.splitOneOf ";:," y)))) <=
                                                                   (read (TR.subRegex (TR.mkRegex "^<=") (extractCompareString ffields) "")))
                                                               then quadrupletTransform (allys,"BINARYYES")
                                                               else quadrupletTransform (allys,"BINARYNO")
                                                           else quadrupletTransform (allys,"NA")) onlydata))])
                                    ],
                                    iffsf (checkCompareFieldTypeCommaFstY) [
                                        addilist ([((DL.map (\x -> quadrupletTransform (x,"HEADER")) notdata) ++
                                                   (DL.map (\allys@(y,_,_) -> 
                                                       if ',' `DL.elem` y
                                                           then if ((DTuple.uncurry (/) 
                                                                   (DTuple.swap (mapTuple (read :: String -> Double) 
                                                                   (tuplifyTwo (DLS.splitOneOf ";:," y))))) <=
                                                                   (read (TR.subRegex (TR.mkRegex "^<=") (extractCompareString ffields) "")))
                                                               then quadrupletTransform (allys,"BINARYYES")
                                                               else quadrupletTransform (allys,"BINARYNO")
                                                           else quadrupletTransform (allys,"NA")) onlydata))])
                                    ]
                                ]
                            ],
                            iffsf (elemPipeCompareOperator) [
                                iffsf (isInfixOfLessThanCompareString) [
                                    iffsf (checkCompareFieldTypeCommaFstX) [
                                        iffsf (checkCompareFieldTypeCommaSndUnderscore) [
                                            addilist ([((DL.map (\x -> quadrupletTransform (x,"HEADER")) notdata) ++
                                                       (DL.map (\allys@(y,_,_) -> 
                                                           if ',' `DL.elem` y
                                                               then if (((read :: String -> Int) 
                                                                       (fst (tuplifyTwo (DLS.splitOneOf ";:," y)))) >=
                                                                       (read (TR.subRegex (TR.mkRegex "^>=") (extractCompareString ffields) "")))
                                                                   then quadrupletTransform (allys,"BINARYYES")
                                                                   else quadrupletTransform (allys,"BINARYNO")
                                                               else quadrupletTransform (allys,"NA")) onlydata))])
                                        ]
                                    ],
                                    iffsf (checkCompareFieldTypeCommaFstUnderscore) [
                                        iffsf (checkCompareFieldTypeCommaSndY) [
                                            addilist ([((DL.map (\x -> quadrupletTransform (x,"HEADER")) notdata) ++
                                                       (DL.map (\allys@(y,_,_) -> 
                                                           if ',' `DL.elem` y
                                                               then if (((read :: String -> Int) 
                                                                       (snd (tuplifyTwo (DLS.splitOneOf ";:," y)))) >=
                                                                       (read (TR.subRegex (TR.mkRegex "^>=") (extractCompareString ffields) "")))
                                                                   then quadrupletTransform (allys,"BINARYYES")
                                                                   else quadrupletTransform (allys,"BINARYNO")
                                                               else quadrupletTransform (allys,"NA")) onlydata))])
                                        ]
                                    ]
                                ],
                                iffsf (isInfixOfGreaterThanCompareString) [
                                    iffsf (checkCompareFieldTypeCommaFstX) [
                                        iffsf (checkCompareFieldTypeCommaSndUnderscore) [
                                            addilist ([((DL.map (\x -> quadrupletTransform (x,"HEADER")) notdata) ++
                                                       (DL.map (\allys@(y,_,_) -> 
                                                           if ',' `DL.elem` y
                                                               then if (((read :: String -> Int) 
                                                                       (fst (tuplifyTwo (DLS.splitOneOf ";:," y)))) <=
                                                                       (read (TR.subRegex (TR.mkRegex "^<=") (extractCompareString ffields) "")))
                                                                   then quadrupletTransform (allys,"BINARYYES")
                                                                   else quadrupletTransform (allys,"BINARYNO")
                                                               else quadrupletTransform (allys,"NA")) onlydata))])
                                        ]
                                    ],
                                    iffsf (checkCompareFieldTypeCommaFstUnderscore) [
                                        iffsf (checkCompareFieldTypeCommaSndY) [
                                            addilist ([((DL.map (\x -> quadrupletTransform (x,"HEADER")) notdata) ++
                                                       (DL.map (\allys@(y,_,_) -> 
                                                           if ',' `DL.elem` y
                                                               then if (((read :: String -> Int) 
                                                                       (snd (tuplifyTwo (DLS.splitOneOf ";:," y)))) <=
                                                                       (read (TR.subRegex (TR.mkRegex "^<=") (extractCompareString ffields) "")))
                                                                   then quadrupletTransform (allys,"BINARYYES")
                                                                   else quadrupletTransform (allys,"BINARYNO")
                                                               else quadrupletTransform (allys,"NA")) onlydata))])
                                        ]
                                    ]
                                ]
                            ]
                        ],
                        iffsf (isAlphaListCompareFieldType) [
                            iffsf (elemPipeCompareOperator) [
                                iffsf (isInfixOfLessThanCompareString) [
                                    addilist ([((DL.map (\x -> quadrupletTransform (x,"HEADER")) notdata) ++
                                               ((DL.map (\allys@(y,_,_) -> 
                                                    if not (y =~ "NA" :: Bool) ||
                                                       not (y =~ "N/A" :: Bool)
                                                        then if (((read :: String -> Double) y) >=
                                                                ((read :: String -> Double) 
                                                                (TR.subRegex (TR.mkRegex "^>=") (extractCompareString ffields) "")))
                                                            then quadrupletTransform (allys,"BINARYYES")
                                                            else quadrupletTransform (allys,"BINARYNO")
                                                        else quadrupletTransform (allys,"NA")) onlydata)))]) 
                                ],
                                iffsf (isInfixOfGreaterThanCompareString) [
                                    addilist ([((DL.map (\x -> quadrupletTransform (x,"HEADER")) notdata) ++
                                                ((DL.map (\allys@(y,_,_) -> 
                                                     if not (y =~ "NA" :: Bool) ||
                                                        not (y =~ "N/A" :: Bool)
                                                         then if (((read :: String -> Double) y) <=
                                                                 ((read :: String -> Double) 
                                                                 (TR.subRegex (TR.mkRegex "^<=") (extractCompareString ffields) "")))
                                                             then quadrupletTransform (allys,"BINARYYES")
                                                             else quadrupletTransform (allys,"BINARYNO")
                                                         else quadrupletTransform (allys,"NA")) onlydata)))]) 
                                ],
                                iffsf (isInfixOfEqualSignCompareString) [
                                    addilist ([((DL.map (\x -> quadrupletTransform (x,"HEADER")) notdata) ++
                                               (DL.concat 
                                               (equalityListCheckBinary 
                                               (DLS.splitOneOf "%" 
                                               (TR.subRegex 
                                               (TR.mkRegex "^==") (extractCompareString ffields) "")) onlydata)))])
                                ]
                            ]
                        ]
                    ],
                    iffsf (trinaryCompareType) [
                        iffsf (isAlphaListCompareFieldType) [
                            iffsf (elemPipeCompareOperator) [ 
                                addilist ([((DL.map (\x -> quadrupletTransform (x,"HEADER")) notdata) ++
                                           (DL.concat 
                                           (equalityListCheckTrinary 
                                           (DL.map (DLS.splitOn "%") 
                                           (DLS.splitOn "#" (extractCompareString ffields))) onlydata)))])
                            ]
                        ]
                    ]
                ]
