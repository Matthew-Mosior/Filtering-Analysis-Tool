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
import YamlParser

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
data SFFilter a = SFRequirement (Filter -> Bool)
                | Ilist a

--Define helper data tree functions for SpecificFilters.
iffsf :: (Filter -> Bool) -> Forest (SFFilter a) -> Tree (SFFilter a)
iffsf = Node . SFRequirement

addilist :: a -> Tree (SFFilter a)
addilist = flip Node [] . Ilist

--Define tree search function.
specificFiltersDecide :: Filter -> Tree (SFFilter a) -> Maybe a
specificFiltersDecide x (Node (Ilist y) _) = Just y
specificFiltersDecide x (Node (SFRequirement p) subtree)
    | p x = asum $ map (specificFiltersDecide x) subtree
    | otherwise = Nothing

--Define binaryFilteringType.
binaryFilteringType :: Filter -> Bool
binaryFilteringType xs = if | (extractFilteringType xs) == "BINARY"
                            -> True
                            | otherwise 
                            -> False

--Define trinaryFilteringType.
trinaryFilteringType :: Filter -> Bool
trinaryFilteringType xs = if | (extractFilteringType xs) == "TRINARY"
                             -> True
                             | otherwise 
                             -> False

--Define isNotAlphaListCompareFields
isNotAlphaListFilteringColumnType :: Filter -> Bool
isNotAlphaListFilteringColumnType xs = if | isNotAlphaList (extractFilteringColumnType xs)
                                          -> True
                                          | otherwise 
                                          -> False 

--Define isAlphaListCompareFields
isAlphaListFilteringColumnType :: Filter -> Bool
isAlphaListFilteringColumnType xs = if | isAlphaList (extractFilteringColumnType xs)
                                       -> True
                                       | otherwise
                                       -> False

--Define elemPlusFilteringOperator
elemPlusFilteringOperator :: Filter -> Bool
elemPlusFilteringOperator xs = if | DL.elem '+' (extractFilteringOperator xs) 
                                  -> True
                                  | otherwise 
                                  -> False

--Define elemMinusFilteringOperator
elemMinusFilteringOperator :: Filter -> Bool
elemMinusFilteringOperator xs = if | DL.elem '-' (extractFilteringOperator xs)
                                   -> True
                                   | otherwise
                                   -> False

--Define elemDivisionSignFilteringOperator
elemDivisionSignFilteringOperator :: Filter -> Bool
elemDivisionSignFilteringOperator xs = if | DL.elem '/' (extractFilteringOperator xs)
                                          -> True
                                          | otherwise
                                          -> False

--Define elemPipeFilteringOperator
elemPipeFilteringOperator :: Filter -> Bool
elemPipeFilteringOperator xs = if | DL.elem '|' (extractFilteringOperator xs)
                                  -> True
                                  | otherwise
                                  -> False

--Define isInfixOfLessThanFilteringString
isInfixOfLessThanFilteringString :: Filter -> Bool
isInfixOfLessThanFilteringString xs = if | (extractBFSNumericOperator (extractFilteringString xs)) == ">="
                                         -> True
                                         | otherwise
                                         -> False

--Define isInfixOfGreaterThanFilteringString
isInfixOfGreaterThanFilteringString :: Filter -> Bool
isInfixOfGreaterThanFilteringString xs = if | (extractBFSNumericOperator (extractFilteringString xs)) == "<="
                                            -> True
                                            | otherwise
                                            -> False

--Define isInfixOfEqualSignFilteringString
isInfixOfEqualSignFilteringString :: Filter -> Bool
isInfixOfEqualSignFilteringString xs = if | (extractBFSStringOperator (extractFilteringString xs)) == "=="
                                          -> True
                                          | otherwise
                                          -> False

--checkFilteringColumnTypeCommaFstX
checkFilteringColumnTypeCommaFstX :: Filter -> Bool
checkFilteringColumnTypeCommaFstX xs = if | ((fst (tuplifyTwo (DLS.splitOn "," (extractFilteringColumnType xs)))) == "x")
                                          -> True
                                          | otherwise
                                          -> False

--checkFilteringColumnTypeCommaFstY
checkFilteringColumnTypeCommaFstY :: Filter -> Bool
checkFilteringColumnTypeCommaFstY xs = if | ((fst (tuplifyTwo (DLS.splitOn "," (extractFilteringColumnType xs)))) == "y")
                                          -> True
                                          | otherwise
                                          -> False

--checkFilteringColumnTypeCommaSndY
checkFilteringColumnTypeCommaSndY :: Filter -> Bool
checkFilteringColumnTypeCommaSndY xs = if | ((snd (tuplifyTwo (DLS.splitOn "," (extractFilteringColumnType xs)))) == "y")
                                          -> True
                                          | otherwise
                                          -> False

--checkFilteringColumnTypeCommaFstUnderscore
checkFilteringColumnTypeCommaFstUnderscore :: Filter -> Bool
checkFilteringColumnTypeCommaFstUnderscore xs = if | ((fst (tuplifyTwo (DLS.splitOn "," (extractFilteringColumnType xs)))) == "_")
                                                   -> True
                                                   | otherwise
                                                   -> False

--checkFilteringColumnTypeCommaSndUnderscore
checkFilteringColumnTypeCommaSndUnderscore :: Filter -> Bool
checkFilteringColumnTypeCommaSndUnderscore xs = if | ((snd (tuplifyTwo (DLS.splitOn "," (extractFilteringColumnType xs)))) == "_")
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
customListFilter xs (y:ys) = if | smallCustomPredicate xs y
                                -> y
                                | otherwise
                                -> customListFilter xs ys
    where
        --Nested function definitions.--
        --smallCustomPredicate
        smallCustomPredicate :: String -> [(String,Int,Int)] -> Bool
        smallCustomPredicate []     _       = False
        smallCustomPredicate _      []      = False
        smallCustomPredicate x ((y,_,_):ys) = if | (y == x :: Bool)
                                                 -> True
                                                 | otherwise
                                                 -> smallCustomPredicate x ys
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
         smallCustomFilter x  (y:ys) = if | smallCustomPredicate x y
                                          -> [y] ++ (smallCustomFilter x ys)
                                          | otherwise
                                          -> smallCustomFilter x ys
         --smallCustomPredicate
         smallCustomPredicate :: String -> (String,Int,Int) -> Bool
         smallCustomPredicate [] ([],_,_) = False
         smallCustomPredicate _  ([],_,_) = False
         smallCustomPredicate [] _        = False
         smallCustomPredicate x  (y,_,_)  = if | (y == x :: Bool)
                                               -> False
                                               | otherwise
                                               -> True
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
        smallCustomFilter x  (y:ys) = if | smallCustomPredicate x y
                                         -> [y] ++ (smallCustomFilter x ys)
                                         | otherwise
                                         -> smallCustomFilter x ys
        --smallCustomPredicate
        smallCustomPredicate :: String -> (String,Int,Int) -> Bool
        smallCustomPredicate [] ([],_,_) = False
        smallCustomPredicate _  ([],_,_) = False
        smallCustomPredicate [] _        = False
        smallCustomPredicate x  (y,_,_)  = if | (y == x :: Bool)
                                              -> True
                                              | otherwise
                                              -> False
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
        smallEqualityListCheckBinary xs y        = if | (tripletFst y) /= "NA" ||
                                                        (tripletFst y) /= "N/A"
                                                      -> if | smallPredicateBinary xs [tripletFst y]
                                                            -> [quadrupletTransform (y,"BINARYYES")]
                                                            | otherwise
                                                            -> [quadrupletTransform (y,"BINARYNO")]
                                                      | otherwise
                                                      -> [quadrupletTransform (y,"NA")]
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
        smallerEqualityListCheckTrinaryHead xs y        = if | (tripletFst y) /= "NA" ||
                                                               (tripletFst y) /= "N/A"
                                                             -> if | DL.any (\x -> DL.isInfixOf "<=" x) xs ||
                                                                     DL.any (\x -> DL.isInfixOf ">=" x) xs
                                                                   -> if | smallPredicateNumericTrinaryHead (DL.concat xs) 
                                                                                                            (tripletFst y)
                                                                         -> [quadrupletTransform (y,"TRINARYHEAD")]
                                                                         | otherwise
                                                                         -> []
                                                                   | smallPredicateCharTrinary xs [tripletFst y]
                                                                   -> [quadrupletTransform (y,"TRINARYHEAD")] 
                                                                   | otherwise
                                                                   -> []
                                                             | otherwise
                                                             -> []
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
        smallerEqualityListCheckTrinaryMiddle xs y        = if | (tripletFst y) /= "NA" ||
                                                                 (tripletFst y) /= "N/A"
                                                               -> if | DL.any (\x -> DL.isInfixOf "<=" x) xs ||
                                                                       DL.any (\x -> DL.isInfixOf ">=" x) xs
                                                                     -> if | smallPredicateNumericTrinaryMiddle (DL.concat xs) 
                                                                                                                (tripletFst y)
                                                                           -> [quadrupletTransform (y,"TRINARYMIDDLE")]
                                                                           | otherwise
                                                                           -> []
                                                                     | smallPredicateCharTrinary xs [tripletFst y]
                                                                     -> [quadrupletTransform (y,"TRINARYMIDDLE")]
                                                                     | otherwise
                                                                     -> []
                                                               | otherwise
                                                               -> []
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
        smallerEqualityListCheckTrinaryTail xs y        = if | (tripletFst y) /= "NA" ||
                                                               (tripletFst y) /= "N/A"
                                                             -> if | DL.any (\x -> DL.isInfixOf "<=" x) xs ||
                                                                     DL.any (\x -> DL.isInfixOf ">=" x) xs
                                                                   -> if | smallPredicateNumericTrinaryTail (DL.concat xs) 
                                                                                                            (tripletFst y)
                                                                         -> [quadrupletTransform (y,"TRINARYTAIL")]
                                                                         | otherwise
                                                                         -> []
                                                                   | smallPredicateCharTrinary xs [tripletFst y]
                                                                   -> [quadrupletTransform (y,"TRINARYTAIL")]
                                                                   | otherwise
                                                                   -> []
                                                             | otherwise
                                                             -> []
        --smallEqualityListCheckTrinaryNA
        smallEqualityListCheckTrinaryNA :: [(String,Int,Int)] -> [(String,Int,Int,String)]
        smallEqualityListCheckTrinaryNA []     = []
        smallEqualityListCheckTrinaryNA (x:xs) = (smallerEqualityListCheckTrinaryNA x)
                                              ++ (smallEqualityListCheckTrinaryNA xs)
        --smallerEqualityListCheckTrinaryNA
        smallerEqualityListCheckTrinaryNA :: (String,Int,Int) -> [(String,Int,Int,String)]
        smallerEqualityListCheckTrinaryNA ([],_,_)  = []
        smallerEqualityListCheckTrinaryNA xs       = if | (tripletFst xs) == "NA" ||
                                                          (tripletFst xs) == "N/A"
                                                        -> [quadrupletTransform (xs,"NA")]
                                                        | otherwise
                                                        -> []
        --smallPredicateNumericTrinaryHead
        smallPredicateNumericTrinaryHead :: String -> String -> Bool
        smallPredicateNumericTrinaryHead [] _  = False
        smallPredicateNumericTrinaryHead _  [] = False
        smallPredicateNumericTrinaryHead xs ys = if | DL.isInfixOf "<=" xs &&
                                                      ((read ys) :: Double) <= 
                                                      (read (TR.subRegex (TR.mkRegex "<=") xs "") :: Double)
                                                    -> True
                                                    | DL.isInfixOf ">=" xs &&
                                                      ((read ys) :: Double) >=
                                                      (read (TR.subRegex (TR.mkRegex ">=") xs "") :: Double)
                                                    -> True
                                                    | otherwise
                                                    -> False 
        --smallPredicateNumericTrinaryMiddle
        smallPredicateNumericTrinaryMiddle :: String -> String -> Bool
        smallPredicateNumericTrinaryMiddle [] _  = False
        smallPredicateNumericTrinaryMiddle _  [] = False
        smallPredicateNumericTrinaryMiddle xs ys = if | DL.isInfixOf "<=" xs &&
                                                        ((read ys) :: Double) >=
                                                        ((read :: String -> Double) (DL.head (DLS.splitOn "<=" xs))) &&
                                                        ((read ys) :: Double) <=
                                                        ((read :: String -> Double) (DL.last (DLS.splitOn "<=" xs)))
                                                      -> True
                                                      | DL.isInfixOf ">=" xs &&
                                                        ((read ys) :: Double) <=
                                                        ((read :: String -> Double) (DL.head (DLS.splitOn ">=" xs))) &&
                                                        ((read ys) :: Double) >=
                                                        ((read :: String -> Double) (DL.last (DLS.splitOn ">=" xs)))
                                                      -> True
                                                      | otherwise
                                                      -> False
        --smallPredicateNumericTrinaryTail
        smallPredicateNumericTrinaryTail :: String -> String -> Bool
        smallPredicateNumericTrinaryTail [] _  = False
        smallPredicateNumericTrinaryTail _  [] = False
        smallPredicateNumericTrinaryTail xs ys = if | DL.isInfixOf "<=" xs &&
                                                      ((read ys) :: Double) >= 
                                                      (read (TR.subRegex (TR.mkRegex "<=") xs "") :: Double)
                                                    -> True
                                                    | DL.isInfixOf ">=" xs &&
                                                      ((read ys) :: Double) <=
                                                      (read (TR.subRegex (TR.mkRegex ">=") xs "") :: Double)
                                                    -> True
                                                    | otherwise
                                                    -> False
        --smallPredicateCharTrinary     
        smallPredicateCharTrinary :: [String] -> [String] -> Bool
        smallPredicateCharTrinary [] _  = False
        smallPredicateCharTrinary _  [] = False
        smallPredicateCharTrinary xs ys = if | ys `isSubsetOf` xs
                                             -> True
                                             | otherwise
                                             -> False
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
specificFilters :: FATConfig -> [[(String,Int,Int)]] -> [[(String,Int,Int,String)]]
specificFilters _  [] = []
specificFilters xs ys = do
    --Extract filtering from FATConfig.
    let filters = extractFiltering xs
    --Run specificFiltersSmall.
    specificFiltersSmall filters ys
        where
            --specificFiltersSmall
            specificFiltersSmall :: [Filter] -> [[(String,Int,Int)]] -> [[(String,Int,Int,String)]]
            specificFiltersSmall [] [] = []
            specificFiltersSmall [] _  = []
            specificFiltersSmall _  [] = []
            specificFiltersSmall (ffields:xs) ys = do
                --Search the specificfilterstree decision tree on ffields.
                (DMaybe.fromJust (specificFiltersDecide ffields specificfilterstree)) ++ (specificFiltersSmall xs ys)
                    where 
                        --Grab the sublist of ys that matches x (on head).
                        matchedys = customListFilter (extractFilteringColumn ffields) ys
                        --Grab the entire portion of matchedys that isn't the column header.
                        onlydata = customOnlyDataFilter (extractFilteringColumn ffields) matchedys
                        --Grab the entire portion of matchedys that is the column header.
                        notdata = customNotDataFilter (extractFilteringColumn ffields) matchedys
                        --Define specificfilterstree data (decision) tree.
                        specificfilterstree =
                            iffsf (const True) [
                                iffsf (binaryFilteringType) [
                                    iffsf (isNotAlphaListFilteringColumnType) [
                                        iffsf (elemPlusFilteringOperator) [
                                            iffsf (isInfixOfLessThanFilteringString) [
                                                iffsf (checkFilteringColumnTypeCommaFstX) [
                                                    addilist ([((DL.map (\x -> quadrupletTransform (x,"HEADER")) notdata) ++
                                                               (DL.map (\allys@(y,_,_) -> 
                                                                   if | ',' `DL.elem` y
                                                                      -> if | ((DTuple.uncurry (+) (mapTuple (read :: String -> Double) 
                                                                              (tuplifyTwo (DLS.splitOneOf ";:," y)))) >=
                                                                              ((read :: String -> Double) (extractBFSNumericNumber (extractFilteringString ffields))))
                                                                            -> quadrupletTransform (allys,"BINARYYES")
                                                                            | otherwise
                                                                            -> quadrupletTransform (allys,"BINARYNO")
                                                                      | otherwise
                                                                      -> quadrupletTransform (allys,"NA")) onlydata))])
                                                ],
                                                iffsf (checkFilteringColumnTypeCommaFstY) [
                                                    addilist ([((DL.map (\x -> quadrupletTransform (x,"HEADER")) notdata) ++
                                                               (DL.map (\allys@(y,_,_) -> 
                                                                   if | ',' `DL.elem` y
                                                                      -> if | ((DTuple.uncurry (+) 
                                                                              (DTuple.swap (mapTuple (read :: String -> Double) 
                                                                              (tuplifyTwo (DLS.splitOneOf ";:," y))))) >=
                                                                              ((read :: String -> Double) (extractBFSNumericNumber (extractFilteringString ffields))))
                                                                            -> quadrupletTransform (allys,"BINARYYES")
                                                                            | otherwise
                                                                            -> quadrupletTransform (allys,"BINARYNO")
                                                                      | otherwise
                                                                      -> quadrupletTransform (allys,"NA")) onlydata))])
                                                ]
                                            ], 
                                            iffsf (isInfixOfGreaterThanFilteringString) [
                                                iffsf (checkFilteringColumnTypeCommaFstX) [
                                                    addilist ([((DL.map (\x -> quadrupletTransform (x,"HEADER")) notdata) ++
                                                               (DL.map (\allys@(y,_,_) -> 
                                                                   if | ',' `DL.elem` y
                                                                      -> if | ((DTuple.uncurry (+) (mapTuple (read :: String -> Double) 
                                                                              (tuplifyTwo (DLS.splitOneOf ";:," y)))) <=
                                                                              ((read :: String -> Double) (extractBFSNumericNumber (extractFilteringString ffields))))
                                                                            -> quadrupletTransform (allys,"BINARYYES")
                                                                            | otherwise
                                                                            -> quadrupletTransform (allys,"BINARYNO")
                                                                      | otherwise
                                                                      -> quadrupletTransform (allys,"NA")) onlydata))])
                                                ],
                                                iffsf (checkFilteringColumnTypeCommaFstY) [
                                                    addilist ([((DL.map (\x -> quadrupletTransform (x,"HEADER")) notdata) ++
                                                               (DL.map (\allys@(y,_,_) -> 
                                                                   if | ',' `DL.elem` y
                                                                      -> if | ((DTuple.uncurry (+) 
                                                                              (DTuple.swap (mapTuple (read :: String -> Double) 
                                                                              (tuplifyTwo (DLS.splitOneOf ";:," y))))) <=
                                                                              ((read :: String -> Double) (extractBFSNumericNumber (extractFilteringString ffields))))
                                                                            -> quadrupletTransform (allys,"BINARYYES")
                                                                            | otherwise
                                                                            -> quadrupletTransform (allys,"BINARYNO")
                                                                      | otherwise
                                                                      -> quadrupletTransform (allys,"NA")) onlydata))])
                                                ]
                                            ] 
                                        ],  
                                        iffsf (elemMinusFilteringOperator) [
                                            iffsf (isInfixOfLessThanFilteringString) [
                                                iffsf (checkFilteringColumnTypeCommaFstX) [
                                                    addilist ([((DL.map (\x -> quadrupletTransform (x,"HEADER")) notdata) ++
                                                               (DL.map (\allys@(y,_,_) -> 
                                                                   if | ',' `DL.elem` y
                                                                      -> if | ((DTuple.uncurry (-) (mapTuple (read :: String -> Double) 
                                                                              (tuplifyTwo (DLS.splitOneOf ";:," y)))) >=
                                                                              ((read :: String -> Double) (extractBFSNumericNumber (extractFilteringString ffields))))
                                                                            -> quadrupletTransform (allys,"BINARYYES")
                                                                            | otherwise
                                                                            -> quadrupletTransform (allys,"BINARYNO")
                                                                      | otherwise
                                                                      -> quadrupletTransform (allys,"NA")) onlydata))])
                                                ],
                                                iffsf (checkFilteringColumnTypeCommaFstY) [
                                                    addilist ([((DL.map (\x -> quadrupletTransform (x,"HEADER")) notdata) ++
                                                               (DL.map (\allys@(y,_,_) -> 
                                                                   if | ',' `DL.elem` y
                                                                      -> if | ((DTuple.uncurry (-) 
                                                                              (DTuple.swap (mapTuple (read :: String -> Double) 
                                                                              (tuplifyTwo (DLS.splitOneOf ";:," y))))) >=
                                                                              ((read :: String -> Double) (extractBFSNumericNumber (extractFilteringString ffields))))
                                                                            -> quadrupletTransform (allys,"BINARYYES")
                                                                            | otherwise
                                                                            -> quadrupletTransform (allys,"BINARYNO")
                                                                      | otherwise
                                                                      -> quadrupletTransform (allys,"NA")) onlydata))])
                                                ]
                                            ],                              
                                            iffsf (isInfixOfGreaterThanFilteringString) [
                                                iffsf (checkFilteringColumnTypeCommaFstX) [
                                                    addilist ([((DL.map (\x -> quadrupletTransform (x,"HEADER")) notdata) ++
                                                               (DL.map (\allys@(y,_,_) -> 
                                                                   if | ',' `DL.elem` y
                                                                      -> if | ((DTuple.uncurry (-) (mapTuple (read :: String -> Double) 
                                                                              (tuplifyTwo (DLS.splitOneOf ";:," y)))) <=
                                                                              ((read :: String -> Double) (extractBFSNumericNumber (extractFilteringString ffields))))
                                                                            -> quadrupletTransform (allys,"BINARYYES")
                                                                            | otherwise
                                                                            -> quadrupletTransform (allys,"BINARYNO")
                                                                      | otherwise
                                                                      -> quadrupletTransform (allys,"NA")) onlydata))])
                                                ],
                                                iffsf (checkFilteringColumnTypeCommaFstY) [
                                                    addilist ([((DL.map (\x -> quadrupletTransform (x,"HEADER")) notdata) ++
                                                               (DL.map (\allys@(y,_,_) -> 
                                                                   if | ',' `DL.elem` y
                                                                      -> if | ((DTuple.uncurry (-) 
                                                                              (DTuple.swap (mapTuple (read :: String -> Double) 
                                                                              (tuplifyTwo (DLS.splitOneOf ";:," y))))) >=
                                                                              ((read :: String -> Double) (extractBFSNumericNumber (extractFilteringString ffields))))
                                                                            -> quadrupletTransform (allys,"BINARYYES")
                                                                            | otherwise
                                                                            -> quadrupletTransform (allys,"BINARYNO")
                                                                      | otherwise
                                                                      -> quadrupletTransform (allys,"NA")) onlydata))])
                                                ]
                                            ]
                                        ],  
                                        iffsf (elemDivisionSignFilteringOperator) [
                                            iffsf (isInfixOfLessThanFilteringString) [
                                                iffsf (checkFilteringColumnTypeCommaFstX) [
                                                    addilist ([((DL.map (\x -> quadrupletTransform (x,"HEADER")) notdata) ++
                                                               (DL.map (\allys@(y,_,_) -> 
                                                                   if | ',' `DL.elem` y
                                                                      -> if | ((DTuple.uncurry (/) (mapTuple (read :: String -> Double) 
                                                                              (tuplifyTwo (DLS.splitOneOf ";:," y)))) >=
                                                                              ((read :: String -> Double) (extractBFSNumericNumber (extractFilteringString ffields))))
                                                                            -> quadrupletTransform (allys,"BINARYYES")
                                                                            | otherwise
                                                                            -> quadrupletTransform (allys,"BINARYNO")
                                                                      | otherwise
                                                                      -> quadrupletTransform (allys,"NA")) onlydata))])
                                                ],
                                                iffsf (checkFilteringColumnTypeCommaFstY) [
                                                    addilist ([((DL.map (\x -> quadrupletTransform (x,"HEADER")) notdata) ++
                                                               (DL.map (\allys@(y,_,_) -> 
                                                                   if | ',' `DL.elem` y
                                                                      -> if | ((DTuple.uncurry (/) 
                                                                              (DTuple.swap (mapTuple (read :: String -> Double) 
                                                                              (tuplifyTwo (DLS.splitOneOf ";:," y))))) >=
                                                                              ((read :: String -> Double) (extractBFSNumericNumber (extractFilteringString ffields))))
                                                                            -> quadrupletTransform (allys,"BINARYYES")
                                                                            | otherwise
                                                                            -> quadrupletTransform (allys,"BINARYNO")
                                                                      | otherwise
                                                                      -> quadrupletTransform (allys,"NA")) onlydata))])
                                                ]
                                            ],
                                            iffsf (isInfixOfGreaterThanFilteringString) [
                                                iffsf (checkFilteringColumnTypeCommaFstX) [
                                                    addilist ([((DL.map (\x -> quadrupletTransform (x,"HEADER")) notdata) ++
                                                               (DL.map (\allys@(y,_,_) -> 
                                                                   if | ',' `DL.elem` y
                                                                      -> if | ((DTuple.uncurry (/) (mapTuple (read :: String -> Double) 
                                                                              (tuplifyTwo (DLS.splitOneOf ";:," y)))) <=
                                                                              ((read :: String -> Double) (extractBFSNumericNumber (extractFilteringString ffields))))
                                                                            -> quadrupletTransform (allys,"BINARYYES")
                                                                            | otherwise
                                                                            -> quadrupletTransform (allys,"BINARYNO")
                                                                      | otherwise
                                                                      -> quadrupletTransform (allys,"NA")) onlydata))])
                                                ],
                                                iffsf (checkFilteringColumnTypeCommaFstY) [
                                                    addilist ([((DL.map (\x -> quadrupletTransform (x,"HEADER")) notdata) ++
                                                               (DL.map (\allys@(y,_,_) -> 
                                                                   if | ',' `DL.elem` y
                                                                      -> if | ((DTuple.uncurry (/) 
                                                                              (DTuple.swap (mapTuple (read :: String -> Double) 
                                                                              (tuplifyTwo (DLS.splitOneOf ";:," y))))) <=
                                                                              ((read :: String -> Double) (extractBFSNumericNumber (extractFilteringString ffields))))
                                                                            -> quadrupletTransform (allys,"BINARYYES")
                                                                            | otherwise
                                                                            -> quadrupletTransform (allys,"BINARYNO")
                                                                      | otherwise
                                                                      -> quadrupletTransform (allys,"NA")) onlydata))])
                                                ]
                                            ]
                                        ],
                                        iffsf (elemPipeFilteringOperator) [
                                            iffsf (isInfixOfLessThanFilteringString) [
                                                iffsf (checkFilteringColumnTypeCommaFstX) [
                                                    iffsf (checkFilteringColumnTypeCommaSndUnderscore) [
                                                        addilist ([((DL.map (\x -> quadrupletTransform (x,"HEADER")) notdata) ++
                                                                   (DL.map (\allys@(y,_,_) -> 
                                                                       if | ',' `DL.elem` y
                                                                          -> if | (((read :: String -> Double) 
                                                                                  (fst (tuplifyTwo (DLS.splitOneOf ";:," y)))) >=
                                                                                  ((read :: String -> Double) (extractBFSNumericNumber (extractFilteringString ffields))))
                                                                                -> quadrupletTransform (allys,"BINARYYES")
                                                                                | otherwise
                                                                                -> quadrupletTransform (allys,"BINARYNO")
                                                                          | otherwise
                                                                          -> quadrupletTransform (allys,"NA")) onlydata))])
                                                    ]
                                                ],
                                                iffsf (checkFilteringColumnTypeCommaFstUnderscore) [
                                                    iffsf (checkFilteringColumnTypeCommaSndY) [
                                                        addilist ([((DL.map (\x -> quadrupletTransform (x,"HEADER")) notdata) ++
                                                                   (DL.map (\allys@(y,_,_) -> 
                                                                       if | ',' `DL.elem` y
                                                                          -> if | (((read :: String -> Double) 
                                                                                  (snd (tuplifyTwo (DLS.splitOneOf ";:," y)))) >=
                                                                                  ((read :: String -> Double) (extractBFSNumericNumber (extractFilteringString ffields))))
                                                                                -> quadrupletTransform (allys,"BINARYYES")
                                                                                | otherwise
                                                                                -> quadrupletTransform (allys,"BINARYNO")
                                                                          | otherwise
                                                                          -> quadrupletTransform (allys,"NA")) onlydata))])
                                                    ]
                                                ]
                                            ],
                                            iffsf (isInfixOfGreaterThanFilteringString) [
                                                iffsf (checkFilteringColumnTypeCommaFstX) [
                                                    iffsf (checkFilteringColumnTypeCommaSndUnderscore) [
                                                        addilist ([((DL.map (\x -> quadrupletTransform (x,"HEADER")) notdata) ++
                                                                   (DL.map (\allys@(y,_,_) -> 
                                                                       if | ',' `DL.elem` y
                                                                          -> if | (((read :: String -> Double) 
                                                                                  (fst (tuplifyTwo (DLS.splitOneOf ";:," y)))) <=
                                                                                  ((read :: String -> Double) (extractBFSNumericNumber (extractFilteringString ffields))))
                                                                                -> quadrupletTransform (allys,"BINARYYES")
                                                                                | otherwise
                                                                                -> quadrupletTransform (allys,"BINARYNO")
                                                                          | otherwise
                                                                          -> quadrupletTransform (allys,"NA")) onlydata))])
                                                    ]
                                                ],
                                                iffsf (checkFilteringColumnTypeCommaFstUnderscore) [
                                                    iffsf (checkFilteringColumnTypeCommaSndY) [
                                                        addilist ([((DL.map (\x -> quadrupletTransform (x,"HEADER")) notdata) ++
                                                                   (DL.map (\allys@(y,_,_) -> 
                                                                       if | ',' `DL.elem` y
                                                                          -> if | (((read :: String -> Double) 
                                                                                  (snd (tuplifyTwo (DLS.splitOneOf ";:," y)))) <=
                                                                                  ((read :: String -> Double) (extractBFSNumericNumber (extractFilteringString ffields))))
                                                                                -> quadrupletTransform (allys,"BINARYYES")
                                                                                | otherwise
                                                                                -> quadrupletTransform (allys,"BINARYNO")
                                                                          | otherwise
                                                                          -> quadrupletTransform (allys,"NA")) onlydata))])
                                                    ]
                                                ]
                                            ]
                                        ]
                                    ],
                                    iffsf (isAlphaListFilteringColumnType) [
                                        iffsf (elemPipeFilteringOperator) [
                                            iffsf (isInfixOfLessThanFilteringString) [
                                                addilist ([((DL.map (\x -> quadrupletTransform (x,"HEADER")) notdata) ++
                                                           ((DL.map (\allys@(y,_,_) -> 
                                                                if | not (y =~ "NA" :: Bool) ||
                                                                     not (y =~ "N/A" :: Bool)
                                                                   -> if | (((read :: String -> Double) y) >=
                                                                           ((read :: String -> Double) 
                                                                           (extractBFSNumericNumber (extractFilteringString ffields))))
                                                                         -> quadrupletTransform (allys,"BINARYYES")
                                                                         | otherwise
                                                                         -> quadrupletTransform (allys,"BINARYNO")
                                                                   | otherwise
                                                                   -> quadrupletTransform (allys,"NA")) onlydata)))])
                                            ],
                                            iffsf (isInfixOfGreaterThanFilteringString) [
                                                addilist ([((DL.map (\x -> quadrupletTransform (x,"HEADER")) notdata) ++
                                                            ((DL.map (\allys@(y,_,_) -> 
                                                                 if | not (y =~ "NA" :: Bool) ||
                                                                      not (y =~ "N/A" :: Bool)
                                                                    -> if | (((read :: String -> Double) y) <=
                                                                            ((read :: String -> Double) 
                                                                            (extractBFSNumericNumber (extractFilteringString ffields))))
                                                                          -> quadrupletTransform (allys,"BINARYYES")
                                                                          | otherwise
                                                                          -> quadrupletTransform (allys,"BINARYNO")
                                                                    | otherwise
                                                                    -> quadrupletTransform (allys,"NA")) onlydata)))])
                                            ],
                                            iffsf (isInfixOfEqualSignFilteringString) [
                                                addilist ([((DL.map (\x -> quadrupletTransform (x,"HEADER")) notdata) ++
                                                           (DL.concat 
                                                           (equalityListCheckBinary 
                                                           (extractBFSStringLiteral (extractFilteringString ffields)) onlydata)))])
                                            ]
                                        ]
                                    ]
                                ],
                                iffsf (trinaryFilteringType) [
                                    iffsf (isAlphaListFilteringColumnType) [
                                        iffsf (elemPipeFilteringOperator) [ 
                                            addilist ([((DL.map (\x -> quadrupletTransform (x,"HEADER")) notdata) ++
                                                       (DL.concat 
                                                       (equalityListCheckTrinary 
                                                       ([extractTFSNumericAndStringHead 
                                                       (extractFilteringString ffields)] 
                                                    ++ [extractTFSNumericAndStringMiddle
                                                       (extractFilteringString ffields)] 
                                                    ++ [extractTFSNumericAndStringTail
                                                       (extractFilteringString ffields)]) onlydata)))])
                                        ]
                                    ]
                                ]
                            ]
