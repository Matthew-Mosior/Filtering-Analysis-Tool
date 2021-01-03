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

module FilterFields where

{--------}


{-Import module.-}

import Common
import SpecificFilters
import YamlParser

{----------------}


{-Imports-}

import Data.Foldable as DF
import Data.HashMap.Strict as DHS
import Data.List as DL
import Data.List.Split as DLS
import Data.Maybe as DMaybe
import Data.Text as DText
import Data.Tree as DTree

{---------}


{-Custom filterFields Datatype and related functions.-}

--Define FFFilter data tree.
data FFFilter a = FFRequirement (FATConfig -> Bool)
                | Flist a

--Define helper data tree functions for SpecificFilters.
iffff :: (FATConfig -> Bool) -> Forest (FFFilter a) -> Tree (FFFilter a)
iffff = Node . FFRequirement

addflist :: a -> Tree (FFFilter a)
addflist = flip Node [] . Flist

--Define tree search function.
filterFieldsDecide :: FATConfig -> Tree (FFFilter a) -> Maybe a
filterFieldsDecide x (Node (Flist y) _) = Just y
filterFieldsDecide x (Node (FFRequirement p) subtree)
    | p x = asum $ DL.map (filterFieldsDecide x) subtree
    | otherwise = Nothing

--Define boolTrueAddFilteringStatus
boolTrueAddFilteringStatus :: FATConfig -> Bool
boolTrueAddFilteringStatus xs = if | extractAddFilteringStatus xs
                                   -> True
                                   | otherwise
                                   -> False

--Define boolTrueAddFilteringBinaries
boolTrueAddFilteringBinaries :: FATConfig -> Bool
boolTrueAddFilteringBinaries xs = if | extractAddFilteringBinaries xs
                                     -> True
                                     | otherwise
                                     -> False

--boolIsCopyColumnFormatting
boolIsCopyColumnFormatting :: FATConfig -> Bool
boolIsCopyColumnFormatting xs = if | isCopyColumnFormatting xs
                                   -> True
                                   | otherwise
                                   -> False

--extractAllBinaryFilteringType
extractAllBinaryFilteringType :: FATConfig -> [String]
extractAllBinaryFilteringType xs = DL.map (snd)
                                   (DL.filter (\(x,y) -> x == "BINARY")
                                   (DL.zip
                                   (DL.map (extractFilteringType) (extractFiltering xs))
                                   (DL.map (extractFilteringColumn) (extractFiltering xs))))
            
{--------------------------------------------------------}


{-filterFields functions.-}

--indexAdder -> This function will
--add indexes to the input list.
indexAdder :: [[String]] -> [[(String,Int,Int)]]
indexAdder [] = []
indexAdder xs = orderList xs (matchedReplication xs [0..(DL.length xs - 1)]) (nestedCycle xs [0..])

--addNonFilters -> This function will
--add back the non-filtered fields.
addNonFilters :: [String] -> [[(String,Int,Int)]] -> [[(String,Int,Int,String)]] -> [[(String,Int,Int,String)]]
addNonFilters [] []    (_:_) = []
addNonFilters [] (_:_) _     = []
addNonFilters [] []    []    = []
addNonFilters xs ys    zs    = (regexFilter xs ys) ++ zs
    where
        --Nested function definitions.--
        --regexFilter
        regexFilter :: [String] -> [[(String,Int,Int)]] -> [[(String,Int,Int,String)]]
        regexFilter [] []     = []
        regexFilter [] _      = []
        regexFilter _  []     = []
        regexFilter xs (y:ys) = if | regexPredicate xs y
                                   -> [[quadrupletTransform ((DL.head y),"HEADER")]
                                     ++ (DL.map (\x -> quadrupletTransform (x,"NA")) (DL.tail y))]
                                     ++ (regexFilter xs ys)
                                   | otherwise
                                   -> regexFilter xs ys
        --regexPredicate
        regexPredicate :: [String] -> [(String,Int,Int)] -> Bool
        regexPredicate []  _ = False
        regexPredicate _  [] = False
        regexPredicate xs ys = if | (tripletFst (DL.head ys)) `DL.elem` xs
                                  -> False
                                  | otherwise
                                  -> True
        --------------------------------

--reorderList -> This function will
--reorder a list based on another list.
reorderList :: [[(String,Int,Int)]] -> [[(String,Int,Int,String)]] -> [[(String,Int,Int,String)]]
reorderList []     [] = []
reorderList []     _  = []
reorderList _      [] = []
reorderList (x:xs) ys = (DL.filter (\y -> quadrupletFst (y DL.!! 0) == tripletFst (x DL.!! 0) &&
                                          quadrupletSnd (y DL.!! 0) == tripletSnd (x DL.!! 0) &&
                                          quadrupletThrd (y DL.!! 0) == tripletThrd (x DL.!! 0)) ys)
                     ++ (reorderList xs ys)

--addFilteringBinaryColumns -> This function will
--add filtering binary (0 or 1) to each row
--for each filter.
addFilteringBinaryColumns :: [String] -> [[(String,Int,Int,String)]] -> [Int] -> [[(String,Int,Int,String)]]
addFilteringBinaryColumns []     [] []        = []
addFilteringBinaryColumns []     []    (_:_)  = []
addFilteringBinaryColumns []     (_:_) _      = []
addFilteringBinaryColumns (_:_)  _     []     = []
addFilteringBinaryColumns (x:xs) ys    (z:zs) = ys ++ ([smallerAddFilteringBinaryColumns x ys z]
                                                   ++  (addFilteringBinaryColumns xs ys zs))
    where
        --Nested function definitions.--
        --smallerAddFilteringBinaryColumns
        smallerAddFilteringBinaryColumns :: String -> [[(String,Int,Int,String)]] -> Int -> [(String,Int,Int,String)]
        smallerAddFilteringBinaryColumns [] [] _  = []
        smallerAddFilteringBinaryColumns _  [] _  = []
        smallerAddFilteringBinaryColumns [] _  _  = []
        smallerAddFilteringBinaryColumns xs ys zs = smallestAddFilteringBinaryColumns (DL.concat
                                                                                      (DL.filter (\(y:_) ->
                                                                                      (xs == (quadrupletFst y)))
                                                                                      ys))
                                                                                      (zs)
        --smallestAddFilteringBinaryColumns
        smallestAddFilteringBinaryColumns :: [(String,Int,Int,String)] -> Int -> [(String,Int,Int,String)]
        smallestAddFilteringBinaryColumns []             _  = []
        smallestAddFilteringBinaryColumns ((a,b,_,d):xs) ys = if | (d == "NA") ->
                                                                   [("1",b,ys,"FILTERCOLUMN")] ++ (smallestAddFilteringBinaryColumns xs ys)
                                                                 | (d == "BINARYYES") ->
                                                                   [("1",b,ys,"FILTERCOLUMN")] ++ (smallestAddFilteringBinaryColumns xs ys)
                                                                 | (d == "BINARYNO") ->
                                                                   [("0",b,ys,"FILTERCOLUMN")] ++ (smallestAddFilteringBinaryColumns xs ys)
                                                                 | (d == "HEADER") ->
                                                                   [(a ++ "_BINARY",b,ys,"HEADER")]
                                                                 ++ (smallestAddFilteringBinaryColumns xs ys)
                                                                 | otherwise -> smallestAddFilteringBinaryColumns xs ys
        --------------------------------

--copyColumnFormatting -> This function will
--copy the formatting of one column and apply
--it to another non-filtered column.
copyColumnFormatting :: [[(String,Int,Int,String)]] -> [(String,String)] -> [String] -> [[(String,Int,Int,String)]]
copyColumnFormatting []    [] []    = []
copyColumnFormatting []    [] (_:_) = []
copyColumnFormatting (_:_) [] _     = []
copyColumnFormatting xs    ys zs    = reorderFormatting
                                      (notdestinationcolumns ++ (applyFormatting xs ys))
                                      xs
    where
        --Local functions.--
        --applyFormatting
        applyFormatting :: [[(String,Int,Int,String)]] -> [(String,String)] -> [[(String,Int,Int,String)]]
        applyFormatting []    []     = []
        applyFormatting []    _      = []
        applyFormatting _     []     = []
        applyFormatting xs    (y:ys) = [smallApplyFormatting sourcecolumn destinationcolumn]
                                    ++ (applyFormatting xs ys)
            where
                --Local definitions.--
                sourcecolumn = DL.concat (DL.filter (\x -> case x of
                                                           [] -> False
                                                           x  -> (fst y) == quadrupletFst (x DL.!! 0)) xs)
                destinationcolumn = DL.concat (DL.filter (\x -> case x of
                                                                [] -> False
                                                                x  -> (snd y) == quadrupletFst (x DL.!! 0)) xs)
                ----------------------
        --smallApplyFormatting
        smallApplyFormatting :: [(String,Int,Int,String)] -> [(String,Int,Int,String)] -> [(String,Int,Int,String)]
        smallApplyFormatting []             []             = []
        smallApplyFormatting _              []             = []
        smallApplyFormatting []             _              = []
        smallApplyFormatting ((_,_,_,d):xs) ((e,f,g,_):ys) = [(e,f,g,d)] ++ (smallApplyFormatting xs ys)
        --reorderFormatting
        reorderFormatting :: [[(String,Int,Int,String)]] -> [[(String,Int,Int,String)]] -> [[(String,Int,Int,String)]]
        reorderFormatting [] []     = []
        reorderFormatting _  []     = []
        reorderFormatting [] _      = []
        reorderFormatting xs (y:ys) = (DL.filter (\a -> case a of
                                                        [] -> False
                                                        a  -> quadrupletFst (a DL.!! 0) == quadrupletFst (y DL.!! 0) &&
                                                              quadrupletSnd (a DL.!! 0) == quadrupletSnd (y DL.!! 0) &&
                                                              quadrupletThrd (a DL.!! 0) == quadrupletThrd (y DL.!! 0)) xs)
                                   ++ (reorderFormatting xs ys)
        --Local definitions.--
        notdestinationcolumns = DL.filter (\x-> (quadrupletFst (x DL.!! 0)) `DL.notElem` zs) xs
        ----------------------

--addPassOrFail -> This function will
--add pass or fail remarks to each line.
addPassOrFail :: [[(String,Int,Int,String)]] -> [[(String,Int,Int,String)]]
addPassOrFail []     = []
addPassOrFail (x:xs) = if | (DL.any (\(_,_,_,d) -> d == "BINARYNO") x)
                          -> [x ++ [("Fail"
                                    ,quadrupletSnd (DL.last x)
                                    ,(quadrupletThrd (DL.last x)) + 1
                                    ,"BINARYNO")]] ++ (addPassOrFail xs)
                          | (DL.all (\(_,_,_,d) -> (d == "NA")            ||
                                                   (d == "TRINARYHEAD")   ||
                                                   (d == "TRINARYMIDDLE") ||
                                                   (d == "TRINARYTAIL")) x)
                          -> [x ++ [("Not_filtered"
                                    ,quadrupletSnd (DL.last x)
                                    ,(quadrupletThrd (DL.last x)) + 1
                                    ,"NA")]] ++ (addPassOrFail xs)
                          | otherwise 
                          -> [x ++ [("Pass"
                                    ,quadrupletSnd (DL.last x)
                                    ,(quadrupletThrd (DL.last x)) + 1
                                    ,"BINARYYES")]] ++ (addPassOrFail xs)  

--addFilteringStatusColumnHeader -> This function will
--add the Filtering_Status to the first sublist.
addFilteringStatusColumnHeader :: [[(String,Int,Int,String)]] -> [[(String,Int,Int,String)]]
addFilteringStatusColumnHeader [] = []
addFilteringStatusColumnHeader xs = [DL.head xs ++ [("Filtering_Status"
                                                    ,0
                                                    ,DL.length xs + 1
                                                    ,"HEADER")]]
                                                ++ (DL.tail xs) 

--filterFields -> This function will
--filter a field by the corresponding
--field.
filterFields :: FATConfig -> [[String]] -> [[(String,Int,Int,String)]]
filterFields _    [] = []
filterFields opts xs = do 
    --Search the specificfilterstree decision tree on user-defined flags.
    (DMaybe.fromJust (filterFieldsDecide opts filterfieldstree))
        where
            --Add indexes to xs.
            indexedxs = indexAdder xs
            --Call specificFilters on fieldandcondition.
            specificfiltered = specificFilters opts (DL.transpose indexedxs)
            --Add back the nonfilteredlists.
            nonfiltersadded = addNonFilters (DL.map (extractFilteringColumn) (extractFiltering opts)) (DL.transpose indexedxs) specificfiltered
            --Reorder nonfiltersadded.
            reorderedlist = reorderList (DL.transpose indexedxs) nonfiltersadded
            --Tranpose reorderedlist.
            transposedreorderedlist = DL.transpose reorderedlist
            --Define filterfieldstree data (decision) tree.
            filterfieldstree =
                iffff (const True) [
                    iffff (boolTrueAddFilteringStatus) [
                        iffff (boolTrueAddFilteringBinaries) [
                            iffff (boolIsCopyColumnFormatting) [
                                addflist ( DL.transpose 
                                           (copyColumnFormatting
                                           (addFilteringBinaryColumns
                                           (extractAllBinaryFilteringType opts)
                                           (DL.transpose (addFilteringStatusColumnHeader
                                                         ([DL.head transposedreorderedlist]
                                                      ++ (addPassOrFail (DL.tail transposedreorderedlist)))))
                                           ([((DL.length (addFilteringStatusColumnHeader
                                                         ([DL.head transposedreorderedlist]
                                                      ++ (addPassOrFail (DL.tail transposedreorderedlist))))) + 1)
                                           ..((DL.length (addFilteringStatusColumnHeader
                                                         ([DL.head transposedreorderedlist]
                                                      ++ (addPassOrFail (DL.tail transposedreorderedlist)))))
                                                       + (DL.length (extractAllBinaryFilteringType opts)))]))
                                           (DL.map (\(x,y) -> (DText.unpack x,DText.unpack x)) 
                                                   (DHS.toList (DMaybe.fromJust (extractCopyColumnFormatting opts))))
                                           (DL.map (snd) (DL.map (\(x,y) -> (DText.unpack x,DText.unpack x)) 
                                                         (DHS.toList (DMaybe.fromJust (extractCopyColumnFormatting opts)))))))  
                                          
                            ],
                            addflist ((addFilteringBinaryColumns
                                      (extractAllBinaryFilteringType opts)
                                      (DL.transpose (addFilteringStatusColumnHeader
                                                    ([DL.head transposedreorderedlist]
                                                 ++ (addPassOrFail (DL.tail transposedreorderedlist)))))
                                      ([((DL.length (addFilteringStatusColumnHeader
                                                    ([DL.head transposedreorderedlist]
                                                 ++ (addPassOrFail (DL.tail transposedreorderedlist))))) + 1)
                                      ..((DL.length (addFilteringStatusColumnHeader
                                                    ([DL.head transposedreorderedlist]
                                                 ++ (addPassOrFail (DL.tail transposedreorderedlist)))))
                                                  + (DL.length (extractAllBinaryFilteringType opts)))])))
                        ],
                        addflist ((addFilteringStatusColumnHeader
                                  ([DL.head transposedreorderedlist]
                               ++ (addPassOrFail (DL.tail transposedreorderedlist)))))
                    ],
                    iffff (boolTrueAddFilteringBinaries) [
                        iffff (boolIsCopyColumnFormatting) [
                            addflist ( DL.transpose
                                       (copyColumnFormatting
                                       (addFilteringBinaryColumns
                                       (extractAllBinaryFilteringType opts)
                                       (DL.transpose transposedreorderedlist)
                                       ([((DL.length transposedreorderedlist) + 1)..((DL.length transposedreorderedlist)
                                     + (DL.length (extractAllBinaryFilteringType opts)))]))
                                       (DL.map (\(x,y) -> (DText.unpack x,DText.unpack x)) 
                                               (DHS.toList (DMaybe.fromJust (extractCopyColumnFormatting opts))))
                                       (DL.map (snd) (DL.map (\(x,y) -> (DText.unpack x,DText.unpack x)) 
                                                     (DHS.toList (DMaybe.fromJust (extractCopyColumnFormatting opts))))))) 
                                   
                        ],
                        addflist ((DL.transpose (addFilteringBinaryColumns
                                   (extractAllBinaryFilteringType opts)
                                   (DL.transpose transposedreorderedlist)
                                   ([((DL.length transposedreorderedlist) + 1)..((DL.length transposedreorderedlist)
                                 + (DL.length (extractAllBinaryFilteringType opts)))]))))  
                    ],
                    iffff (boolTrueAddFilteringStatus) [
                        iffff (boolIsCopyColumnFormatting) [
                            addflist ( DL.transpose
                                       (copyColumnFormatting
                                       (DL.transpose
                                       (addFilteringStatusColumnHeader
                                       ([DL.head transposedreorderedlist]
                                    ++ (addPassOrFail (DL.tail transposedreorderedlist)))))
                                       (DL.map (\(x,y) -> (DText.unpack x,DText.unpack x)) 
                                               (DHS.toList (DMaybe.fromJust (extractCopyColumnFormatting opts))))
                                       (DL.map (snd) (DL.map (\(x,y) -> (DText.unpack x,DText.unpack x)) 
                                                     (DHS.toList (DMaybe.fromJust (extractCopyColumnFormatting opts))))))) 
                                       
                        ]
                    ],
                    iffff (boolIsCopyColumnFormatting) [
                        addflist ( DL.transpose
                                   (copyColumnFormatting
                                   (DL.transpose transposedreorderedlist)
                                   (DL.map (\(x,y) -> (DText.unpack x,DText.unpack x)) 
                                                      (DHS.toList (DMaybe.fromJust (extractCopyColumnFormatting opts))))
                                   (DL.map (snd) (DL.map (\(x,y) -> (DText.unpack x,DText.unpack x)) 
                                                 (DHS.toList (DMaybe.fromJust (extractCopyColumnFormatting opts)))))))
                    ],
                    addflist (transposedreorderedlist) 
                ]

{-------------------------}
