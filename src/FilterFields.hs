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

{----------------}


{-Imports-}

import Data.Foldable as DF
import Data.List as DL
import Data.List.Split as DLS
import Data.Maybe as DMaybe
import Data.Tree as DTree

{---------}


{-Custom filterFields Datatype and related functions.-}

--Define FFFilter data tree.
data FFFilter a = FFRequirement ([Flag] -> Bool)
                | Flist a

--Define helper data tree functions for SpecificFilters.
iffff :: ([Flag] -> Bool) -> Forest (FFFilter a) -> Tree (FFFilter a)
iffff = Node . FFRequirement

addflist :: a -> Tree (FFFilter a)
addflist = flip Node [] . Flist

--Define tree search function.
filterFieldsDecide :: [Flag] -> Tree (FFFilter a) -> Maybe a
filterFieldsDecide x (Node (Flist y) _) = Just y
filterFieldsDecide x (Node (FFRequirement p) subtree)
    | p x = asum $ map (filterFieldsDecide x) subtree
    | otherwise = Nothing

--Define boolAddFilteringStatus
boolAddFilteringStatus :: [Flag] -> Bool
boolAddFilteringStatus xs = if | DL.elem AddFilteringStatus xs
                               -> True
                               | otherwise
                               -> False

--Define boolAddFilteringBinaries
boolAddFilteringBinaries :: [Flag] -> Bool
boolAddFilteringBinaries xs = if | DL.elem AddFilteringBinaries xs
                                 -> True
                                 | otherwise
                                 -> False

--Define boolIsCopyColumnFormatting
boolIsCopyColumnFormatting :: [Flag] -> Bool
boolIsCopyColumnFormatting xs = if | DL.length (DL.filter (isCopyColumnFormatting) xs) > 0 
                                   -> True
                                   | otherwise
                                   -> False

{--------------------------------------------------------}

{-filterFields functions.-}

--isFilterFields -> This function will
--test for FilterFields flag.
isFilterFields :: Flag -> Bool
isFilterFields (FilterFields _) = True
isFilterFields _                = False

--extractFilterFields -> This function will
--extract the string associated with
--FilterFields.
extractFilterFields :: Flag -> String
extractFilterFields (FilterFields x) = x

--isCopyColumnFormatting -> This function will
--test for CopyColumnFormatting flag.
isCopyColumnFormatting :: Flag -> Bool
isCopyColumnFormatting (CopyColumnFormatting _) = True
isCopyColumnFormatting _                        = False

--extractCopyColumnFormatting -> This function will
--extract the string associated with
--CopyColumnFormatting.
extractCopyColumnFormatting :: Flag -> String
extractCopyColumnFormatting (CopyColumnFormatting x) = x

--indexAdder -> This function will
--add indexes to the input list.
indexAdder :: [[String]] -> [[(String,Int,Int)]]
indexAdder [] = []
indexAdder xs = orderList xs (matchedReplication xs [0..(DL.length xs - 1)]) (nestedCycle xs [0..])

--addNonFilters -> This function will
--add back the non-filtered fields.
addNonFilters :: [[String]] -> [[(String,Int,Int)]] -> [[(String,Int,Int,String)]] -> [[(String,Int,Int,String)]]
addNonFilters [] []    (_:_) = []
addNonFilters [] (_:_) _     = []
addNonFilters [] []    []    = []
addNonFilters xs ys    zs    = (regexFilter (DL.map (DL.!! 1) xs) ys) ++ zs
    where
        --Nested function definitions.--
        --regexFilter
        regexFilter :: [String] -> [[(String,Int,Int)]] -> [[(String,Int,Int,String)]]
        regexFilter [] []     = []
        regexFilter [] _      = []
        regexFilter _  []     = []
        regexFilter xs (y:ys) = if regexPredicate xs y
                                    then [[quadrupletTransform ((DL.head y),"HEADER")]
                                      ++ (DL.map (\x -> quadrupletTransform (x,"NA")) (DL.tail y))]
                                      ++ (regexFilter xs ys)
                                    else regexFilter xs ys
        --regexPredicate
        regexPredicate :: [String] -> [(String,Int,Int)] -> Bool
        regexPredicate []  _ = False
        regexPredicate _  [] = False
        regexPredicate xs ys = if (tripletFst (DL.head ys)) `DL.elem` xs
                                   then False
                                   else True
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
addFilteringBinaryColumns :: [[String]] -> [[(String,Int,Int,String)]] -> [Int] -> [[(String,Int,Int,String)]]
addFilteringBinaryColumns []     [] []        = []
addFilteringBinaryColumns []     []    (_:_)  = []
addFilteringBinaryColumns []     (_:_) _      = []
addFilteringBinaryColumns (_:_)  _     []     = []
addFilteringBinaryColumns (x:xs) ys    (z:zs) = ys ++ ([smallerAddFilteringBinaryColumns x ys z]
                                                   ++  (addFilteringBinaryColumns xs ys zs))
    where
        --Nested function definitions.--
        --smallerAddFilteringBinaryColumns
        smallerAddFilteringBinaryColumns :: [String] -> [[(String,Int,Int,String)]] -> Int -> [(String,Int,Int,String)]
        smallerAddFilteringBinaryColumns [] [] _  = []
        smallerAddFilteringBinaryColumns _  [] _  = []
        smallerAddFilteringBinaryColumns [] _  _  = []
        smallerAddFilteringBinaryColumns xs ys zs = smallestAddFilteringBinaryColumns (DL.concat
                                                                                      (DL.filter (\(y:_) ->
                                                                                      ((xs DL.!! 1) == (quadrupletFst y)))
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
filterFields :: [Flag] -> [[String]] -> [[(String,Int,Int,String)]]
filterFields []   [] = []
filterFields opts xs = do 
    --Search the specificfilterstree decision tree on user-defined flags.
    (DMaybe.fromJust (filterFieldsDecide opts filterfieldstree))
        where
            --Grab just "FIELDS".
            ffields = singleunnest (DL.filter (isFilterFields) opts)
            --Extract the string from FilterFields.
            ffstring = extractFilterFields ffields
            --Remove beginning and ending delimiters.
            begendremoved = DL.init (DL.tail ffstring)
            --Push the separate filtrations into a list.
            filteringlist = DLS.splitOn ";" begendremoved
            --Get the field separated from the filtration condition.
            fieldandcondition = DL.map (DLS.splitOneOf "?:~") filteringlist
            --Add indexes to xs.
            indexedxs = indexAdder xs
            --Call specificFilters on fieldandcondition.
            specificfiltered = specificFilters filteringlist (DL.transpose indexedxs)
            --Add back the nonfilteredlists.
            nonfiltersadded = addNonFilters fieldandcondition (DL.transpose indexedxs) specificfiltered
            --Reorder nonfiltersadded.
            reorderedlist = reorderList (DL.transpose indexedxs) nonfiltersadded
            --Tranpose reorderedlist.
            transposedreorderedlist = DL.transpose reorderedlist
            --Define filterfieldstree data (decision) tree.
            filterfieldstree =
                iffff (const True) [
                    iffff (boolAddFilteringStatus) [
                        iffff (boolAddFilteringBinaries) [
                            iffff (boolIsCopyColumnFormatting) [
                                addflist ( DL.transpose 
                                           (copyColumnFormatting
                                           (addFilteringBinaryColumns
                                           (DL.filter (\x -> (x DL.!! 0) == "BINARY") fieldandcondition)
                                           (DL.transpose (addFilteringStatusColumnHeader
                                                         ([DL.head transposedreorderedlist]
                                                      ++ (addPassOrFail (DL.tail transposedreorderedlist)))))
                                           ([((DL.length (addFilteringStatusColumnHeader
                                                         ([DL.head transposedreorderedlist]
                                                      ++ (addPassOrFail (DL.tail transposedreorderedlist))))) + 1)
                                           ..((DL.length (addFilteringStatusColumnHeader
                                                         ([DL.head transposedreorderedlist]
                                                      ++ (addPassOrFail (DL.tail transposedreorderedlist)))))
                                                       + (DL.length (DL.filter (\x -> (x DL.!! 0) == "BINARY")
                                                         fieldandcondition)))]))
                                           (DL.map (\[x,y] -> (x,y))
                                           (DL.map
                                           (DLS.splitOn ":")
                                           (DLS.splitOn ";"
                                           (DL.init
                                           (DL.tail 
                                           (extractCopyColumnFormatting 
                                           (DL.head
                                           (DL.filter
                                           (isCopyColumnFormatting) opts))))))))
                                           (DL.map (snd) 
                                           (DL.map (\[x,y] -> (x,y))
                                           (DL.map
                                           (DLS.splitOn ":")
                                           (DLS.splitOn ";"
                                           (DL.init
                                           (DL.tail
                                           (extractCopyColumnFormatting
                                           (DL.head
                                           (DL.filter
                                           (isCopyColumnFormatting) opts)))))))))))
                            ],
                            addflist ((addFilteringBinaryColumns
                                      (DL.filter (\x -> (x DL.!! 0) == "BINARY") fieldandcondition)
                                      (DL.transpose (addFilteringStatusColumnHeader
                                                    ([DL.head transposedreorderedlist]
                                                 ++ (addPassOrFail (DL.tail transposedreorderedlist)))))
                                      ([((DL.length (addFilteringStatusColumnHeader
                                                    ([DL.head transposedreorderedlist]
                                                 ++ (addPassOrFail (DL.tail transposedreorderedlist))))) + 1)
                                      ..((DL.length (addFilteringStatusColumnHeader
                                                    ([DL.head transposedreorderedlist]
                                                 ++ (addPassOrFail (DL.tail transposedreorderedlist)))))
                                                  + (DL.length (DL.filter (\x -> (x DL.!! 0) == "BINARY")
                                                    fieldandcondition)))])))
                        ],
                        addflist ((addFilteringStatusColumnHeader
                                  ([DL.head transposedreorderedlist]
                               ++ (addPassOrFail (DL.tail transposedreorderedlist)))))
                    ],
                    iffff (boolAddFilteringBinaries) [
                        iffff (boolIsCopyColumnFormatting) [
                            addflist ( DL.transpose
                                       (copyColumnFormatting
                                       (addFilteringBinaryColumns
                                       (DL.filter (\x -> (x DL.!! 0) == "BINARY") fieldandcondition)
                                       (DL.transpose transposedreorderedlist)
                                       ([((DL.length transposedreorderedlist) + 1)..((DL.length transposedreorderedlist)
                                     + (DL.length (DL.filter (\x -> (x DL.!! 0) == "BINARY")
                                       fieldandcondition)))]))
                                       (DL.map (\[x,y] -> (x,y))
                                       (DL.map
                                       (DLS.splitOn ":")
                                       (DLS.splitOn ";"
                                       (DL.init
                                       (DL.tail
                                       (extractCopyColumnFormatting
                                       (DL.head
                                       (DL.filter
                                       (isCopyColumnFormatting) opts))))))))
                                       (DL.map (snd)
                                       (DL.map (\[x,y] -> (x,y))
                                       (DL.map
                                       (DLS.splitOn ":")
                                       (DLS.splitOn ";"
                                       (DL.init
                                       (DL.tail
                                       (extractCopyColumnFormatting
                                       (DL.head
                                       (DL.filter
                                       (isCopyColumnFormatting) opts)))))))))))              
                        ],
                        addflist ((DL.transpose (addFilteringBinaryColumns
                                   (DL.filter (\x -> (x DL.!! 0) == "BINARY") fieldandcondition)
                                   (DL.transpose transposedreorderedlist)
                                   ([((DL.length transposedreorderedlist) + 1)..((DL.length transposedreorderedlist)
                                 + (DL.length (DL.filter (\x -> (x DL.!! 0) == "BINARY")
                                   fieldandcondition)))]))))  
                    ],
                    iffff (boolAddFilteringStatus) [
                        iffff (boolIsCopyColumnFormatting) [
                            addflist ( DL.transpose
                                       (copyColumnFormatting
                                       (DL.transpose
                                       (addFilteringStatusColumnHeader
                                       ([DL.head transposedreorderedlist]
                                    ++ (addPassOrFail (DL.tail transposedreorderedlist)))))
                                       (DL.map (\[x,y] -> (x,y))
                                       (DL.map
                                       (DLS.splitOn ":")
                                       (DLS.splitOn ";"
                                       (DL.init
                                       (DL.tail
                                       (extractCopyColumnFormatting
                                       (DL.head
                                       (DL.filter
                                       (isCopyColumnFormatting) opts))))))))
                                       (DL.map (snd)
                                       (DL.map (\[x,y] -> (x,y))
                                       (DL.map
                                       (DLS.splitOn ":")
                                       (DLS.splitOn ";"
                                       (DL.init
                                       (DL.tail
                                       (extractCopyColumnFormatting
                                       (DL.head
                                       (DL.filter
                                       (isCopyColumnFormatting) opts)))))))))))
                        ]
                    ],
                    iffff (boolIsCopyColumnFormatting) [
                        addflist ( DL.transpose
                                   (copyColumnFormatting
                                   (DL.transpose transposedreorderedlist)
                                   (DL.map (\[x,y] -> (x,y))
                                   (DL.map
                                   (DLS.splitOn ":")
                                   (DLS.splitOn ";"
                                   (DL.init
                                   (DL.tail
                                   (extractCopyColumnFormatting
                                   (DL.head
                                   (DL.filter
                                   (isCopyColumnFormatting) opts))))))))
                                   (DL.map (snd)
                                   (DL.map (\[x,y] -> (x,y))
                                   (DL.map
                                   (DLS.splitOn ":")
                                   (DLS.splitOn ";"
                                   (DL.init
                                   (DL.tail
                                   (extractCopyColumnFormatting
                                   (DL.head
                                   (DL.filter
                                   (isCopyColumnFormatting) opts)))))))))))
                    ],
                    addflist (transposedreorderedlist) 
                ]

{-------------------------}
