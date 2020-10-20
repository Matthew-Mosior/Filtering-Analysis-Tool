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

module Common where

{--------}


{-Imports-}

import Control.Arrow as CA
import Control.Monad as CM
import Data.Char as DC
import Data.List as DL
import Data.List.Split as DLS

{---------}


{-Custom CML Option Datatype.-}

data Flag
    = Verbose                     -- -v
    | Version                     -- -V -?
    | OutputFileName       String -- -o
    | OutputSheetName      String -- -s
    | StyleSheetChoice     String --
    | FullProtection              --
    | FilterFields         String -- -F
    | AddFilteringStatus          -- -S
    | AddFilteringBinaries        -- -B
    | CopyColumnFormatting String -- -c
    | HideColumns          String -- -H
    | BinaryPassingColor   String -- -p (Default: #FFFF0000)
    | BinaryFailingColor   String -- -f (Default: #FF00FF00)
    | TrinaryHeadColor     String -- -h (Default: #FFFF0000)
    | TrinaryMiddleColor   String -- -m (Default: #FFFFFF33)
    | TrinaryTailColor     String -- -l (Default: #FF00FF00)
    | NAColor              String -- -n (Default: #FFC0C0C0)
    | Help                        -- --help
    deriving (Eq,Ord,Show)

{-----------------------------}


{-Common functions.-}

--isSubsetOf -> This function will
--be used in the stripHeader function.
xs `isSubsetOf` ys = DL.any (`DL.elem` ys) xs

--lineFeed -> This function will
--read the file in and split on
--whitespace, returning a list
--of lists.
lineFeed :: String -> [[String]]
lineFeed [] = []
lineFeed xs = DL.map DL.words (DL.lines xs)

--isHexList -> This function will
--test a string for only hex
--characters.
isHexList :: String -> Bool
isHexList xs = DL.all DC.isHexDigit xs &&
               (DL.head xs) == '#'

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

--mapNotLast -> This function will
--work like the traditional map
--function in Data.List, but not
--map to the last element of a list.
mapNotLast :: (a -> a) -> [a] -> [a]
mapNotLast fn []     = []
mapNotLast fn [x]    = [x]
mapNotLast fn (x:xs) = fn x : mapNotLast fn xs

--mapTuple -> This function will
--map a function across all elements
--of a two-tuple.
mapTuple :: (b'->c') -> (b',b') -> (c',c')
mapTuple = CM.join (***)

--matchedReplication -> This function will
--take in two lists, and replicate items in
--one list as long as the other list.
matchedReplication :: [[String]] -> [Int] -> [[Int]]
matchedReplication [] []         = []
matchedReplication _  []         = []
matchedReplication [] _          = []
matchedReplication (x:xs) (y:ys) = [DL.replicate (DL.length x) y] ++ (matchedReplication xs ys)

--nestedCycle -> This function will
--repeat a list of numbers the length
--of another list.
nestedCycle :: [[String]] -> [Int] -> [[Int]]
nestedCycle [] []     = []
nestedCycle _ []      = []
nestedCycle [] _      = []
nestedCycle (x:xs) ys = [DL.take (DL.length x) ys] ++ (nestedCycle xs ys)

--orderList -> This function will
--order a nested list.
orderList :: [[String]] -> [[Int]] -> [[Int]] -> [[(String,Int,Int)]]
orderList [] [] []             = []
orderList [] [] _              = []
orderList [] _  []             = []
orderList _  [] []             = []
orderList _  _  []             = []
orderList [] _  _              = []
orderList _  [] _              = []
orderList (x:xs) (y:ys) (z:zs) = [DL.zip3 x y z] ++ (orderList xs ys zs)

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

--tuplifyTwo -> This function will
--turn a list of two elements into
--a two-tuple.
tuplifyTwo :: [a] -> (a,a)
tuplifyTwo [x,y]     = (x,y)

--listifyTwo -> This function will
--turn a tuple into a list of
--two elements.
listifyTwo :: (a,a) -> [a]
listifyTwo (x,y) = [x,y]

--singleunnest -> This function will
--unnest a list.
singleunnest :: [a] -> a
singleunnest [a] = a

--smallSort -> This function will
--perform sorting on lists of triplets.
smallSort :: [[(String,Int,Int)]] -> [[(String,Int,Int)]]
smallSort [] = []
smallSort (x:xs) = [DL.sortBy (\(_,_,a) (_,_,b) -> compare a b) x] ++ (smallSort xs)

--strongEq -> This function will
--serve as a stronger version of ==.
strongEq :: (Eq a) => [a] -> [a] -> Bool
strongEq x y = DL.null (x DL.\\ y) && DL.null (y DL.\\ x)

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

{-------------------}
