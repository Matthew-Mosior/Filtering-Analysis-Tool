{-=Filtering-Analysis-Tool (FAT): A Haskell-based solution to=-}
{-=analyze filtering schemes applied to tab delimited data.=-}
{-=Author: Matthew Mosior=-}
{-=Version: 3.0=-}
{-=Synopsis:  This Haskell Script will take in=-} 
{-=a tab-delimited file and provide a in-depth view=-} 
{-=of the user-defined filtering schema provided.=-}


{-Lanuguage Extension.-}

{-# LANGUAGE MultiWayIf #-}

{----------------------}


{-Import module.-}

import SpecificFilters
import FatDefinitions

{----------------}


{-Imports-}

import Codec.Xlsx as CX
import Control.Arrow as CA
import Control.Monad as CM
import Data.ByteString.Char8 as DBC
import Data.ByteString.Lazy as DBL
import Data.Char as DC
import Data.List as DL
import Data.List.Split as DLS
import Data.Ix as DI
import Data.Map as DMap
import Data.Maybe as DMaybe
import Data.Text as DText
import Data.Time.Clock.POSIX as DTCP
import Data.Tuple as DTuple
import System.Console.GetOpt as SCG
import System.Environment as SE
import System.Exit as SX
import System.IO as SIO
import System.IO.Temp as SIOT
import System.Process as SP
import Text.Read as TRead
import Text.Regex as TR
import Text.Regex.TDFA as TRP

{---------} 


{-Custom CML Option Datatype.-}

data Flag 
    = Verbose                     -- -v
    | Version                     -- -V -?
    | OutputFileType       String -- -t
    | OutputFileName       String -- -o 
    | OutputSheetName      String -- -s
    | FilterFields         String -- -F
    | AddFilteringStatus          -- -S
    | AddFilteringBinaries        -- -B
    | CopyColumnFormatting String -- -c
    | BinaryPassingColor   String -- -p (Default: #FFFF0000)
    | BinaryFailingColor   String -- -f (Default: #FF00FF00)
    | TrinaryHeadColor     String -- -h (Default: #FFFF0000)
    | TrinaryMiddleColor   String -- -m (Default: #FFFFFF33)
    | TrinaryTailColor     String -- -l (Default: #FF00FF00)
    | NAColor              String -- -n (Default: #FFC0C0C0)
    | Help                        -- --help
    deriving (Eq,Ord,Show) 

{-----------------------------}


{-Custom bool functions for Flag Datatype.-}

--isOutputFileType -> This function will
--test for the OutputFileType flag.
isOutputFileType :: Flag -> Bool
isOutputFileType (OutputFileType _) = True
isOutputFileType _                  = False  

--isOutputFileName -> This function will
--test for OutputFileName flag.
isOutputFileName :: Flag -> Bool
isOutputFileName (OutputFileName _) = True
isOutputFileName _                  = False    

--isOutputSheetName -> This function will
--test for OutputSheetName flag.
isOutputSheetName :: Flag -> Bool
isOutputSheetName (OutputSheetName _) = True
isOutputSheetName _                   = False

--isFilterFields -> This function will
--test for FilterFields flag.
isFilterFields :: Flag -> Bool
isFilterFields (FilterFields _) = True
isFilterFields _                = False

--isCopyColumnFormatting -> This function will
--test for CopyColumnFormatting flag.
isCopyColumnFormatting :: Flag -> Bool
isCopyColumnFormatting (CopyColumnFormatting _) = True
isCopyColumnFormatting _                        = False

--isBinaryPassingColor -> This function will
--test for the BinaryPassingColor Flag.
isBinaryPassingColor :: Flag -> Bool
isBinaryPassingColor (BinaryPassingColor _) = True
isBinaryPassingColor _                      = False

--isBinaryFailingColor -> This function will
--test for the BinaryFailingColor Flag.
isBinaryFailingColor :: Flag -> Bool
isBinaryFailingColor (BinaryFailingColor _) = True
isBinaryFailingColor _                      = False

--isTrinaryHeadColor -> This function will
--test for the TrinaryHeadColor Flag.
isTrinaryHeadColor :: Flag -> Bool
isTrinaryHeadColor (TrinaryHeadColor _) = True
isTrinaryHeadColor _                    = False

--isTrinaryMiddleColor -> This function will
--test for the TrinaryMiddleColor Flag.
isTrinaryMiddleColor :: Flag -> Bool
isTrinaryMiddleColor (TrinaryMiddleColor _) = True
isTrinaryMiddleColor _                      = False

--isTrinaryTailColor -> This function will
--test for the TrinaryTailColor Flag.
isTrinaryTailColor :: Flag -> Bool
isTrinaryTailColor (TrinaryTailColor _) = True
isTrinaryTailColor _                    = False

--isNAColor -> This function will
--test for the NAColor Flag.
isNAColor :: Flag -> Bool
isNAColor (NAColor _) = True
isNAColor _           = False

{------------------------------------------}


{-Custom extraction functions for Flag Datatype.-}

--extractOutputFileType -> This function will
--extract the string associated with
--OutputFileType.
extractOutputFileType :: Flag -> String
extractOutputFileType (OutputFileType x) = x

--extractOutputFileName -> This function will
--extract the string associated with 
--OutputFileName.
extractOutputFileName :: Flag -> String
extractOutputFileName (OutputFileName x) = x

--extractOutputSheetName -> This function will
--extract the string associated with
--OutputSheetName.
extractOutputSheetName :: Flag -> String
extractOutputSheetName (OutputSheetName x) = x

--extractFilterFields -> This function will
--extract the string associated with 
--FilterFields.
extractFilterFields :: Flag -> String
extractFilterFields (FilterFields x) = x

--extractCopyColumnFormatting -> This function will
--extract the string associated with
--CopyColumnFormatting.
extractCopyColumnFormatting :: Flag -> String
extractCopyColumnFormatting (CopyColumnFormatting x) = x

--extractBinaryPassingColor -> This function will
--extract the string associated with
--BinaryPassingColor.
extractBinaryPassingColor :: Flag -> String
extractBinaryPassingColor (BinaryPassingColor x) = x

--extractBinaryFailingColor -> This function will
--extract the string associated with
--BinaryFailingColor.
extractBinaryFailingColor :: Flag -> String
extractBinaryFailingColor (BinaryFailingColor x) = x

--extractTrinaryHeadColor -> This function will
--extract the string associated with
--TrinaryHeadColor.
extractTrinaryHeadColor :: Flag -> String
extractTrinaryHeadColor (TrinaryHeadColor x) = x

--extractTrinaryMiddleColor -> This function will
--extract the string associated with
--TrinaryMiddleColor.
extractTrinaryMiddleColor :: Flag -> String
extractTrinaryMiddleColor (TrinaryMiddleColor x) = x

--extractTrinaryTailColor -> This function will
--extract the string associated with
--TrinaryTailColor.
extractTrinaryTailColor :: Flag -> String
extractTrinaryTailColor (TrinaryTailColor x) = x

--extractNAColor -> This function will
--extract the string associated with
--NAColor.
extractNAColor :: Flag -> String
extractNAColor (NAColor x) = x

{------------------------------------------------}


{-Option Description function relating to datatype above.-}

--options -> This function will
--describe flags.
options :: [OptDescr Flag]
options =
    [ Option ['v']     ["verbose"]              (NoArg Verbose)                                      "Output on stderr.",
      Option ['V','?'] ["version"]              (NoArg Version)                                      "Show version number.",
      Option ['t']     ["outputfiletype"]       (ReqArg OutputFileType "OUTFILETYPE")                "The output file type (tsv or xlsx).",
      Option ['o']     ["outputfilename"]       (ReqArg OutputFileName "OUTFILENAME")                "The output file name.",
      Option ['s']     ["outputsheetname"]      (ReqArg OutputSheetName "OUTSHEETNAME")              "The string to be used as the xlsx sheet name.",
      Option ['F']     ["filterfields"]         (ReqArg FilterFields "FIELDS")                       "The fields to filter on.",
      Option ['S']     ["addfilteringstatus"]   (NoArg AddFilteringStatus)                           "Add column to end of file describing\n\
                                                                                                     \the filtering status of each row.\n",
      Option ['B']     ["addfilteringbinaries"] (NoArg AddFilteringBinaries)                         "Add a column for each BINARY filter applied\n\
                                                                                                     \denoting whether that variant passed (1) or\n\
                                                                                                     \failed (0) that filter.\n",
      Option ['c']     ["copycolumnformatting"] (ReqArg CopyColumnFormatting "COPYCOLUMNFORMATTING") "Copy column formatting of one column to\n\
                                                                                                     \to another column (row-wise).\n",
      Option ['p']     ["binarypassingcolor"]   (ReqArg BinaryPassingColor "BINARYPASSCOLOR")        "The ARGB hex value to use to fill passing cells with.\n\
                                                                                                     \Default value: #FFFF0000\n",
      Option ['f']     ["binaryfailingcolor"]   (ReqArg BinaryFailingColor "BINARYFAILCOLOR")        "The ARGB hex value to use to fill failing cells with.\n\
                                                                                                     \Default value: #FF00FF00\n",
      Option ['h']     ["trinaryheadcolor"]     (ReqArg TrinaryHeadColor "TRINARYHEADCOLOR")         "The ARGB hex value to use to fill head cells with.\n\
                                                                                                     \Default value: #FFFF0000\n",
      Option ['m']     ["trinarymiddlecolor"]   (ReqArg TrinaryMiddleColor "TRINARYMIDCOLOR")        "The ARGB hex value to use to fill middle cells with.\n\
                                                                                                     \Default value: #FFFFFF33\n",
      Option ['l']     ["trinarytailcolor"]     (ReqArg TrinaryTailColor "TRINARYTAILCOLOR")         "The ARGB hex value to use to fill tail cells with.\n\
                                                                                                     \Default value: ##FF00FF00\n",
      Option ['n']     ["nacolor"]              (ReqArg NAColor "NACOLOR")                           "The ARGB hex value to use to fill non-filtered cells with.\n\
                                                                                                     \Default value: #FFC0C0C0\n",  
      Option []        ["help"]                 (NoArg Help)                                         "Print this help message.\n"
    ] 

{---------------------------------------------------------}


{-Function to correctly parse the flags.-}

--compilerOpts -> This function will
--parse incoming command line arguments.
compilerOpts :: [String] -> IO ([Flag],String)
compilerOpts argv =
    case getOpt Permute options argv of
        (args,file,[]) ->
            if | DL.elem Help args ->
               do SIO.hPutStrLn stderr (greeting ++ SCG.usageInfo header options)
                  SX.exitWith SX.ExitSuccess
               | DL.elem Version args ->
               do SIO.hPutStrLn stderr (greeting ++ version ++ "\n" ++ SCG.usageInfo header options)
                  SX.exitWith SX.ExitSuccess
               | DL.length file > 1 ->
               do SIO.hPutStrLn stderr (flerror ++ github ++ "\n" ++ SCG.usageInfo header options)
                  SX.exitWith (SX.ExitFailure 1)
               | DL.length (DL.filter (isFilterFields) args) < 1 ->
               do SIO.hPutStrLn stderr (ffmiss ++ github ++ "\n" ++ SCG.usageInfo header options)
                  SX.exitWith (SX.ExitFailure 1)
               | (DL.length (DL.filter (isFilterFields) args) > 0) && 
                 (not (filterFieldsCheck (extractFilterFields (DL.head (DL.filter (isFilterFields) args))))) ->
               do SIO.hPutStrLn stderr (fferror ++ github ++ "\n" ++ SCG.usageInfo header options)
                  SX.exitWith (SX.ExitFailure 1)
               | (DL.length (DL.filter (isOutputFileType) args) > 0) && 
                 (DL.length (DL.filter (isOutputFileName) args) < 1) ->
               do SIO.hPutStrLn stderr (outfilenamemiss ++ suffpossible ++ "\n" ++ SCG.usageInfo header options)
                  SX.exitWith (SX.ExitFailure 1)
               | (DL.length (DL.filter (isOutputFileName) args) > 0) &&
                 (DL.length (DL.filter (isOutputFileType) args) < 1) ->
               do SIO.hPutStrLn stderr (outtypemiss ++ outtypepossible ++ "\n" ++ SCG.usageInfo header options)
                  SX.exitWith (SX.ExitFailure 1) 
               | (DL.length (DL.filter (isOutputFileType) args) > 0) &&
                 (DL.length (DL.filter (isOutputFileName) args) > 0) &&
                 (not (checkOutputFileTypeAndOutputFileNameFormat 
                 (extractOutputFileType 
                 (DL.head (DL.filter (isOutputFileType) args))) 
                 (extractOutputFileName (DL.head (DL.filter (isOutputFileName) 
                 args))))) ->
               do SIO.hPutStrLn stderr (outtypesuffmm ++ outtypepossible ++ suffpossible ++ "\n" ++ SCG.usageInfo header options)
                  SX.exitWith (SX.ExitFailure 1)
               | extractOutputFileType (DL.head (DL.filter (isOutputFileType) args)) == "tsv" &&
                 (DL.length (DL.filter (isOutputSheetName) args) > 0) ->
               do SIO.hPutStrLn stderr (outsheetnamemm ++ "\n" ++ SCG.usageInfo header options)
                  SX.exitWith (SX.ExitFailure 1)
               | (DL.length (DL.filter (isOutputSheetName) args) > 0) &&
                 (not (checkOutputSheetName (extractOutputSheetName (DL.head (DL.filter (isOutputSheetName) args))))) ->
               do SIO.hPutStrLn stderr (outsheeterror ++ "\n" ++ SCG.usageInfo header options)
                  SX.exitWith (SX.ExitFailure 1)
               | (DL.length (DL.filter (isBinaryPassingColor) args) > 0) &&
                 (not (isHexList (extractBinaryPassingColor (DL.head (DL.filter (isBinaryPassingColor) args))))) ->
               do SIO.hPutStrLn stderr (hexferror ++ hexf ++ "\n" ++ SCG.usageInfo header options)
                  SX.exitWith (SX.ExitFailure 1)
               | (DL.length (DL.filter (isBinaryFailingColor) args) > 0) &&
                 (not (isHexList (extractBinaryFailingColor (DL.head (DL.filter (isBinaryFailingColor) args))))) ->
               do SIO.hPutStrLn stderr (hexferror ++ hexf ++ "\n" ++ SCG.usageInfo header options)
                  SX.exitWith (SX.ExitFailure 1)
               | (DL.length (DL.filter (isTrinaryMiddleColor) args) > 0) &&
                 (not (isHexList (extractTrinaryMiddleColor (DL.head (DL.filter (isTrinaryMiddleColor) args))))) ->
               do SIO.hPutStrLn stderr (hexferror ++ hexf ++ "\n" ++ SCG.usageInfo header options)
                  SX.exitWith (SX.ExitFailure 1)
               | (DL.length (DL.filter (isNAColor) args) > 0) &&
                 (not (isHexList (extractNAColor (DL.head (DL.filter (isNAColor) args))))) -> 
               do SIO.hPutStrLn stderr (hexferror ++ hexf ++ "\n" ++ SCG.usageInfo header options)
                  SX.exitWith (SX.ExitFailure 1)
               | otherwise -> return (DL.nub args, DL.concat file) 
        (_,_,errors) -> do
            SIO.hPutStrLn stderr (DL.concat errors ++ SCG.usageInfo header options)
            SX.exitWith (SX.ExitFailure 1)
        where 
            greeting         = "Filtering Analysis Tool, Copyright (c) 2020 Matthew Mosior.\n"
            header           = "Usage: Fat [-vV?tosFSBcpfhmln] [tsv]\n"
            version          = "Filtering Analysis Tool (FAT), Version 1.0.\n"
            github           = "Please see https://github.com/Matthew-Mosior/Filtering-Analysis-Tool/wiki for more information.\n" 
            flerror          = "Incorrect number of input files:  Please provide one input file.\n"
            fferror          = "Incorrect structure of the filtration string (;?:~~;).\n"
            ffmiss           = "Filtration string missing.\nPlease define a \ 
                               \filtration string using the -F (--filterfields) argument.\n"
            outtypes         = "Possible output file formats are tsv and xlsx.\n"
            outtypeferror    = "Output file format not recognized.\n"
            outtypemiss      = "Output file type missing.\nPlease provide an output file format.\n"
            outfilenamemiss  = "Output file name missing.\nPlease provide an output file name.\n"
            outsheetnamemm   = "Xlsx sheet name not applicable for tsv output file.\n\
                               \If outputting tsv, do not provide out -s (--outputsheetname) argument.\n"
            outsheeterror    = "Please provide xlsx accepted characters for sheet names (\\/*?:[].)\n\
                               \less than 32 characters long to the -s (--outputsheetname) argument.\n"
            hexferror        = "Incorrect hex format.\n"
            hexf             = "ASCII hexadecimal digits: '0'..'9', 'a'..'f', 'A'..'F'.\n"
            sufferror        = "File extension not allowed.\n"
            suffpossible     = "Allowed file extensions are:\n.tsv\n.xlsx\n"
            outtypepossible  = "Allowed output file formats are:\ntsv\nxlsx\n"
            outtypesuffmm    = "Output file format and output file extension are mismatched.\n\
                               \Please ensure output file type and output file extension are the same:\n\
                               \tsv  <-> .tsv\n\
                               \xlsx <-> .xlsx\n"

{----------------------------------------}


{-General Utility Functions.-}

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

{----------------------------}


{-OutputFileType and OutputFileName function.-}

--checkOutputFileTypeAndOutputFileNameFormat -> This function will
--check the format of 
--the OutputFileType and 
--OutputFileName string.
checkOutputFileTypeAndOutputFileNameFormat :: String -> String -> Bool
checkOutputFileTypeAndOutputFileNameFormat [] [] = False
checkOutputFileTypeAndOutputFileNameFormat [] _  = False
checkOutputFileTypeAndOutputFileNameFormat _  [] = False
checkOutputFileTypeAndOutputFileNameFormat xs ys = if (xs == "tsv" && (ys =~ ("\\.tsv$" :: String) :: Bool)) ||
                                                      (xs == "xlsx" && (ys =~ ("\\.xlsx$" :: String) :: Bool))
                                                       then True
                                                       else False

{---------------------------}


{-OutputSheetName function.-}

checkOutputSheetName :: String -> Bool
checkOutputSheetName [] = False
checkOutputSheetName xs = if DL.length xs <= 31 &&
                             DL.all (\x -> x `DL.notElem` "\\/*?:[].") xs
                              then True
                              else False

{---------------------------}


{-FilterFields Functions.-}

--ffgenerator -> This function will
--generate the list of acceptable
--numbers of filterfield delimiters.
ffgenerator :: [Int] -> [Int]
ffgenerator xs = xs ++ ffgenerator (DL.map (\n -> n + 5) xs)

--filterFieldsCheck -> This function will
--check for the appropriate number of 
--delimiters present in FilterFields.
filterFieldsCheck :: String -> Bool
filterFieldsCheck xs = if (DL.elem flaglength ffdelimiterlengths) && 
                          ((DL.filter (flip DL.elem ("?;:~" :: String)) xs) == ffdelimitercycler)
                           then True
                           else False
    where
        flaglength         = DL.length (DL.filter (flip DL.elem ("?;:~" :: String)) xs)
        ffdelimiterlengths = DL.take (DL.length (DL.filter (flip DL.elem ("?;:~" :: String)) xs))
                           $ ffgenerator [6]
        ffdelimitercycler = DL.concat (DL.take (DL.length (DL.filter (flip DL.elem ("?;:~" :: String)) xs)) 
                            (DL.cycle [";","?",":","~","~"]))

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
        regexPredicate xs ys = if (Main.tripletFst (DL.head ys)) `DL.elem` xs 
                                   then False
                                   else True 
        --------------------------------

--reorderList -> This function will
--reorder a list based on another list.
reorderList :: [[(String,Int,Int)]] -> [[(String,Int,Int,String)]] -> [[(String,Int,Int,String)]]
reorderList []     [] = []
reorderList []     _  = []
reorderList _      [] = []
reorderList (x:xs) ys = (DL.filter (\y -> quadrupletFst (y DL.!! 0) == Main.tripletFst (x DL.!! 0) &&
                                          quadrupletSnd (y DL.!! 0) == Main.tripletSnd (x DL.!! 0) &&
                                          quadrupletThrd (y DL.!! 0) == Main.tripletThrd (x DL.!! 0)) ys)
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

--filterFields -> This function will
--filter a field by the corresponding
--field.
filterFields :: [Flag] -> [[String]] -> [[(String,Int,Int,String)]]
filterFields []   [] = []
filterFields opts xs = do --Grab just "FIELDS". 
                          let ffields = singleunnest (DL.filter (isFilterFields) opts)
                          --Extract the string from FilterFields. 
                          let ffstring = extractFilterFields ffields
                          --Remove beginning and ending delimiters.
                          let begendremoved = DL.init (DL.tail ffstring)
                          --Push the separate filtrations into a list.
                          let filteringlist = DLS.splitOn ";" begendremoved
                          --Get the field separated from the filtration condition.
                          let fieldandcondition = DL.map (DLS.splitOneOf "?:~") filteringlist 
                          --Add indexes to xs.
                          let indexedxs = indexAdder xs
                          --Call specificFilters on fieldandcondition. 
                          let specificfiltered = specificFilters fieldandcondition (DL.transpose indexedxs)
                          --Add back the nonfilteredlists.
                          let nonfiltersadded = addNonFilters fieldandcondition (DL.transpose indexedxs) specificfiltered 
                          --Reorder nonfiltersadded.
                          let reorderedlist = reorderList (DL.transpose indexedxs) nonfiltersadded 
                          --Tranpose reorderedlist.
                          let transposedreorderedlist = DL.transpose reorderedlist 
                               --User provides AddFilteringStatus and AddFilteringBinaries flag.
                          if | DL.elem AddFilteringStatus opts &&
                               DL.elem AddFilteringBinaries opts -> 
                             do --Add Pass or Fail remark to end of all but first list of lists.
                                let prefinalizedtransposedlist = [DL.head transposedreorderedlist]
                                                              ++ (DL.map (\x -> 
                                                                     if (DL.any (\(_,_,_,d) -> d == "BINARYNO") x) 
                                                                         then x ++ [("Fail"
                                                                                    ,quadrupletSnd (DL.last x)
                                                                                    ,(quadrupletThrd (DL.last x)) + 1
                                                                                    ,"BINARYNO")]
                                                                             else if (DL.all (\(_,_,_,d) -> (d == "NA") ||
                                                                                                            (d == "TRINARYHEAD") ||
                                                                                                            (d == "TRINARYMIDDLE") ||
                                                                                                            (d == "TRINARYTAIL")) x)
                                                                                 then x ++ [("Not_filtered"
                                                                                            ,quadrupletSnd (DL.last x)
                                                                                            ,(quadrupletThrd (DL.last x)) + 1
                                                                                            ,"NA")]
                                                                                 else x ++ [("Pass"
                                                                                            ,quadrupletSnd (DL.last x)
                                                                                            ,(quadrupletThrd (DL.last x)) + 1
                                                                                            ,"BINARYYES")]) 
                                                                 (DL.tail transposedreorderedlist))
                                --Add extra column header to Name Pass/Fail column just added.
                                let filteringstatus = [DL.head prefinalizedtransposedlist ++ [("Filtering_Status"
                                                                                              ,0
                                                                                              ,DL.length prefinalizedtransposedlist + 1
                                                                                              ,"HEADER")]]
                                                   ++ (DL.tail prefinalizedtransposedlist)
                                --Add Pass (1) or Fail (0) binary notation to each filtered column.
                                let finalfilteringstatus = DL.transpose (addFilteringBinaryColumns 
                                                                        (DL.filter (\x -> (x DL.!! 0) == "BINARY") fieldandcondition) 
                                                                        (DL.transpose filteringstatus)
                                                                        ([((DL.length filteringstatus) + 1)..((DL.length filteringstatus) 
                                                                      + (DL.length (DL.filter (\x -> (x DL.!! 0) == "BINARY") 
                                                                        fieldandcondition)))]))
                                --Check for CopyColumnFormatting flag.
                                if | DL.length (DL.filter (isCopyColumnFormatting) opts) > 0 -> 
                                   do let copycolumnformattingstr = extractCopyColumnFormatting (DL.head 
                                                                                                (DL.filter 
                                                                                                (isCopyColumnFormatting) opts))
                                      let allcolumnssplit = DL.map (\[x,y] -> (x,y))
                                                            (DL.map 
                                                            (DLS.splitOn ":")
                                                            (DLS.splitOn ";"
                                                            (DL.init
                                                            (DL.tail copycolumnformattingstr))))
                                      let alldestinationcolumns = DL.map (snd) allcolumnssplit
                                      --Return output of copyColumnFormatting.
                                      DL.transpose (copyColumnFormatting (DL.transpose finalfilteringstatus) 
                                                                         (allcolumnssplit)
                                                                         (alldestinationcolumns))
                                   | otherwise -> finalfilteringstatus 
                                                   
                                                    
                               --User provides only AddFilteringStatus flag.
                             | DL.elem AddFilteringStatus opts &&
                               DL.notElem AddFilteringBinaries opts -> 
                             do --Add Pass or Fail remark to end of all but first list of lists.
                                let prefinalizedtransposedlist = [DL.head transposedreorderedlist]
                                                              ++ (DL.map (\x ->
                                                                     if (DL.any (\(_,_,_,d) -> d == "BINARYNO") x)
                                                                         then x ++ [("Fail"
                                                                                    ,quadrupletSnd (DL.last x)
                                                                                    ,(quadrupletThrd (DL.last x)) + 1
                                                                                    ,"BINARYNO")]
                                                                             else if (DL.all (\(_,_,_,d) -> (d == "NA") ||
                                                                                                            (d == "TRINARYHEAD") ||
                                                                                                            (d == "TRINARYMIDDLE") ||
                                                                                                            (d == "TRINARYTAIL")) x)
                                                                                 then x ++ [("Not_filtered"
                                                                                            ,quadrupletSnd (DL.last x)
                                                                                            ,(quadrupletThrd (DL.last x)) + 1
                                                                                            ,"NA")]
                                                                                 else x ++ [("Pass"
                                                                                            ,quadrupletSnd (DL.last x)
                                                                                            ,(quadrupletThrd (DL.last x)) + 1
                                                                                            ,"BINARYYES")])
                                                                 (DL.tail transposedreorderedlist))
                                --Add extra column header to Name Pass/Fail column just added.
                                let finalizedtransposedlist = [DL.head prefinalizedtransposedlist ++ [("Filtering_Status"
                                                                                                      ,0
                                                                                                      ,DL.length prefinalizedtransposedlist + 1
                                                                                                      ,"HEADER")]]
                                                           ++ (DL.tail prefinalizedtransposedlist)
                                --Check for CopyColumnFormatting flag.
                                if | DL.length (DL.filter (isCopyColumnFormatting) opts) > 0 ->
                                   do let copycolumnformattingstr = extractCopyColumnFormatting (DL.head
                                                                                                (DL.filter
                                                                                                (isCopyColumnFormatting) opts))
                                      let allcolumnssplit = DL.map (\[x,y] -> (x,y))
                                                            (DL.map
                                                            (DLS.splitOn ":")
                                                            (DLS.splitOn ";"
                                                            (DL.init
                                                            (DL.tail copycolumnformattingstr))))
                                      let alldestinationcolumns = DL.map (snd) allcolumnssplit
                                      --Return output of copyColumnFormatting.
                                      DL.transpose (copyColumnFormatting (DL.transpose finalizedtransposedlist)
                                                                         (allcolumnssplit)
                                                                         (alldestinationcolumns))
                                   | otherwise -> finalizedtransposedlist 
                                     
                                      
                               --User provides only AddFilteringBinaries flag.
                             | DL.notElem AddFilteringStatus opts &&
                               DL.elem AddFilteringBinaries opts -> 
                             do --Add Pass (1) or Fail (0) binary notation to each filtered column.
                                let finalizedbinarycolumns = DL.transpose (addFilteringBinaryColumns
                                                                          (DL.filter (\x -> (x DL.!! 0) == "BINARY") fieldandcondition)
                                                                          (DL.transpose transposedreorderedlist)
                                                                          ([((DL.length transposedreorderedlist) + 1)..((DL.length transposedreorderedlist)
                                                                        + (DL.length (DL.filter (\x -> (x DL.!! 0) == "BINARY")
                                                                          fieldandcondition)))]))
                                --Check for CopyColumnFormatting flag.
                                if | DL.length (DL.filter (isCopyColumnFormatting) opts) > 0 ->
                                   do let copycolumnformattingstr = extractCopyColumnFormatting (DL.head
                                                                                                (DL.filter
                                                                                                (isCopyColumnFormatting) opts))
                                      let allcolumnssplit = DL.map (\[x,y] -> (x,y))
                                                            (DL.map
                                                            (DLS.splitOn ":")
                                                            (DLS.splitOn ";"
                                                            (DL.init
                                                            (DL.tail copycolumnformattingstr))))
                                      let alldestinationcolumns = DL.map (snd) allcolumnssplit
                                      --Return output of copyColumnFormatting.
                                      DL.transpose (copyColumnFormatting (DL.transpose finalizedbinarycolumns)
                                                                         (allcolumnssplit)
                                                                         (alldestinationcolumns))
                                   | otherwise -> finalizedbinarycolumns 
                               --User didn't provide AddFilteringStatus or AddFilteringBinaries flag.
                             | otherwise -> 
                             do --Check for CopyColumnFormatting flag.
                                if | DL.length (DL.filter (isCopyColumnFormatting) opts) > 0 ->
                                   do let copycolumnformattingstr = extractCopyColumnFormatting (DL.head
                                                                                                (DL.filter
                                                                                                (isCopyColumnFormatting) opts))
                                      let allcolumnssplit = DL.map (\[x,y] -> (x,y))
                                                            (DL.map
                                                            (DLS.splitOn ":")
                                                            (DLS.splitOn ";"
                                                            (DL.init
                                                            (DL.tail copycolumnformattingstr))))
                                      let alldestinationcolumns = DL.map (snd) allcolumnssplit
                                      --Return output of copyColumnFormatting.
                                      DL.transpose (copyColumnFormatting (DL.transpose transposedreorderedlist)
                                                                         (allcolumnssplit)
                                                                         (alldestinationcolumns))
                                   | otherwise -> transposedreorderedlist 
                                                        
                                                         

{-------------------------}


{-Xlsx function and definitions.-}

--createCellMap -> This function will
--create a CellMap using input from 
--[(String,Int,Int,String)] and
--cartesian coordinates for data-occupying cells.
createCellMap :: [(String,Int,Int,String)] -> [(Int,Int)] -> [((Int,Int),Cell)]
createCellMap []     []     = []
createCellMap _      []     = []
createCellMap []     _      = []
createCellMap ((a,_,_,d):xs) (y:ys) = --Header fields (":HEADER").
                              if | d == "HEADER" || 
                                   a == "Filtering_Status" ->
                                   ([(y,Cell { _cellStyle = Just 1
                                             , _cellValue = Just (CellText (DText.pack a))
                                             , _cellComment = Nothing
                                             , _cellFormula = Nothing 
                                             }
                                    )]) ++ (createCellMap xs ys)
                                 --Binary passing fields (":BINARYYES").
                                 | d == "BINARYYES" || 
                                   a == "Pass" ->
                                   ([(y,Cell { _cellStyle = Just 2
                                             , _cellValue = Just (CellText (DText.pack a))
                                             , _cellComment = Nothing
                                             , _cellFormula = Nothing 
                                             }
                                   )]) ++ (createCellMap xs ys)
                                 --Binary failing fields (":BINARYNO").
                                 | d == "BINARYNO"|| 
                                   a == "Fail" ->
                                   ([(y,Cell { _cellStyle = Just 4
                                             , _cellValue = Just (CellText (DText.pack a))
                                             , _cellComment = Nothing
                                             , _cellFormula = Nothing 
                                             }
                                   )]) ++ (createCellMap xs ys)
                                 --Trinary HEAD fields (":TRINARYHEAD").
                                 | d == "TRINARYHEAD" ->
                                   ([(y,Cell { _cellStyle = Just 7
                                             , _cellValue = Just (CellText (DText.pack a))
                                             , _cellComment = Nothing
                                             , _cellFormula = Nothing
                                             }
                                   )]) ++ (createCellMap xs ys)
                                 --Trinary MIDDLE fields (":TRINARYMIDDLE").
                                 | d == "TRINARYMIDDLE" ->
                                   ([(y,Cell { _cellStyle = Just 6
                                             , _cellValue = Just (CellText (DText.pack a))
                                             , _cellComment = Nothing
                                             , _cellFormula = Nothing
                                             }
                                   )]) ++ (createCellMap xs ys)
                                 --Trinary TAIL fields (":TRINARYTAIL").
                                 | d == "TRINARYTAIL" ->
                                   ([(y,Cell { _cellStyle = Just 5
                                             , _cellValue = Just (CellText (DText.pack a))
                                             , _cellComment = Nothing
                                             , _cellFormula = Nothing
                                             }
                                   )]) ++ (createCellMap xs ys)
                                 --NA fields (":NA").
                                 | d == "NA" || d == "Not_filtered" ->
                                   ([(y,Cell { _cellStyle = Just 3
                                             , _cellValue = Just (CellText (DText.pack a))
                                             , _cellComment = Nothing
                                             , _cellFormula = Nothing 
                                             }
                                   )]) ++ (createCellMap xs ys)
                                 --Filtering Binary fields (":FILTERCOLUMN").
                                 | d == "FILTERCOLUMN" ->
                                   ([(y,Cell { _cellStyle = Just 1
                                             , _cellValue = Just (CellText (DText.pack a))
                                             , _cellComment = Nothing
                                             , _cellFormula = Nothing
                                             }
                                   )]) ++ (createCellMap xs ys)
                                 | otherwise -> createCellMap xs ys
        ----------------------

--createAndPrintXlsx -> This function will
--create and print the xlsx file.
createAndPrintXlsx :: [Flag] -> [[(String,Int,Int,String)]] -> IO ()
createAndPrintXlsx [] []   = return ()
createAndPrintXlsx [] _    = return ()
createAndPrintXlsx _  []   = return ()
createAndPrintXlsx opts xs = do
    --Grab just "OUTFILENAME".
    let outfilename = DL.head (DL.filter (isOutputFileName) opts)
    --Extract the string from OutputFileName.
    let outfilenamestring = extractOutputFileName outfilename
    --Calculate xy-coordinates (1-based) for xs.
    let cartcoor = DI.range ((1,1),(DL.length xs,DL.length (DL.head xs)))
    --Create CellMap for fixedxs.
    let finalcellmap = DMap.fromList (createCellMap (DL.concat xs) cartcoor)
    --Add finalcellmap to filledworksheet.
    let filledworksheet = CX.Worksheet { _wsColumnsProperties = defaultcolumnproperties
                                       , _wsRowPropertiesMap = defaultrowpropertiesmap
                                       , _wsCells = finalcellmap
                                       , _wsDrawing = defaultwsdrawing
                                       , _wsMerges = defaultwsmerges
                                       , _wsSheetViews = defaultwssheetviews
                                       , _wsPageSetup = defaultwspagesetup
                                       , _wsConditionalFormattings = defaultwsconditionalformattings
                                       , _wsDataValidations = defaultwsdatavalidations
                                       , _wsPivotTables = defaultwspivottables
                                       , _wsAutoFilter = defaultwsautofilter
                                       , _wsTables = defaultwstables
                                       , _wsProtection = defaultwsprotection
                                       , _wsSharedFormulas = defaultwssharedformulas 
                                       }
    --Check for OutputSheetName flag.
    if DL.length (DL.filter (isOutputSheetName) opts) > 0
        then do --Grab just "OUTFILENAME".
                let outsheetname = DL.head (DL.filter (isOutputSheetName) opts)
                --Extract the string from OutputFileName.
                let outsheetnamestring = extractOutputSheetName outsheetname
                --Add filledworksheet to filledxlsx.
                let filledxlsx = CX.Xlsx { _xlSheets = [(DText.pack (outsheetnamestring),filledworksheet)]
                                         , _xlStyles = renderStyleSheet defaultstylesheet
                                         , _xlDefinedNames = defaultxldefinednames
                                         , _xlCustomProperties = defaultxlcustomproperties
                                         , _xlDateBase = defaultxldatebase
                                         }
                --Grab time.
                currenttime <- DTCP.getPOSIXTime
                --Print out filledxlsx file.
                DBL.writeFile outfilenamestring $ CX.fromXlsx currenttime filledxlsx
        else do --Set xlsxsheetname.
                let xlsxsheetname = TR.subRegex (TR.mkRegex "\\.xlsx$") outfilenamestring ""
                --Add filledworksheet to filledxlsx.
                let filledxlsx = CX.Xlsx { _xlSheets = [(DText.pack (xlsxsheetname),filledworksheet)]
                                         , _xlStyles = renderStyleSheet defaultstylesheet
                                         , _xlDefinedNames = defaultxldefinednames
                                         , _xlCustomProperties = defaultxlcustomproperties
                                         , _xlDateBase = defaultxldatebase 
                                         }
                --Grab time.
                currenttime <- DTCP.getPOSIXTime
                --Print out filledxlsx file.
                DBL.writeFile outfilenamestring $ CX.fromXlsx currenttime filledxlsx

{-----------------}


{-Printing functions.-}

--tempFileCreation -> This function will
--print the file to stdout using
--readProcess of the unix tool cat.
catFile :: [[(String,Int,Int,String)]] -> IO ()
catFile [] = return ()
catFile xs = do
    --Open a temporary file.
    (tempfile,temph) <- SIOT.openTempFile "." "temp.txt"
    --Turn xs into a list of strings.
    let finalxs = DL.map ((DL.map (\x -> DL.intercalate "," x)))
                         (DL.map (DL.map (\x -> listifyTwo x))
                         (DL.map (DL.map (\(a,b,c,d) -> (a,d))) xs))
    --Intercalate a tab, and then a newline into xs.
    let intercalatedxs = DL.intercalate "\n" (DL.map (DL.intercalate "\t") finalxs)
    --Add intercalatedxs to temp.txt.
    SIO.hPutStrLn temph intercalatedxs
    --Close the temporary file's handle.
    SIO.hClose temph
    --Print out the contents of tempfile to the screen using cat unix tool.
    (_,_,_,ph) <- SP.createProcess (SP.proc "cat" [tempfile])
    ec <- SP.waitForProcess ph
    case ec of
        SX.ExitSuccess   -> do _ <- SP.readProcess "rm" [tempfile] []
                               return ()
        SX.ExitFailure _ -> do _ <- error "Could not cat file."
                               _ <- SP.readProcess "rm" [tempfile] []
                               return ()

--printFile -> This function will
--print the file to either stdout
--or to a output file based on
--command-lines options provided.
printFile :: [Flag] -> [[(String,Int,Int,String)]] -> IO ()
printFile []   [] = return ()
printFile []   _  = return ()
printFile _    [] = return ()
printFile opts xs = do
    --Grab just "OUTFILETYPE".
    let outfiletype = DL.head (DL.filter (isOutputFileType) opts)
    --Extract the string from OutputFileType.
    let outfiletypestring = extractOutputFileType outfiletype 
    --Check to see if user provided tsv or xlsx file. 
    if outfiletypestring == "tsv"
        then do --Grab just "OUTFILENAME".
                let outfilename = DL.head (DL.filter (isOutputFileName) opts)
                --Extract the string from OutputFileName.
                let outfilenamestring = extractOutputFileName outfilename
                --Turn xs into a list of strings.
                let finalxs = DL.map ((DL.map (\x -> DL.intercalate "," x)))
                                     (DL.map (DL.map (\x -> listifyTwo x))
                                     (DL.map (DL.map (\(a,b,c,d) -> (a,d))) xs))
                --mapNotLast tabs and newlines in xs.
                let tabsandnewlinesadded = DL.intercalate "\n" (DL.map (DL.intercalate "\t") finalxs)
                --Write the output to the user-specified filename.
                SIO.writeFile (outfilenamestring) $ (tabsandnewlinesadded) 
        else do --Create and print the xlsx file.
                createAndPrintXlsx opts xs

{---------------------}


{-FAT Specific Function.-}

--processArgsAndFiles -> This function will
--walk through each of the command-line
--arguments and files provided by the user.
processArgsAndFiles :: ([Flag],String) -> IO ()
processArgsAndFiles ([],[]) = return () 
processArgsAndFiles (options,inputfile) = do
    --Read in the file.
    readinputfile <- SIO.readFile inputfile
    --Apply lineFeed function to inputfile.
    let processedfile = lineFeed readinputfile 
    --Filter the file based on the filter fields header. 
    let filteredfile = filterFields options processedfile 
    --Print the file to stdout (cat) or to a file.
    if DL.length (DL.filter (isOutputFileType) options) > 0 
        then printFile options filteredfile
        else catFile filteredfile

--processArgsAndContents -> This function will
--walk through each of the command-line
--arguments and files provided by the user.
processArgsAndContents :: ([Flag],String) -> IO ()
processArgsAndContents ([],[]) = return ()
processArgsAndContents (options,content) = do
    --Apply lineFeed function to inputfile.
    let processedfile = lineFeed content
    --Filter the file based on the filter fields header.
    let filteredfile = filterFields options processedfile
    --Print the file to stdout (cat) or to a file.
    if DL.length (DL.filter (isOutputFileType) options) > 0
        then printFile options filteredfile
        else catFile filteredfile

{-------------------------}


{-Main function.-}

main :: IO ()
main = do
    --Get command line arguments.
    (args,files) <- SE.getArgs >>= compilerOpts
    --See if files is null
    if DL.null files
        then do --Get stdin.
                contents <- SIO.getContents
                --Run args and contents through processArgsandContents.
                processArgsAndContents (args,contents)
        else do --Run args and files through processArgsandFiles.
                processArgsAndFiles (args,files)
    
{----------------}
