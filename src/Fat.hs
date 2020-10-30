{-=Filtering-Analysis-Tool (FAT): A Haskell-based solution to=-}
{-=analyze filtering schemes applied to tab delimited data.=-}
{-=Author: Matthew Mosior=-}
{-=Version: 1.0=-}
{-=Synopsis:  This Haskell Script will take in=-} 
{-=a tab-delimited file and provide a in-depth view=-} 
{-=of the user-defined filtering schema provided.=-}


{-Lanuguage Extension.-}

{-# LANGUAGE MultiWayIf #-}

{----------------------}


{-Import modules.-}

import Common
import FatDefinitions
import FilterFields

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


{-Custom bool functions for Flag Datatype.-}

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

--isStyleSheetChoice -> This function will
--test for OutputSheetName flag.
isStyleSheetChoice :: Flag -> Bool
isStyleSheetChoice (StyleSheetChoice _) = True
isStyleSheetChoice _                    = False

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

--isHideColumns -> This function will
--test for HideColumn flag.
isHideColumns :: Flag -> Bool
isHideColumns (HideColumns _) = True
isHideColumns _               = False

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

--extractStyleSheetChoice -> This function will
--extract the string associated with
--StyleSheetChoice.
extractStyleSheetChoice :: Flag -> String
extractStyleSheetChoice (StyleSheetChoice x) = x

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

--extractHideColumns -> This function will
--extract the string associated with
--HideColumns.
extractHideColumns :: Flag -> String
extractHideColumns (HideColumns x) = x

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
    [ Option ['v']     ["verbose"]              (NoArg Verbose)                                      "Output on stderr.\n",
      Option ['V','?'] ["version"]              (NoArg Version)                                      "Show version number.\n",
      Option ['o']     ["outputfilename"]       (ReqArg OutputFileName "OUTFILENAME")                "The output file name.\n",
      Option ['s']     ["outputsheetname"]      (ReqArg OutputSheetName "OUTSHEETNAME")              "The string to be used as the xlsx sheet name.\n",
      Option []        ["stylesheet"]           (ReqArg StyleSheetChoice "STYLESHEET")               "The stylesheet to be used.\n",
      Option []        ["fullprotection"]       (NoArg FullProtection)                               "Protect the workbook.\n",
      Option ['F']     ["filterfields"]         (ReqArg FilterFields "FIELDS")                       "The fields to filter on.\n",
      Option ['S']     ["addfilteringstatus"]   (NoArg AddFilteringStatus)                           "Add column to end of file describing\n\
                                                                                                     \the filtering status of each row.\n",
      Option ['B']     ["addfilteringbinaries"] (NoArg AddFilteringBinaries)                         "Add a column for each BINARY filter applied\n\
                                                                                                     \denoting whether that variant passed (1) or\n\
                                                                                                     \failed (0) that filter.\n",
      Option ['c']     ["copycolumnformatting"] (ReqArg CopyColumnFormatting "COPYCOLUMNFORMATTING") "Copy column formatting of one column to\n\
                                                                                                     \to another column (row-wise).\n",
      Option ['H']     ["hidecolumns"]          (ReqArg HideColumns "HIDECOLUMNS")                   "Columns to hide from view within an excel\n\
                                                                                                     \worksheet.\n",
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
               | DL.length (DL.filter (isStyleSheetChoice) args) < 1 ->
               do SIO.hPutStrLn stderr (sscmiss ++ github ++ "\n" ++ SCG.usageInfo header options)
                  SX.exitWith (SX.ExitFailure 1)
               | DL.length (DL.filter (isStyleSheetChoice) args) > 0 &&
                 (not (checkStyleSheetChoice (extractStyleSheetChoice (DL.head (DL.filter (isStyleSheetChoice) args))))) ->
               do SIO.hPutStrLn stderr (sscerror ++ github ++ SCG.usageInfo header options)
                  SX.exitWith (SX.ExitFailure 1)
               | DL.length file > 1 ->
               do SIO.hPutStrLn stderr (flerror ++ github ++ "\n" ++ SCG.usageInfo header options)
                  SX.exitWith (SX.ExitFailure 1)
               | DL.length (DL.filter (Main.isFilterFields) args) < 1 ->
               do SIO.hPutStrLn stderr (ffmiss ++ github ++ "\n" ++ SCG.usageInfo header options)
                  SX.exitWith (SX.ExitFailure 1)
               | (DL.length (DL.filter (Main.isFilterFields) args) > 0) && 
                 (not (filterFieldsCheck (Main.extractFilterFields (DL.head (DL.filter (Main.isFilterFields) args))))) ->
               do SIO.hPutStrLn stderr (fferror ++ github ++ "\n" ++ SCG.usageInfo header options)
                  SX.exitWith (SX.ExitFailure 1)
               | (DL.length (DL.filter (isOutputFileName) args) < 1) ->
               do SIO.hPutStrLn stderr (outfilenamemiss ++ suffpossible ++ "\n" ++ SCG.usageInfo header options)
                  SX.exitWith (SX.ExitFailure 1)
               | (DL.length (DL.filter (isOutputFileName) args) > 0) &&
                 (not (checkOutputFileName
                 (extractOutputFileName (DL.head (DL.filter (isOutputFileName) 
                 args))))) ->
               do SIO.hPutStrLn stderr (sufferror ++ suffpossible ++ "\n" ++ SCG.usageInfo header options)
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
            header           = "Usage: Fat [-vV?osFSBcHpfhmln] [tsv]\n"
            version          = "Filtering Analysis Tool (FAT), Version 1.0.\n"
            github           = "Please see https://github.com/Matthew-Mosior/Filtering-Analysis-Tool/wiki for more information.\n"
            sscmiss          = "StyleSheet argument missing.\nPlease provide a StyleSheet:\ndefault\nvaccine\n"
            sscerror         = "Please provide one of following StyleSheet choices:\ndefault\nvaccine\n" 
            flerror          = "Incorrect number of input files:  Please provide one input file.\n"
            fferror          = "Incorrect structure of the filtration string (;?:~~;).\n"
            ffmiss           = "Filtration string missing.\nPlease define a \ 
                               \filtration string using the -F (--filterfields) argument.\n"
            outfilenamemiss  = "Output file name missing.\nPlease provide an output file name.\n" 
            outsheeterror    = "Please provide xlsx accepted characters for sheet names (\\/*?:[].)\n\
                               \less than 32 characters long to the -s (--outputsheetname) argument.\n"
            hexferror        = "Incorrect hex format.\n"
            hexf             = "ASCII hexadecimal digits: '0'..'9', 'a'..'f', 'A'..'F'.\n"
            sufferror        = "File extension not allowed.\n"
            suffpossible     = "Allowed file extensions are:\n.xlsx\n" 

{----------------------------------------}


{-StyleSheetChoice function.-}

checkStyleSheetChoice :: String -> Bool
checkStyleSheetChoice [] = False
checkStyleSheetChoice xs = if xs == "default" ||
                              xs == "vaccine"
                               then True
                               else False

{----------------------------}


{-OutputFileName function.-}

--checkOutputFileName -> This function will
--check the format of 
--the OutputFileName string.
checkOutputFileName :: String -> Bool
checkOutputFileName [] = False
checkOutputFileName xs = if (xs =~ ("\\.xlsx$" :: String) :: Bool)
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
 
{-------------------------}


{-Xlsx function and definitions.-}

--createCellMap -> This function will
--create a CellMap using input from 
--[(String,Int,Int,String)] and
--cartesian coordinates for data-occupying cells.
createCellMap :: [(String,Int,Int,String)] -> [(Int,Int)] -> [Flag] -> [((Int,Int),Cell)]
createCellMap []               []     []    = []
createCellMap []               []     (_:_) = []
createCellMap []               (_:_)  _     = []
createCellMap ((_, _, _, _):_) []     _     = []
createCellMap ((a,_,_,d):xs)   (y:ys) opts  = do --Grab just "STYLESHEETCHOICE".
                                                 let stylesheetchoice = DL.head (DL.filter (isStyleSheetChoice) opts)
                                                 --Extract the string from OutputFileName.
                                                 let stylesheetchoicestring = extractStyleSheetChoice stylesheetchoice
                                                 --If user defined "default" for StyleSheetChoice.
                                                 if | stylesheetchoicestring == "default" ->
                                                    --Header fields (":HEADER").
                                                    if | d == "HEADER" ->
                                                         ([(y,Cell { _cellStyle = Just 1
                                                                   , _cellValue = Just (CellText (DText.pack a))
                                                                   , _cellComment = Nothing
                                                                   , _cellFormula = Nothing 
                                                                   }
                                                         )]) ++ (createCellMap xs ys opts)
                                                       --Binary passing fields (":BINARYYES").
                                                       | d == "BINARYYES" ->
                                                         ([(y,Cell { _cellStyle = Just 2
                                                                   , _cellValue = Just (CellText (DText.pack a))
                                                                   , _cellComment = Nothing
                                                                   , _cellFormula = Nothing 
                                                                   }
                                                         )]) ++ (createCellMap xs ys opts)
                                                       --Binary failing fields (":BINARYNO").
                                                       | d == "BINARYNO" ->
                                                         ([(y,Cell { _cellStyle = Just 4
                                                                   , _cellValue = Just (CellText (DText.pack a))
                                                                   , _cellComment = Nothing
                                                                   , _cellFormula = Nothing 
                                                                   }
                                                         )]) ++ (createCellMap xs ys opts)
                                                       --Trinary HEAD fields (":TRINARYHEAD").
                                                       | d == "TRINARYHEAD" ->
                                                         ([(y,Cell { _cellStyle = Just 7
                                                                   , _cellValue = Just (CellText (DText.pack a))
                                                                   , _cellComment = Nothing
                                                                   , _cellFormula = Nothing
                                                                   }
                                                         )]) ++ (createCellMap xs ys opts)
                                                       --Trinary MIDDLE fields (":TRINARYMIDDLE").
                                                       | d == "TRINARYMIDDLE" ->
                                                         ([(y,Cell { _cellStyle = Just 6
                                                                   , _cellValue = Just (CellText (DText.pack a))
                                                                   , _cellComment = Nothing
                                                                   , _cellFormula = Nothing
                                                                   }
                                                         )]) ++ (createCellMap xs ys opts)
                                                       --Trinary TAIL fields (":TRINARYTAIL").
                                                       | d == "TRINARYTAIL" ->
                                                         ([(y,Cell { _cellStyle = Just 5
                                                                   , _cellValue = Just (CellText (DText.pack a))
                                                                   , _cellComment = Nothing
                                                                   , _cellFormula = Nothing
                                                                   }
                                                         )]) ++ (createCellMap xs ys opts)
                                                       --NA fields (":NA").
                                                       | d == "NA" ->
                                                         ([(y,Cell { _cellStyle = Just 3
                                                                   , _cellValue = Just (CellText (DText.pack a))
                                                                   , _cellComment = Nothing
                                                                   , _cellFormula = Nothing 
                                                                   }
                                                         )]) ++ (createCellMap xs ys opts)
                                                       --Filtering Binary fields (":FILTERCOLUMN").
                                                       | d == "FILTERCOLUMN" ->
                                                         ([(y,Cell { _cellStyle = Just 1
                                                                   , _cellValue = Just (CellText (DText.pack a))
                                                                   , _cellComment = Nothing
                                                                   , _cellFormula = Nothing
                                                                   }
                                                         )]) ++ (createCellMap xs ys opts)
                                                       | otherwise -> createCellMap xs ys opts
                                                    | otherwise ->
                                                    --Header fields (":HEADER").
                                                    if | d == "HEADER" ->
                                                         ([(y,Cell { _cellStyle = Just 2
                                                                   , _cellValue = Just (CellText (DText.pack a))
                                                                   , _cellComment = Nothing
                                                                   , _cellFormula = Nothing
                                                                   }
                                                         )]) ++ (createCellMap xs ys opts)
                                                       --Binary passing fields (":BINARYYES").
                                                       | d == "BINARYYES" ->
                                                         ([(y,Cell { _cellStyle = Just 4
                                                                   , _cellValue = Just (CellText (DText.pack a))
                                                                   , _cellComment = Nothing
                                                                   , _cellFormula = Nothing
                                                                   }
                                                         )]) ++ (createCellMap xs ys opts)
                                                       --Binary failing fields (":BINARYNO").
                                                       | d == "BINARYNO" ->
                                                         ([(y,Cell { _cellStyle = Just 6
                                                                   , _cellValue = Just (CellText (DText.pack a))
                                                                   , _cellComment = Nothing
                                                                   , _cellFormula = Nothing
                                                                   }
                                                         )]) ++ (createCellMap xs ys opts)
                                                       --Trinary HEAD fields (":TRINARYHEAD").
                                                       | d == "TRINARYHEAD" ->
                                                         ([(y,Cell { _cellStyle = Just 9
                                                                   , _cellValue = Just (CellText (DText.pack a))
                                                                   , _cellComment = Nothing
                                                                   , _cellFormula = Nothing
                                                                   }
                                                         )]) ++ (createCellMap xs ys opts)
                                                       --Trinary MIDDLE fields (":TRINARYMIDDLE").
                                                       | d == "TRINARYMIDDLE" ->
                                                         ([(y,Cell { _cellStyle = Just 8
                                                                   , _cellValue = Just (CellText (DText.pack a))
                                                                   , _cellComment = Nothing
                                                                   , _cellFormula = Nothing
                                                                   }
                                                         )]) ++ (createCellMap xs ys opts)
                                                       --Trinary TAIL fields (":TRINARYTAIL").
                                                       | d == "TRINARYTAIL" ->
                                                         ([(y,Cell { _cellStyle = Just 7
                                                                   , _cellValue = Just (CellText (DText.pack a))
                                                                   , _cellComment = Nothing
                                                                   , _cellFormula = Nothing
                                                                   }
                                                         )]) ++ (createCellMap xs ys opts)
                                                       --NA fields (":NA").
                                                       | d == "NA" ->
                                                         ([(y,Cell { _cellStyle = Just 5
                                                                   , _cellValue = Just (CellText (DText.pack a))
                                                                   , _cellComment = Nothing
                                                                   , _cellFormula = Nothing
                                                                   }
                                                         )]) ++ (createCellMap xs ys opts)
                                                       --Filtering Binary fields (":FILTERCOLUMN").
                                                       | d == "FILTERCOLUMN" ->
                                                         ([(y,Cell { _cellStyle = Just 3
                                                                   , _cellValue = Just (CellText (DText.pack a))
                                                                   , _cellComment = Nothing
                                                                   , _cellFormula = Nothing
                                                                }
                                                         )]) ++ (createCellMap xs ys opts)
                                                       | otherwise -> createCellMap xs ys opts 
                                               
        ----------------------

--hideColumns -> This function will
--hide column(s) from view within an
--excel worksheet.
hideColumns :: [[(String,Int,Int,String)]] -> [Flag] -> [ColumnsProperties]
hideColumns [] []   = []
hideColumns _  []   = []
hideColumns [] _    = []
hideColumns xs opts = do
    --Grab just "HIDECOLUMNS".
    let hidecolumns = DL.head (DL.filter (isHideColumns) opts)
    --Extract the string from HideColumns.
    let hidecolumnstring = extractHideColumns hidecolumns
    --Prepare hidecolumnstring.
    let finalhidecolumnstring = DLS.splitOn "," (DL.tail (DL.init hidecolumnstring))
    --Call smallHideColumns.
    setColumnsProperties xs finalhidecolumnstring

--setColumnsProperties -> This function will
--set ColumnsProperties.
setColumnsProperties :: [[(String,Int,Int,String)]] -> [String] -> [ColumnsProperties]
setColumnsProperties [] []     = []
setColumnsProperties _  []     = []
setColumnsProperties [] _      = []
setColumnsProperties xs (y:ys) = (smallSetColumnsProperties xs y) ++ (setColumnsProperties xs ys)
    where
        --Local function.--
        --smallSetColumnsProperties
        smallSetColumnsProperties :: [[(String,Int,Int,String)]] -> String -> [ColumnsProperties]
        smallSetColumnsProperties []     [] = []
        smallSetColumnsProperties _      [] = []
        smallSetColumnsProperties []     _  = []
        smallSetColumnsProperties (x:xs) ys = (smallerSetColumnsProperties x ys) ++ (smallSetColumnsProperties xs ys)
        --smallerSetColumnsProperties
        smallerSetColumnsProperties :: [(String,Int,Int,String)] -> String -> [ColumnsProperties]
        smallerSetColumnsProperties xs     ys = if | (quadrupletFst (xs DL.!! 0)) == ys ->
                                                   [ ColumnsProperties
                                                     { cpMin = (quadrupletThrd (xs DL.!! 0)) + 1
                                                     , cpMax = (quadrupletThrd (xs DL.!! 0)) + 1
                                                     , cpWidth = Nothing
                                                     , cpStyle = Nothing
                                                     , cpHidden = True
                                                     , cpCollapsed = True
                                                     , cpBestFit = False
                                                     }]
                                                   | otherwise -> []
   
        ------------------- 

--allColumns -> This function will
--format all columns on an
--excel worksheet.
allColumns :: [[(String,Int,Int,String)]] -> [ColumnsProperties]
allColumns []     = []
allColumns (x:xs) = (smallAllColumns x) ++ (allColumns xs)
    where
        --smallAllColumns
        smallAllColumns :: [(String,Int,Int,String)] -> [ColumnsProperties]
        smallAllColumns [] = []
        smallAllColumns xs = [ ColumnsProperties
                             { cpMin = (quadrupletThrd (xs DL.!! 0)) + 1
                             , cpMax = (quadrupletThrd (xs DL.!! 0)) + 1
                             , cpWidth = Just 14.0
                             , cpStyle = Nothing
                             , cpHidden = False
                             , cpCollapsed = False
                             , cpBestFit = True
                             }]

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
    let finalcellmap = DMap.fromList (createCellMap (DL.concat xs) cartcoor opts)
    --Add finalcellmap to filledworksheet.
    --Check for FullProtection flag.
    if | DL.elem FullProtection opts -> 
       do --Check for HideColumns flag..
          if | DL.length (DL.filter (isHideColumns) opts) > 0 ->
             do --Call hideColumms.
                let filledworksheet = CX.Worksheet { _wsColumnsProperties = hideColumns (DL.transpose xs) opts
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
                                                   , _wsProtection = fullwsprotection
                                                   , _wsSharedFormulas = defaultwssharedformulas
                                                   }
                --Check for OutputSheetName flag.
                if | DL.length (DL.filter (isOutputSheetName) opts) > 0 ->
                   do --Grab just "OUTFILENAME".
                      let outsheetname = DL.head (DL.filter (isOutputSheetName) opts)
                      --Extract the string from OutputFileName.
                      let outsheetnamestring = extractOutputSheetName outsheetname
                      --Grab just "STYLESHEETCHOICE".
                      let stylesheetchoice = DL.head (DL.filter (isStyleSheetChoice) opts)
                      --Extract the string from OutputFileName.
                      let stylesheetchoicestring = extractStyleSheetChoice stylesheetchoice
                      --If stylesheetchoicestring == "default".
                      if | stylesheetchoicestring == "default" ->
                         do --Add filledworksheet to filledxlsx.
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
                         | otherwise ->
                         do --Add filledworksheet to filledxlsx.
                            let filledxlsx = CX.Xlsx { _xlSheets = [(DText.pack (outsheetnamestring),filledworksheet)]
                                                     , _xlStyles = renderStyleSheet vaccinestylesheet
                                                     , _xlDefinedNames = defaultxldefinednames
                                                     , _xlCustomProperties = defaultxlcustomproperties
                                                     , _xlDateBase = defaultxldatebase
                                                     }
                            --Grab time.
                            currenttime <- DTCP.getPOSIXTime
                            --Print out filledxlsx file.
                            DBL.writeFile outfilenamestring $ CX.fromXlsx currenttime filledxlsx
                   | otherwise ->
                   do --Set xlsxsheetname.
                      let xlsxsheetname = TR.subRegex (TR.mkRegex "\\.xlsx$") outfilenamestring ""
                      --Grab just "STYLESHEETCHOICE".
                      let stylesheetchoice = DL.head (DL.filter (isStyleSheetChoice) opts)
                      --Extract the string from OutputFileName.
                      let stylesheetchoicestring = extractStyleSheetChoice stylesheetchoice
                      --If stylesheetchoicestring == "default".
                      if | stylesheetchoicestring == "default" ->
                         do --Add filledworksheet to filledxlsx.
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
                         | otherwise ->
                         do --Add filledworksheet to filledxlsx.
                            let filledxlsx = CX.Xlsx { _xlSheets = [(DText.pack (xlsxsheetname),filledworksheet)]
                                                     , _xlStyles = renderStyleSheet vaccinestylesheet
                                                     , _xlDefinedNames = defaultxldefinednames
                                                     , _xlCustomProperties = defaultxlcustomproperties
                                                     , _xlDateBase = defaultxldatebase
                                                     }
                            --Grab time.
                            currenttime <- DTCP.getPOSIXTime
                            --Print out filledxlsx file.
                            DBL.writeFile outfilenamestring $ CX.fromXlsx currenttime filledxlsx
             | otherwise ->
             do --Use defaultwsprotection as defined in FatDefinitions.hs.
                let filledworksheet = CX.Worksheet { _wsColumnsProperties = allColumns (DL.transpose xs)
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
                                                   , _wsProtection = fullwsprotection
                                                   , _wsSharedFormulas = defaultwssharedformulas
                                                   }
                --Check for OutputSheetName flag.
                if | DL.length (DL.filter (isOutputSheetName) opts) > 0 ->
                   do --Grab just "OUTFILENAME".
                      let outsheetname = DL.head (DL.filter (isOutputSheetName) opts)
                      --Extract the string from OutputFileName.
                      let outsheetnamestring = extractOutputSheetName outsheetname
                      --Grab just "STYLESHEETCHOICE".
                      let stylesheetchoice = DL.head (DL.filter (isStyleSheetChoice) opts)
                      --Extract the string from OutputFileName.
                      let stylesheetchoicestring = extractStyleSheetChoice stylesheetchoice
                      --If stylesheetchoicestring == "default".
                      if | stylesheetchoicestring == "default" ->
                         do --Add filledworksheet to filledxlsx.
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
                         | otherwise ->
                         do --Add filledworksheet to filledxlsx.
                            let filledxlsx = CX.Xlsx { _xlSheets = [(DText.pack (outsheetnamestring),filledworksheet)]
                                                     , _xlStyles = renderStyleSheet vaccinestylesheet
                                                     , _xlDefinedNames = defaultxldefinednames
                                                     , _xlCustomProperties = defaultxlcustomproperties
                                                     , _xlDateBase = defaultxldatebase
                                                     }
                            --Grab time.
                            currenttime <- DTCP.getPOSIXTime
                            --Print out filledxlsx file.
                            DBL.writeFile outfilenamestring $ CX.fromXlsx currenttime filledxlsx
                    | otherwise ->
                    do --Set xlsxsheetname.
                      let xlsxsheetname = TR.subRegex (TR.mkRegex "\\.xlsx$") outfilenamestring ""
                      --Grab just "STYLESHEETCHOICE".
                      let stylesheetchoice = DL.head (DL.filter (isStyleSheetChoice) opts)
                      --Extract the string from OutputFileName.
                      let stylesheetchoicestring = extractStyleSheetChoice stylesheetchoice
                      --If stylesheetchoicestring == "default".
                      if | stylesheetchoicestring == "default" ->
                         do --Add filledworksheet to filledxlsx.
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
                         | otherwise ->
                         do --Add filledworksheet to filledxlsx.
                            let filledxlsx = CX.Xlsx { _xlSheets = [(DText.pack (xlsxsheetname),filledworksheet)]
                                                     , _xlStyles = renderStyleSheet vaccinestylesheet
                                                     , _xlDefinedNames = defaultxldefinednames
                                                     , _xlCustomProperties = defaultxlcustomproperties
                                                     , _xlDateBase = defaultxldatebase
                                                     }
                            --Grab time.
                            currenttime <- DTCP.getPOSIXTime
                            --Print out filledxlsx file.
                            DBL.writeFile outfilenamestring $ CX.fromXlsx currenttime filledxlsx
       | otherwise -> 
       do --Check for HideColumns flag..
          if | DL.length (DL.filter (isHideColumns) opts) > 0 ->
             do --Call hideColumns
                let filledworksheet = CX.Worksheet { _wsColumnsProperties = hideColumns (DL.transpose xs) opts
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
                if | DL.length (DL.filter (isOutputSheetName) opts) > 0 ->
                   do --Grab just "OUTFILENAME".
                      let outsheetname = DL.head (DL.filter (isOutputSheetName) opts)
                      --Extract the string from OutputFileName.
                      let outsheetnamestring = extractOutputSheetName outsheetname
                      --Grab just "STYLESHEETCHOICE".
                      let stylesheetchoice = DL.head (DL.filter (isStyleSheetChoice) opts)
                      --Extract the string from OutputFileName.
                      let stylesheetchoicestring = extractStyleSheetChoice stylesheetchoice
                      --If stylesheetchoicestring == "default".
                      if | stylesheetchoicestring == "default" ->
                         do --Add filledworksheet to filledxlsx.
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
                         | otherwise -> 
                         do --Add filledworksheet to filledxlsx.
                            let filledxlsx = CX.Xlsx { _xlSheets = [(DText.pack (outsheetnamestring),filledworksheet)]
                                                     , _xlStyles = renderStyleSheet vaccinestylesheet
                                                     , _xlDefinedNames = defaultxldefinednames
                                                     , _xlCustomProperties = defaultxlcustomproperties
                                                     , _xlDateBase = defaultxldatebase
                                                     }
                            --Grab time.
                            currenttime <- DTCP.getPOSIXTime
                            --Print out filledxlsx file.
                            DBL.writeFile outfilenamestring $ CX.fromXlsx currenttime filledxlsx
                   | otherwise ->
                   do --Set xlsxsheetname.
                      let xlsxsheetname = TR.subRegex (TR.mkRegex "\\.xlsx$") outfilenamestring ""
                      --Grab just "STYLESHEETCHOICE".
                      let stylesheetchoice = DL.head (DL.filter (isStyleSheetChoice) opts)
                      --Extract the string from OutputFileName.
                      let stylesheetchoicestring = extractStyleSheetChoice stylesheetchoice
                      --If stylesheetchoicestring == "default".
                      if | stylesheetchoicestring == "default" ->
                         do --Add filledworksheet to filledxlsx.
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
                         | otherwise -> 
                         do --Add filledworksheet to filledxlsx.
                            let filledxlsx = CX.Xlsx { _xlSheets = [(DText.pack (xlsxsheetname),filledworksheet)]
                                                     , _xlStyles = renderStyleSheet vaccinestylesheet
                                                     , _xlDefinedNames = defaultxldefinednames
                                                     , _xlCustomProperties = defaultxlcustomproperties
                                                     , _xlDateBase = defaultxldatebase
                                                     }
                            --Grab time.
                            currenttime <- DTCP.getPOSIXTime
                            --Print out filledxlsx file.
                            DBL.writeFile outfilenamestring $ CX.fromXlsx currenttime filledxlsx
             | otherwise -> 
             do --Use defaultwsprotection as defined in FatDefinitions.hs.
                let filledworksheet = CX.Worksheet { _wsColumnsProperties = allColumns (DL.transpose xs)
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
                if | DL.length (DL.filter (isOutputSheetName) opts) > 0 ->
                   do --Grab just "OUTFILENAME".
                      let outsheetname = DL.head (DL.filter (isOutputSheetName) opts)
                      --Extract the string from OutputFileName.
                      let outsheetnamestring = extractOutputSheetName outsheetname
                      --Grab just "STYLESHEETCHOICE".
                      let stylesheetchoice = DL.head (DL.filter (isStyleSheetChoice) opts)
                      --Extract the string from OutputFileName.
                      let stylesheetchoicestring = extractStyleSheetChoice stylesheetchoice
                      --If stylesheetchoicestring == "default".
                      if | stylesheetchoicestring == "default" ->
                         do --Add filledworksheet to filledxlsx.
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
                         | otherwise -> 
                         do --Add filledworksheet to filledxlsx.
                            let filledxlsx = CX.Xlsx { _xlSheets = [(DText.pack (outsheetnamestring),filledworksheet)]
                                                     , _xlStyles = renderStyleSheet vaccinestylesheet
                                                     , _xlDefinedNames = defaultxldefinednames
                                                     , _xlCustomProperties = defaultxlcustomproperties
                                                     , _xlDateBase = defaultxldatebase
                                                     }
                            --Grab time.
                            currenttime <- DTCP.getPOSIXTime
                            --Print out filledxlsx file.
                            DBL.writeFile outfilenamestring $ CX.fromXlsx currenttime filledxlsx
                    | otherwise ->
                    do --Set xlsxsheetname.
                      let xlsxsheetname = TR.subRegex (TR.mkRegex "\\.xlsx$") outfilenamestring ""
                      --Grab just "STYLESHEETCHOICE".
                      let stylesheetchoice = DL.head (DL.filter (isStyleSheetChoice) opts)
                      --Extract the string from OutputFileName.
                      let stylesheetchoicestring = extractStyleSheetChoice stylesheetchoice
                      --If stylesheetchoicestring == "default".
                      if | stylesheetchoicestring == "default" ->
                         do --Add filledworksheet to filledxlsx.
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
                         | otherwise ->
                         do --Add filledworksheet to filledxlsx.
                            let filledxlsx = CX.Xlsx { _xlSheets = [(DText.pack (xlsxsheetname),filledworksheet)]
                                                     , _xlStyles = renderStyleSheet vaccinestylesheet
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

--printFile -> This function will
--print the file to either stdout
--or to a output file based on
--command-lines options provided.
printFile :: [Flag] -> [[(String,Int,Int,String)]] -> IO ()
printFile []   [] = return ()
printFile []   _  = return ()
printFile _    [] = return ()
printFile opts xs = --Create and print the xlsx file.
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
    --Print the xlsx file.
    printFile options filteredfile

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
    --Print the xlsx file.
    printFile options filteredfile

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
