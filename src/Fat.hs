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
import YamlParser

{----------------}


{-Imports-}

import Codec.Xlsx as CX
import Control.Arrow as CA
import Control.Monad as CM
import Data.Aeson as DAeson
import Data.ByteString.Char8 as DBC
import Data.ByteString.Lazy as DBL
import Data.Char as DC
import Data.HashMap.Strict as DHS
import Data.List as DL
import Data.List.Split as DLS
import Data.Ix as DI
import Data.Map as DMap
import Data.Maybe as DMaybe
import Data.Text as DText
import Data.Time.Clock.POSIX as DTCP
import Data.Tuple as DTuple
import Data.Yaml as DYaml
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


{-Option Description function relating to datatype above.-}

--options -> This function will
--describe flags.
options :: [OptDescr Flag]
options =
    [ Option ['h'] ["help"] (NoArg Help) "Print this help message.\n"
    ] 

{---------------------------------------------------------}


{-Function to correctly parse the flags.-}

--compilerOpts -> This function will
--parse incoming command line arguments.
compilerOpts :: [String] -> IO ([Flag],[String])
compilerOpts argv =
    case getOpt Permute options argv of
        (args,files,[]) ->
            if | DL.elem Help args ->
               do SIO.hPutStrLn stderr (greeting ++ SCG.usageInfo header options)
                  SX.exitWith SX.ExitSuccess
               | otherwise -> return (DL.nub args,files) 
        (_,_,errors) -> do
            SIO.hPutStrLn stderr (DL.concat errors ++ SCG.usageInfo header options)
            SX.exitWith (SX.ExitFailure 1)
        where 
            greeting         = "Filtering Analysis Tool, Copyright (c) 2020 Matthew Mosior.\n"
            header           = "Usage: FAT [-h] [Configuration YAML] [Tab-delimited (tsv) file]\n\
                               \Filtering Analysis Tool (FAT), Version 1.0.\n\
                               \Please see https://github.com/Matthew-Mosior/Filtering-Analysis-Tool/wiki for more information.\n"

{----------------------------------------}


{-Configuration YAML sanitization.-}

--checkStyleSheetChoice -> This function will
--check the format of
--the stylesheetchoice text.
checkStyleSheetChoice :: FATConfig -> Bool
checkStyleSheetChoice xs = if | (extractStyleSheetChoice xs) == "default" ||
                                (extractStyleSheetChoice xs) == "vaccine"
                              -> True
                              | otherwise
                              -> False

--checkOutputFileName -> This function will
--check the format of
--the outputfilename text.
checkOutputFileName :: FATConfig -> Bool
checkOutputFileName xs = if | ((extractOutputFileName xs) =~ ("\\.xlsx$" :: String) :: Bool)
                            -> True
                            | otherwise 
                            -> False

--checkOutputSheetName -> This function will
--check the format of
--the outputsheetname text.
checkOutputSheetName :: FATConfig -> Bool
checkOutputSheetName xs = if | DL.length (extractOutputSheetName xs) <= 31 &&
                               DL.all (\x -> x `DL.notElem` "\\/*?:[].") (extractOutputSheetName xs)
                             -> True
                             | otherwise
                             -> False

--checkCopyColumnFormatting -> This function will
--check the key-value pairs for copycolumnformatting. 
checkCopyColumnFormatting :: [(String,String)] -> [[String]] -> Bool
checkCopyColumnFormatting xs ys = if | DL.all (\x -> x `DL.elem` columns) keycolumns &&
                                       DL.all (\x -> x `DL.elem` columns) valuecolumns
                                     -> True
                                     | otherwise 
                                     -> False
    where
        --Local definitions.--
        keycolumns    = DL.map (fst) xs
        valuecolumns  = DL.map (snd) xs
        columns       = DL.head ys 
        ----------------------

--processConfigurationYaml -> This function will
--sanitize all possible fields in the Configuration YAML.
processConfigurationYaml :: FATConfig -> [[String]] -> Bool
processConfigurationYaml xs ys = if | checkStyleSheetChoice xs                                                            &&
                                      checkOutputFileName xs                                                              &&
                                      checkOutputSheetName xs                                                             &&
                                      checkCopyColumnFormatting (DL.map (\(x,y) -> (DText.unpack x,DText.unpack x)) 
                                                                (DHS.toList (DMaybe.fromJust (extractCopyColumnFormatting xs)))) ys 
                                    -> True
                                    | otherwise 
                                    -> False
                                 

{--------------------------------}


{-Xlsx function and definitions.-}

--createCellMap -> This function will
--create a CellMap using input from 
--[(String,Int,Int,String)] and
--cartesian coordinates for data-occupying cells.
createCellMap :: [(String,Int,Int,String)] -> [(Int,Int)] -> FATConfig -> [((Int,Int),Cell)]
createCellMap []               []     _     = []
createCellMap []               (_:_)  _     = []
createCellMap ((_, _, _, _):_) []     _     = []
createCellMap ((a,_,_,d):xs)   (y:ys) opts  = do --If user defined "default" for StyleSheetChoice.
                                                 if | (extractStyleSheetChoice opts) == "default" ->
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
hideColumns :: [[(String,Int,Int,String)]] -> FATConfig -> [ColumnsProperties]
hideColumns [] _    = []
hideColumns xs opts = do
    --Grab just "HIDECOLUMNS".
    let finalhidecolumnstring = DMaybe.fromJust (extractHideColumns opts)
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
createAndPrintXlsx :: FATConfig -> [[(String,Int,Int,String)]] -> IO ()
createAndPrintXlsx _  []   = return ()
createAndPrintXlsx opts xs = do
    --Extract the string from OutputFileName.
    let outfilenamestring = extractOutputFileName opts
    --Calculate xy-coordinates (1-based) for xs.
    let cartcoor = DI.range ((1,1),(DL.length xs,DL.length (DL.head xs)))
    --Create CellMap for fixedxs.
    let finalcellmap = DMap.fromList (createCellMap (DL.concat xs) cartcoor opts) 
    --Check for FullProtection flag.
    if | DMaybe.fromJust (extractFullProtection opts) -> 
       do --Check for HideColumns flag..
          if | isHideColumns opts ->
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
                if | isOutputSheetName opts ->
                   do --Extract the string from OutputFileName.
                      let outsheetnamestring = extractOutputSheetName opts
                      --Extract the string from OutputFileName.
                      let stylesheetchoicestring = extractStyleSheetChoice opts
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
                      --Extract the string from OutputFileName.
                      let stylesheetchoicestring = extractStyleSheetChoice opts
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
                if | isOutputSheetName opts ->
                   do --Extract the string from OutputFileName.
                      let outsheetnamestring = extractOutputSheetName opts
                      --Extract the string from OutputFileName.
                      let stylesheetchoicestring = extractStyleSheetChoice opts
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
                      --Extract the string from OutputFileName.
                      let stylesheetchoicestring = extractStyleSheetChoice opts
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
          if | isHideColumns opts ->
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
                if | isOutputSheetName opts ->
                   do --Extract the string from OutputFileName.
                      let outsheetnamestring = extractOutputSheetName opts
                      --Extract the string from OutputFileName.
                      let stylesheetchoicestring = extractStyleSheetChoice opts
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
                      --Extract the string from OutputFileName.
                      let stylesheetchoicestring = extractStyleSheetChoice opts
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
                if | isOutputSheetName opts ->
                   do --Extract the string from OutputFileName.
                      let outsheetnamestring = extractOutputSheetName opts
                      --Extract the string from OutputFileName.
                      let stylesheetchoicestring = extractStyleSheetChoice opts
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
                      --Extract the string from OutputFileName.
                      let stylesheetchoicestring = extractStyleSheetChoice opts
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
printFile :: FATConfig -> [[(String,Int,Int,String)]] -> IO ()
printFile _    [] = return ()
printFile opts xs = --Create and print the xlsx file.
                    createAndPrintXlsx opts xs

{---------------------}


{-FAT Specific Function.-}

--processArgsAndFiles -> This function will
--walk through each of the command-line
--arguments and files provided by the user.
processArgsAndFiles :: ([Flag],[String]) -> IO ()
processArgsAndFiles ([],[]) = return () 
processArgsAndFiles (options,inputfiles) = do
    --Read in the file.
    readinputtsv <- SIO.readFile (inputfiles DL.!! 1)
    --Apply lineFeed function to inputfile.
    let processedtsv = lineFeed readinputtsv
    --Read in Configuration YAML.
    readinputyaml <- DBC.readFile (inputfiles DL.!! 0)
    --Decode readinputyaml.
    decodedinputyaml <- 
        case decodeEither' readinputyaml of
            Left exc -> error $ "Could not parse Configuration YAML file: \n" ++ show exc
            Right decodedinputyaml -> return decodedinputyaml 
    --Process and ensure correctly formatting Configuration YAML input.
    if | processConfigurationYaml decodedinputyaml processedtsv
       -> do --Filter the file based on the filter fields header. 
             let filteredfile = filterFields decodedinputyaml processedtsv
             --Print the xlsx file.
             printFile decodedinputyaml filteredfile
       | otherwise 
       -> do --Print out failure message.
             print "Could not sanitize Configuation YAML.\n"
             SX.exitWith (SX.ExitFailure 1)
           
{-------------------------}


{-Main function.-}

main :: IO ()
main = do
    --Get command line arguments.
    (args,files) <- SE.getArgs >>= compilerOpts
    --See if files is null
    if | (DL.length files) /= 2
       -> do --Print error statement and exit.
             print "FAT requires two arguments:\n\
                   \Argument 1: Configuration YAML file\n\
                   \Argument 2: Tab-delimited (tsv) file\n"
             SX.exitWith (SX.ExitFailure 1)
       | otherwise 
       -> do --Run args and files through processArgsandFiles.
             processArgsAndFiles (args,files)
    
{----------------}
