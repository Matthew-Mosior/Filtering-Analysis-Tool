{-=Filtering-Analysis-Tool (FAT): A Haskell-based solution to=-}
{-=analyze filtering schemes applied to tab delimited data.=-}
{-=Author: Matthew Mosior=-}
{-=Version: 1.0=-}
{-=Synopsis:  This Haskell Script will take in=-}
{-=a tsv file and provide a in-depth view of=-}
{-=the user-defined filtering schema provided.=-}
{-=This module will provide xlsx definitions.=-}

{-Lanuguage Extensions.-}

{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE DeriveGeneric      #-}

{----------------------}


{-Module.-}

module YamlParser where

{--------}


{-Import modules.-}

import Control.Applicative as CA
import Control.Monad (mzero)
import Data.List as DL
import Data.Maybe as DMaybe
import Data.Yaml as DYaml
import Data.Aeson as DAeson
import Data.HashMap.Lazy as DHL
import YamlParse.Applicative as YPA
import Data.Map.Strict as DMap
import Data.Text as DText
import GHC.Generics

{----------------}


{-Custom YAML input file Datatype and related functions.-}

data FATConfig = FATConfig { outputfilename       :: Text
                           , outputsheetname      :: Text
                           , stylesheetchoice     :: Text
                           , fullprotection       :: Bool
                           , filtering            :: [Filter]
                           , addfilteringstatus   :: Bool
                           , addfilteringbinaries :: Bool
                           , copycolumnformatting :: Maybe Object
                           , hidecolumns          :: Maybe [Text]
                           , binarypassingcolor   :: Maybe Text
                           , binaryfailingcolor   :: Maybe Text
                           , trinaryheadcolor     :: Maybe Text
                           , trinarymiddlecolor   :: Maybe Text
                           , trinarytailcolor     :: Maybe Text
                           , nacolor              :: Maybe Text
                           } 

data Filter = Filter { filteringtype       :: Text
                     , filteringcolumn     :: Text
                     , filteringcolumntype :: Text
                     , filteringoperator   :: Text
                     , filteringstring     :: FString
                     } 

data FString = BFSNumericChoice BFSNumeric
             | BFSStringChoice BFSString
             | TFSNumericAndStringChoice TFSNumericAndString
             deriving (Eq,Ord,Show,Read)

data BFSNumeric = BFSNumeric { bfsnumericoperator :: Text
                             , bfsnumericnumber   :: Text
                             } deriving (Eq,Ord,Show,Read)

data BFSString = BFSString { bfsstringoperator :: Text
                           , bfsstringliteral  :: [Text]
                           } deriving (Eq,Ord,Show,Read)

data TFSNumericAndString = TFSNumericAndString { tfsnumericstringhead   :: [Text]
                                               , tfsnumericstringmiddle :: [Text]
                                               , tfsnumerstringtail   :: [Text]
                                               } deriving (Eq,Ord,Show,Read)

instance FromJSON FATConfig where
    parseJSON (Object v) = parseFATConfig v
    parseJSON _          = CA.empty
    
parseFATConfig v = FATConfig
    <$> v .:  "output_file_name"
    <*> v .:  "output_sheet_name"
    <*> v .:  "stylesheet_choice"
    <*> v .:  "full_protection"
    <*> v .:  "filters"
    <*> v .:  "add_filtering_status"
    <*> v .:  "add_filtering_binaries"
    <*> v .:? "copy_column_formatting"
    <*> v .:? "hide_columns"
    <*> v .:? "binary_passing_color"
    <*> v .:? "binary_failing_color"
    <*> v .:? "trinary_head_color"
    <*> v .:? "trinary_middle_color"
    <*> v .:? "trinary_tail_color"
    <*> v .:? "na_color" 

instance FromJSON Filter where
    parseJSON (Object v) = parseFilter v
    parseJSON _          = CA.empty

parseFilter v = Filter
    <$> v .: "filtering_type"
    <*> v .: "filtering_column"
    <*> v .: "filtering_column_type"
    <*> v .: "filtering_operator"
    <*> v .: "filtering_string"

instance FromJSON FString where
    parseJSON (Object v)    = fstringValue
        where hasBFSNumeric = DHL.member "bfs_numeric_number" v
              hasBFSString  = DHL.member "bfs_string_literal" v
              fstringValue  = parseFString hasBFSNumeric hasBFSString v
    parseJSON _             = CA.empty

parseFString hasBFSNumeric hasBFSString value
    | hasBFSNumeric = BFSNumericChoice <$> parseBFSNumeric value
    | hasBFSString  = BFSStringChoice  <$> parseBFSString value
    | otherwise     = TFSNumericAndStringChoice <$> parseTFSNumericAndString value

instance FromJSON BFSNumeric where
    parseJSON (Object v) = parseBFSNumeric v
    parseJSON _          = CA.empty

parseBFSNumeric v = BFSNumeric
    <$> v .: "bfs_numeric_operator"
    <*> v .: "bfs_numeric_number"

instance FromJSON BFSString where
    parseJSON (Object v) = parseBFSString v
    parseJSON _          = CA.empty

parseBFSString v = BFSString
    <$> v .: "bfs_string_operator"
    <*> v .: "bfs_string_literal"

instance FromJSON TFSNumericAndString where
   parseJSON (Object v) = parseTFSNumericAndString v
   parseJSON _          = CA.empty

parseTFSNumericAndString v = TFSNumericAndString
    <$> v .: "tfs_numeric_and_string_head"
    <*> v .: "tfs_numeric_and_string_middle"
    <*> v .: "tfs_numeric_and_string_tail"
  
{--------------------------------------------------------}


{-Custom bool functions for FATConfig Datatype.-}

--isOutputFileName -> This function will
--test for outputfilename value constructor for FATConfig.
isOutputFileName :: FATConfig -> Bool
isOutputFileName (FATConfig outputfilename _ _ _ _ _ _ _ _ _ _ _ _ _ _) = True
isOutputFileName _                                                      = False

--isOutputSheetName -> This function will
--test for outputsheetname value constructor for FATConfig.
isOutputSheetName :: FATConfig -> Bool
isOutputSheetName (FATConfig _ outputsheetname _ _ _ _ _ _ _ _ _ _ _ _ _) = True
isOutputSheetName _                                                       = False

--isStyleSheetChoice -> This function will
--test for outputsheetname value constructor for FATConfig.
isStyleSheetChoice :: FATConfig -> Bool
isStyleSheetChoice (FATConfig _ _ stylesheetchoice _ _ _ _ _ _ _ _ _ _ _ _) = True
isStyleSheetChoice _                                                        = False

--isFullProtection -> This function will
--test for the fullprotection value constructor for FATConfig.
isFullProtection :: FATConfig -> Bool
isFullProtection (FATConfig _ _ _ fullprotection _ _ _ _ _ _ _ _ _ _ _) = True
isFullProtection _                                                      = False

--isFilterFields -> This function will
--test for filtering value constructor for FATConfig.
isFiltering :: FATConfig -> Bool
isFiltering (FATConfig _ _ _ _ filtering _ _ _ _ _ _ _ _ _ _) = True
isFiltering _                                                 = False

--isAddFilteringStatus -> This function will
--test for addfilteringstatus value constructor for FATConfig.
isAddFilteringStatus :: FATConfig -> Bool
isAddFilteringStatus (FATConfig _ _ _ _ _ addfilteringstatus _ _ _ _ _ _ _ _ _) = True
isAddFilteringStatus _                                                          = False

--isAddFilteringBinaries -> This function will
--test for addfilteringstatus value constructor for FATConfig.
isAddFilteringBinaries :: FATConfig -> Bool
isAddFilteringBinaries (FATConfig _ _ _ _ _ _ addfilteringbinaries _ _ _ _ _ _ _ _) = True
isAddFilteringBinaries _                                                            = False

--isCopyColumnFormatting -> This function will
--test for copycolumnformatting value constructor for FATConfig.
isCopyColumnFormatting :: FATConfig -> Bool
isCopyColumnFormatting (FATConfig _ _ _ _ _ _ _ copycolumnformatting _ _ _ _ _ _ _) = True
isCopyColumnFormatting _                                                            = False

--isHideColumns -> This function will
--test for hidecolumn value constructor for FATConfig.
isHideColumns :: FATConfig -> Bool
isHideColumns (FATConfig _ _ _ _ _ _ _ _ hidecolumns _ _ _ _ _ _) = True
isHideColumns _                                                   = False

--isBinaryPassingColor -> This function will
--test for the binarypassingcolor value constructor for FATConfig.
isBinaryPassingColor :: FATConfig -> Bool
isBinaryPassingColor (FATConfig _ _ _ _ _ _ _ _ _ binarypassingcolor _ _ _ _ _) = True
isBinaryPassingColor _                                                          = False

--isBinaryFailingColor -> This function will
--test for the binaryfailingcolor value constructor for FATConfig.
isBinaryFailingColor :: FATConfig -> Bool
isBinaryFailingColor (FATConfig _ _ _ _ _ _ _ _ _ _ binaryfailingcolor _ _ _ _) = True
isBinaryFailingColor _                                                          = False

--isTrinaryHeadColor -> This function will
--test for the trinaryheadcolor value constructor for FATConfig.
isTrinaryHeadColor :: FATConfig -> Bool
isTrinaryHeadColor (FATConfig _ _ _ _ _ _ _ _ _ _ _ trinaryheadcolor _ _ _) = True
isTrinaryHeadColor _                                                        = False

--isTrinaryMiddleColor -> This function will
--test for the trinarymiddlecolor value constructor for FATConfig.
isTrinaryMiddleColor :: FATConfig -> Bool
isTrinaryMiddleColor (FATConfig _ _ _ _ _ _ _ _ _ _ _ _ trinarymiddlecolor _ _) = True
isTrinaryMiddleColor _                                                          = False

--isTrinaryTailColor -> This function will
--test for the trinarytailcolor value constructor for FATConfig.
isTrinaryTailColor :: FATConfig -> Bool
isTrinaryTailColor (FATConfig _ _ _ _ _ _ _ _ _ _ _ _ _ trinarytailcolor _) = True
isTrinaryTailColor _                                                        = False

--isNAColor -> This function will
--test for the nacolor value constructor for FATConfig.
isNAColor :: FATConfig -> Bool
isNAColor (FATConfig _ _ _ _ _ _ _ _ _ _ _ _ _ _ nacolor) = True
isNAColor _                                               = False

--isFilteringType -> This function will
--test for the filteringtype value constructor for Filter.
isFilteringType :: Filter -> Bool
isFilteringType (Filter filteringtype _ _ _ _) = True
isFilteringType _                              = False

--isFilteringColumn -> This function will
--test for the filteringcolumn value constructor for Filter.
isFilteringColumn :: Filter -> Bool
isFilteringColumn (Filter _ filteringcolumn _ _ _) = True
isFilteringColumn _                                = False

--isFilteringColumnType -> This function will
--test for the filteringcolumntype value constructor for Filter.
isFilteringColumnType :: Filter -> Bool
isFilteringColumnType (Filter _ _ filteringcolumntype _ _) = True
isFilteringColumnType _                                    = False

--isFilteringOperator -> This function will
--test for the filteringoperator value constructor for Filter.
isFilteringOperator :: Filter -> Bool
isFilteringOperator (Filter _ _ _ filteringoperator _) = True
isFilteringOperator _                                  = False

--isFilteringString -> This function will
--test for the filteringstring value constructor for Filter.
isFilteringString :: Filter -> Bool
isFilteringString (Filter _ _ _ _ filteringstring) = True
isFilteringString _                                = False 

{------------------------------------------}


{-Custom extraction functions for FATConfig Datatype.-}

--extractOutputFileName -> This function will
--extract the string associated with
--outputfilename.
extractOutputFileName :: FATConfig -> String
extractOutputFileName (FATConfig x _ _ _ _ _ _ _ _ _ _ _ _ _ _) = DText.unpack x

--extractOutputSheetName -> This function will
--extract the string associated with
--outputsheetname.
extractOutputSheetName :: FATConfig -> String
extractOutputSheetName (FATConfig _ x _ _ _ _ _ _ _ _ _ _ _ _ _) = DText.unpack x

--extractStyleSheetChoice -> This function will
--extract the string associated with
--stylesheetchoice.
extractStyleSheetChoice :: FATConfig -> String
extractStyleSheetChoice (FATConfig _ _ x _ _ _ _ _ _ _ _ _ _ _ _) = DText.unpack x

--extractFullProtection -> This function will
--extract the Boolean value associated with
--fullprotection.
extractFullProtection :: FATConfig -> Bool
extractFullProtection (FATConfig _ _ _ x _ _ _ _ _ _ _ _ _ _ _) = x

--extractFiltering -> This function will
--extract the string associated with
--filtering.
extractFiltering :: FATConfig -> [Filter]
extractFiltering (FATConfig _ _ _ _ x _ _ _ _ _ _ _ _ _ _) = x

--extractAddFilteringStatus -> This function will
--extract the boolean value associated with
--addfilteringstatus.
extractAddFilteringStatus :: FATConfig -> Bool
extractAddFilteringStatus (FATConfig _ _ _ _ _ x _ _ _ _ _ _ _ _ _) = x

--extractAddFilteringBinaries -> This function will
--extract the boolean value associated with
--addfilteringbinaries.
extractAddFilteringBinaries :: FATConfig -> Bool
extractAddFilteringBinaries (FATConfig _ _ _ _ _ _ x _ _ _ _ _ _ _ _) = x

--extractCopyColumnFormatting -> This function will
--extract the string associated with
--copycolumnformatting.
extractCopyColumnFormatting :: FATConfig -> Maybe Object
extractCopyColumnFormatting (FATConfig _ _ _ _ _ _ _ x _ _ _ _ _ _ _) = x
extractCopyColumnFormatting _                                         = Nothing

--extractHideColumns -> This function will
--extract the string associated with
--hidecolumns.
extractHideColumns :: FATConfig -> Maybe [String]
extractHideColumns (FATConfig _ _ _ _ _ _ _ _ x _ _ _ _ _ _) = Just (DL.map (DText.unpack) (DMaybe.fromJust x))
extractHideColumns _                                         = Nothing

--extractBinaryPassingColor -> This function will
--extract the string associated with
--binarypassingcolor.
extractBinaryPassingColor :: FATConfig -> Maybe String
extractBinaryPassingColor (FATConfig _ _ _ _ _ _ _ _ _ x _ _ _ _ _) = Just (DText.unpack (DMaybe.fromJust x))
extractBinaryPassingColor _                                         = Nothing

--extractBinaryFailingColor -> This function will
--extract the string associated with
--binaryfailingcolor.
extractBinaryFailingColor :: FATConfig -> Maybe String
extractBinaryFailingColor (FATConfig _ _ _ _ _ _ _ _ _ _ x _ _ _ _) = Just (DText.unpack (DMaybe.fromJust x))
extractBinaryFailingColor _                                         = Nothing

--extractTrinaryHeadColor -> This function will
--extract the string associated with
--trinaryheadcolor.
extractTrinaryHeadColor :: FATConfig -> Maybe String
extractTrinaryHeadColor (FATConfig _ _ _ _ _ _ _ _ _ _ _ x _ _ _) = Just (DText.unpack (DMaybe.fromJust x))
extractTrinaryHeadColor _                                         = Nothing

--extractTrinaryMiddleColor -> This function will
--extract the string associated with
--trinarymiddlecolor.
extractTrinaryMiddleColor :: FATConfig -> Maybe String
extractTrinaryMiddleColor (FATConfig _ _ _ _ _ _ _ _ _ _ _ _ x _ _) = Just (DText.unpack (DMaybe.fromJust x))
extractTrinaryMiddleColor _                                         = Nothing

--extractTrinaryTailColor -> This function will
--extract the string associated with
--trinarytailcolor.
extractTrinaryTailColor :: FATConfig -> Maybe String
extractTrinaryTailColor (FATConfig _ _ _ _ _ _ _ _ _ _ _ _ _ x _) = Just (DText.unpack (DMaybe.fromJust x))
extractTrinaryTailColor _                                         = Nothing

--extractNAColor -> This function will
--extract the string associated with
--nacolor.
extractNAColor :: FATConfig -> Maybe String
extractNAColor (FATConfig _ _ _ _ _ _ _ _ _ _ _ _ _ _ x) = Just (DText.unpack (DMaybe.fromJust x))
extractNAColor _                                         = Nothing

--extractFilteringType -> This function will
--extract the string associated with filteringtype.
extractFilteringType :: Filter -> String
extractFilteringType (Filter x _ _ _ _) = DText.unpack x

--extractFilteringColumn -> This function will
--extract the string associated with filteringcolumn.
extractFilteringColumn :: Filter -> String
extractFilteringColumn (Filter _ x _ _ _) = DText.unpack x

--extractFilteringColumnType -> This function will
--extract the string associated with filteringcolumntype.
extractFilteringColumnType :: Filter -> String
extractFilteringColumnType (Filter _ _ x _ _) = DText.unpack x

--extractFilteringOperator -> This function will
--extract the string associated with filteringoperator.
extractFilteringOperator :: Filter -> String
extractFilteringOperator (Filter _ _ _ x _) = DText.unpack x

--extractFilteringString -> This function will
--extract the string associated with filteringstring.
extractFilteringString :: Filter -> FString
extractFilteringString (Filter _ _ _ _ x) = x

--extractBFSNumericOperator -> This function will
--extract the data type associated with bfsnumericoperator.
extractBFSNumericOperator :: FString -> String
extractBFSNumericOperator (BFSNumericChoice (BFSNumeric x _)) = DText.unpack x

--extractBFSNumericNumber -> This function will
--extract the data type associated with bfsnumericnumber.
extractBFSNumericNumber :: FString -> String
extractBFSNumericNumber (BFSNumericChoice (BFSNumeric _ x)) = DText.unpack x

--extractBFSStringOperator -> This function will
--extract the data type associated with bfsstringoperator.
extractBFSStringOperator :: FString -> String
extractBFSStringOperator (BFSStringChoice (BFSString x _)) = DText.unpack x

--extractBFSStringLiteral -> This function will
--extract the data type associated with bfsstringliteral.
extractBFSStringLiteral :: FString -> [String]
extractBFSStringLiteral (BFSStringChoice (BFSString _ x)) = DL.map (\y -> DText.unpack y) x

--extractTFSNumericAndStringHead -> This function will
--extract the data type associated with tfsnumericstringhead.
extractTFSNumericAndStringHead :: FString -> [String]
extractTFSNumericAndStringHead (TFSNumericAndStringChoice (TFSNumericAndString x _ _)) = DL.map (\y -> DText.unpack y) x

--extractTFSNumericAndStringMiddle -> This function will
--extract the data type associated with tfsnumericstringmiddle.
extractTFSNumericAndStringMiddle :: FString -> [String]
extractTFSNumericAndStringMiddle (TFSNumericAndStringChoice (TFSNumericAndString _ x _)) = DL.map (\y -> DText.unpack y) x

--extractTFSNumericAndStringTail -> This function will
--extract the data type associated with tfsnumericstringtail.
extractTFSNumericAndStringTail :: FString -> [String]
extractTFSNumericAndStringTail (TFSNumericAndStringChoice (TFSNumericAndString _ _ x)) = DL.map (\y -> DText.unpack y) x

{------------------------------------------------}
