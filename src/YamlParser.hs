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
                           } deriving (Eq,Show,Read)

data Filter = Filter { filteringtype       :: Text
                     , filteringcolumn     :: Text
                     , filteringcolumntype :: Text
                     , filteringoperator   :: Text
                     , filteringstring     :: FString
                     } deriving (Eq,Ord,Show,Read) 

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
extractHideColumns :: FATConfig -> Maybe [Text]
--extractHideColumns (FATConfig _ _ _ _ _ _ _ _ x _ _ _ _ _ _) = Just (DL.map (DText.unpack) (DMaybe.fromJust x))
extractHideColumns (FATConfig _ _ _ _ _ _ _ _ x _ _ _ _ _ _) = x
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
extractBFSNumericOperator (BFSStringChoice _)                 = []
extractBFSNumericOperator (TFSNumericAndStringChoice _)       = []  

--extractBFSNumericNumber -> This function will
--extract the data type associated with bfsnumericnumber.
extractBFSNumericNumber :: FString -> String
extractBFSNumericNumber (BFSNumericChoice (BFSNumeric _ x)) = DText.unpack x
extractBFSNumericNumber (BFSStringChoice _)                 = []
extractBFSNumericNumber (TFSNumericAndStringChoice _)       = []

--extractBFSStringOperator -> This function will
--extract the data type associated with bfsstringoperator.
extractBFSStringOperator :: FString -> String
extractBFSStringOperator (BFSStringChoice (BFSString x _)) = DText.unpack x
extractBFSStringOperator (BFSNumericChoice _)              = []
extractBFSStringOperator (TFSNumericAndStringChoice _)     = []

--extractBFSStringLiteral -> This function will
--extract the data type associated with bfsstringliteral.
extractBFSStringLiteral :: FString -> [String]
extractBFSStringLiteral (BFSStringChoice (BFSString _ x)) = DL.map (\y -> DText.unpack y) x
extractBFSStringLiteral (BFSNumericChoice _)              = []
extractBFSStringLiteral (TFSNumericAndStringChoice _)     = []

--extractTFSNumericAndStringHead -> This function will
--extract the data type associated with tfsnumericstringhead.
extractTFSNumericAndStringHead :: FString -> [String]
extractTFSNumericAndStringHead (TFSNumericAndStringChoice (TFSNumericAndString x _ _)) = DL.map (\y -> DText.unpack y) x
extractTFSNumericAndStringHead (BFSNumericChoice _) = []
extractTFSNumericAndStringHead (BFSStringChoice _)  = []

--extractTFSNumericAndStringMiddle -> This function will
--extract the data type associated with tfsnumericstringmiddle.
extractTFSNumericAndStringMiddle :: FString -> [String]
extractTFSNumericAndStringMiddle (TFSNumericAndStringChoice (TFSNumericAndString _ x _)) = DL.map (\y -> DText.unpack y) x
extractTFSNumericAndStringMiddle (BFSNumericChoice _) = []
extractTFSNumericAndStringMiddle (BFSStringChoice _) = []

--extractTFSNumericAndStringTail -> This function will
--extract the data type associated with tfsnumericstringtail.
extractTFSNumericAndStringTail :: FString -> [String]
extractTFSNumericAndStringTail (TFSNumericAndStringChoice (TFSNumericAndString _ _ x)) = DL.map (\y -> DText.unpack y) x
extractTFSNumericAndStringTail (BFSNumericChoice _) = []
extractTFSNumericAndStringTail (BFSStringChoice _) = []

{------------------------------------------------}
