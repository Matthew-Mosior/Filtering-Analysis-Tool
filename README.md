# Filtering-Analysis-Tool: A Xlsx File Creation Tool

## Introduction

**Filtering-Analysis-Tool (FAT)** is a software tool that takes tab-delimited files and transforms them into fully-featured XLSX files using FAT's own domain-specific language (DSL) to meet user-defined filtering parameters.<br/>

## Prerequisites

**FAT** assumes you have a the [GHC](https://www.haskell.org/ghc/) compiler and packages installed that it imports.  The easiest way to do this is to download the [Haskell Platform](https://www.haskell.org/platform/).<br/><br/>

## Installing required packages

To install the peripheral packages **FAT** requires, you can call the following command assuming you have [cabal](https://www.haskell.org/cabal/), a package manager and build system for Haskell, installed on your system (it comes with the [Haskell Platform](https://www.haskell.org/platform/)).<br/><br/>
`$ cabal install [packagename]`<br/><br/>

**Required packages**
- Codec.Xlsx
- Control.Applicative
- Control.Arrow
- Control.Monad
- Control.Monad (mzero)
- Data.Aeson
- Data.ByteString.Char8
- Data.ByteString.Lazy
- Data.Char
- Data.Foldable
- Data.Hashmap.Lazy
- Data.Hashmap.Strict
- Data.Ix
- Data.List
- Data.List.Split
- Data.Map
- Data.Map.Strict
- Data.Maybe
- Data.Set
- Data.Text
- Data.Time.Clock.POSIX
- Data.Tree
- Data.Tuple
- Data.Yaml
- GHC.Generics
- System.Console.GetOpt
- System.Environment
- System.Exit
- System.IO
- System.IO.Temp
- System.Process
- Text.Read
- Text.Regex
- Text.Regex.TDFA
- YamlParse.Applicative

## Input

**FAT** requires two inputs:<br/><br/>

  1. **Configuration YAML** - The first positional argument to **FAT** is the configuration YAML.  This YAML defines the filtering that will be applied to the user-defined tab-delimited file.<br/><br/>

     **FAT** uses a DSL to allow for filtering in an modular and extensible fashion.<br/><br/>

     Please see the [wiki](https://github.com/Matthew-Mosior/Filtering-Analysis-Tool/wiki) for a full guide on how to set up complex filtering schemes using the configuration YAML.

  2. **Tab-delimited (tsv) file** - The second positional argument to **FAT** is the tab-delimited (tsv) file.  The filtering scheme defined in the configuration YAML will be applied to this tab-delimited file to create the output XLSX file.
