{-=Filtering-Analysis-Tool (FAT): A Haskell-based solution to=-}
{-=analyze filtering schemes applied to tab delimited data.=-}
{-=Author: Matthew Mosior=-}
{-=Version: 1.0=-}
{-=Synopsis:  This Haskell Script will take in=-}
{-=a tsv file and provide a in-depth view of=-}
{-=the user-defined filtering schema provided.=-}
{-=This module will provide xlsx definitions.=-}

{-Module.-}

module FatDefinitions where

{--------}


{-Imports-}

import Codec.Xlsx as CX
import Data.Map as DMap
import Data.Text as DText

{---------}


{-Codec.Xlsx definitions.-}

--Defines default Xlsx data type value.
defaultxlsx :: Xlsx
defaultxlsx = def Xlsx

--Defines default Worksheet data type value.
defaultworksheet :: Worksheet
defaultworksheet = def Worksheet

--Defines default _wsColumnsProperties data type value.
defaultcolumnproperties :: [ColumnsProperties]
defaultcolumnproperties = [ ColumnsProperties
                          { cpMin = 0
                          , cpMax = 0
                          , cpWidth = Nothing
                          , cpStyle = Nothing
                          , cpHidden = False
                          , cpCollapsed = False
                          , cpBestFit = True
                          }]

--Defines default _wsRowPropertiesMap data type value.
defaultrowpropertiesmap :: Map Int RowProperties
defaultrowpropertiesmap = (fromList []) :: Map Int RowProperties

--Defines default _wsDrawing data type value.
defaultwsdrawing :: Maybe a
defaultwsdrawing = Nothing

--Defines default _wsMerges data type value.
defaultwsmerges :: [a]
defaultwsmerges = []

--Defines default _wsSheetViews data type value.
defaultwssheetviews :: Maybe [SheetView]
defaultwssheetviews = Just
    [ SheetView
        { _sheetViewColorId = Nothing
        , _sheetViewDefaultGridColor = Nothing
        , _sheetViewRightToLeft = Nothing
        , _sheetViewShowFormulas = Nothing
        , _sheetViewShowGridLines = Just True
        , _sheetViewShowOutlineSymbols = Nothing
        , _sheetViewShowRowColHeaders = Nothing
        , _sheetViewShowRuler = Nothing
        , _sheetViewShowWhiteSpace = Nothing
        , _sheetViewShowZeros = Nothing
        , _sheetViewTabSelected = Just True
        , _sheetViewTopLeftCell = Nothing
        , _sheetViewType = Nothing
        , _sheetViewWindowProtection = Nothing
        , _sheetViewWorkbookViewId = 0
        , _sheetViewZoomScale = Nothing
        , _sheetViewZoomScaleNormal = Nothing
        , _sheetViewZoomScalePageLayoutView = Nothing
        , _sheetViewZoomScaleSheetLayoutView = Nothing
        , _sheetViewPane = Nothing
        , _sheetViewSelection =
            [ Selection
                { _selectionActiveCell = Just
                    ( CellRef { unCellRef = (DText.pack "E3") } )
                , _selectionActiveCellId = Nothing
                , _selectionPane = Nothing
                , _selectionSqref = Just
                    ( SqRef
                        [ CellRef { unCellRef = (DText.pack "E3") } ]
                    )
                }
            ]
        }
    ]

--Defines default _wsPageSetup data type value.
defaultwspagesetup :: Maybe a
defaultwspagesetup = Nothing

--Defines default _wsConditionalFormattings data type value.
defaultwsconditionalformattings :: Map SqRef ConditionalFormatting
defaultwsconditionalformattings = (fromList []) :: Map SqRef ConditionalFormatting

--Defines default_wsDataValidations data type value.
defaultwsdatavalidations :: Map SqRef DataValidation
defaultwsdatavalidations = (fromList []) :: Map SqRef DataValidation

--Defines default _wsPivotTables data type value.
defaultwspivottables :: [a]
defaultwspivottables = []

--Defines default _wsAutoFilter data type value.
defaultwsautofilter :: Maybe a
defaultwsautofilter = Nothing

--Defines default _wsTables data type value.
defaultwstables :: [a]
defaultwstables = []

--Defines default _wsProtection data type value.
defaultwsprotection :: Maybe a
defaultwsprotection = Nothing

--Full worksheet protection.
fullwsprotection :: Maybe SheetProtection
fullwsprotection = Just fullSheetProtection

--Defines default _wsSharedFormulas data type value.
defaultwssharedformulas :: Map SharedFormulaIndex SharedFormulaOptions
defaultwssharedformulas = (fromList []) ::  Map SharedFormulaIndex SharedFormulaOptions

--Defines the default StyleSheet.
defaultstylesheet :: StyleSheet
defaultstylesheet = StyleSheet
    { _styleSheetBorders =
        [ Border
            { _borderDiagonalDown = Nothing
            , _borderDiagonalUp = Nothing
            , _borderOutline = Nothing
            , _borderBottom = Just
                ( BorderStyle
                    { _borderStyleColor = Nothing
                    , _borderStyleLine = Nothing
                    }
                )
            , _borderDiagonal = Just
                ( BorderStyle
                    { _borderStyleColor = Nothing
                    , _borderStyleLine = Nothing
                    }
                )
            , _borderEnd = Nothing
            , _borderHorizontal = Nothing
            , _borderLeft = Just
                ( BorderStyle
                    { _borderStyleColor = Nothing
                    , _borderStyleLine = Nothing
                    }
                )
            , _borderRight = Just
                ( BorderStyle
                    { _borderStyleColor = Nothing
                    , _borderStyleLine = Nothing
                    }
                )
            , _borderStart = Nothing
            , _borderTop = Just
                ( BorderStyle
                    { _borderStyleColor = Nothing
                    , _borderStyleLine = Nothing
                    }
                )
            , _borderVertical = Nothing
            }
        ]
    , _styleSheetCellXfs =
        [ CellXf
            { _cellXfApplyAlignment = Nothing
            , _cellXfApplyBorder = Nothing
            , _cellXfApplyFill = Nothing
            , _cellXfApplyFont = Nothing
            , _cellXfApplyNumberFormat = Nothing
            , _cellXfApplyProtection = Nothing
            , _cellXfBorderId = Just 0
            , _cellXfFillId = Just 0
            , _cellXfFontId = Just 0
            , _cellXfNumFmtId = Just 0
            , _cellXfPivotButton = Nothing
            , _cellXfQuotePrefix = Nothing
            , _cellXfId = Just 0
            , _cellXfAlignment = Nothing
            , _cellXfProtection = Nothing
            }
        , CellXf
            { _cellXfApplyAlignment = Just True
            , _cellXfApplyBorder = Nothing
            , _cellXfApplyFill = Nothing
            , _cellXfApplyFont = Nothing
            , _cellXfApplyNumberFormat = Nothing
            , _cellXfApplyProtection = Nothing
            , _cellXfBorderId = Just 0
            , _cellXfFillId = Just 0
            , _cellXfFontId = Just 0
            , _cellXfNumFmtId = Just 0
            , _cellXfPivotButton = Nothing
            , _cellXfQuotePrefix = Nothing
            , _cellXfId = Just 0
            , _cellXfAlignment = Just
                ( Alignment
                    { _alignmentHorizontal = Just CellHorizontalAlignmentCenter
                    , _alignmentIndent = Nothing
                    , _alignmentJustifyLastLine = Nothing
                    , _alignmentReadingOrder = Nothing
                    , _alignmentRelativeIndent = Nothing
                    , _alignmentShrinkToFit = Nothing
                    , _alignmentTextRotation = Nothing
                    , _alignmentVertical = Nothing
                    , _alignmentWrapText = Nothing
                    }
                )
            , _cellXfProtection = Nothing
            }
        , CellXf
            { _cellXfApplyAlignment = Just True
            , _cellXfApplyBorder = Just True
            , _cellXfApplyFill = Just True
            , _cellXfApplyFont = Nothing
            , _cellXfApplyNumberFormat = Just True
            , _cellXfApplyProtection = Nothing
            , _cellXfBorderId = Just 0
            , _cellXfFillId = Just 2
            , _cellXfFontId = Just 0
            , _cellXfNumFmtId = Just 0
            , _cellXfPivotButton = Nothing
            , _cellXfQuotePrefix = Nothing
            , _cellXfId = Just 0
            , _cellXfAlignment = Just
                ( Alignment
                    { _alignmentHorizontal = Just CellHorizontalAlignmentCenter
                    , _alignmentIndent = Nothing
                    , _alignmentJustifyLastLine = Nothing
                    , _alignmentReadingOrder = Nothing
                    , _alignmentRelativeIndent = Nothing
                    , _alignmentShrinkToFit = Nothing
                    , _alignmentTextRotation = Nothing
                    , _alignmentVertical = Nothing
                    , _alignmentWrapText = Nothing
                    }
                )
            , _cellXfProtection = Nothing
            }
        , CellXf
            { _cellXfApplyAlignment = Just True
            , _cellXfApplyBorder = Just True
            , _cellXfApplyFill = Just True
            , _cellXfApplyFont = Nothing
            , _cellXfApplyNumberFormat = Just True
            , _cellXfApplyProtection = Nothing
            , _cellXfBorderId = Just 0
            , _cellXfFillId = Just 3
            , _cellXfFontId = Just 0
            , _cellXfNumFmtId = Just 0
            , _cellXfPivotButton = Nothing
            , _cellXfQuotePrefix = Nothing
            , _cellXfId = Just 0
            , _cellXfAlignment = Just
                ( Alignment
                    { _alignmentHorizontal = Just CellHorizontalAlignmentCenter
                    , _alignmentIndent = Nothing
                    , _alignmentJustifyLastLine = Nothing
                    , _alignmentReadingOrder = Nothing
                    , _alignmentRelativeIndent = Nothing
                    , _alignmentShrinkToFit = Nothing
                    , _alignmentTextRotation = Nothing
                    , _alignmentVertical = Nothing
                    , _alignmentWrapText = Nothing
                    }
                )
            , _cellXfProtection = Nothing
            }
        , CellXf
            { _cellXfApplyAlignment = Just True
            , _cellXfApplyBorder = Just True
            , _cellXfApplyFill = Just True
            , _cellXfApplyFont = Nothing
            , _cellXfApplyNumberFormat = Just True
            , _cellXfApplyProtection = Nothing
            , _cellXfBorderId = Just 0
            , _cellXfFillId = Just 4
            , _cellXfFontId = Just 0
            , _cellXfNumFmtId = Just 0
            , _cellXfPivotButton = Nothing
            , _cellXfQuotePrefix = Nothing
            , _cellXfId = Just 0
            , _cellXfAlignment = Just
                ( Alignment
                    { _alignmentHorizontal = Just CellHorizontalAlignmentCenter
                    , _alignmentIndent = Nothing
                    , _alignmentJustifyLastLine = Nothing
                    , _alignmentReadingOrder = Nothing
                    , _alignmentRelativeIndent = Nothing
                    , _alignmentShrinkToFit = Nothing
                    , _alignmentTextRotation = Nothing
                    , _alignmentVertical = Nothing
                    , _alignmentWrapText = Nothing
                    }
                )
            , _cellXfProtection = Nothing
            }
        , CellXf
            { _cellXfApplyAlignment = Just True
            , _cellXfApplyBorder = Just True
            , _cellXfApplyFill = Just True
            , _cellXfApplyFont = Nothing
            , _cellXfApplyNumberFormat = Just True
            , _cellXfApplyProtection = Nothing
            , _cellXfBorderId = Just 0
            , _cellXfFillId = Just 5
            , _cellXfFontId = Just 0
            , _cellXfNumFmtId = Just 0
            , _cellXfPivotButton = Nothing
            , _cellXfQuotePrefix = Nothing
            , _cellXfId = Just 0
            , _cellXfAlignment = Just
                ( Alignment
                    { _alignmentHorizontal = Just CellHorizontalAlignmentCenter
                    , _alignmentIndent = Nothing
                    , _alignmentJustifyLastLine = Nothing
                    , _alignmentReadingOrder = Nothing
                    , _alignmentRelativeIndent = Nothing
                    , _alignmentShrinkToFit = Nothing
                    , _alignmentTextRotation = Nothing
                    , _alignmentVertical = Nothing
                    , _alignmentWrapText = Nothing
                    }
                )
            , _cellXfProtection = Nothing
            }
        , CellXf
            { _cellXfApplyAlignment = Just True
            , _cellXfApplyBorder = Just True
            , _cellXfApplyFill = Just True
            , _cellXfApplyFont = Nothing
            , _cellXfApplyNumberFormat = Just True
            , _cellXfApplyProtection = Nothing
            , _cellXfBorderId = Just 0
            , _cellXfFillId = Just 6
            , _cellXfFontId = Just 0
            , _cellXfNumFmtId = Just 0
            , _cellXfPivotButton = Nothing
            , _cellXfQuotePrefix = Nothing
            , _cellXfId = Just 0
            , _cellXfAlignment = Just
                ( Alignment
                    { _alignmentHorizontal = Just CellHorizontalAlignmentCenter
                    , _alignmentIndent = Nothing
                    , _alignmentJustifyLastLine = Nothing
                    , _alignmentReadingOrder = Nothing
                    , _alignmentRelativeIndent = Nothing
                    , _alignmentShrinkToFit = Nothing
                    , _alignmentTextRotation = Nothing
                    , _alignmentVertical = Nothing
                    , _alignmentWrapText = Nothing
                    }
                )
            , _cellXfProtection = Nothing
            }
        , CellXf
            { _cellXfApplyAlignment = Just True
            , _cellXfApplyBorder = Just True
            , _cellXfApplyFill = Just True
            , _cellXfApplyFont = Nothing
            , _cellXfApplyNumberFormat = Just True
            , _cellXfApplyProtection = Nothing
            , _cellXfBorderId = Just 0
            , _cellXfFillId = Just 7
            , _cellXfFontId = Just 0
            , _cellXfNumFmtId = Just 0
            , _cellXfPivotButton = Nothing
            , _cellXfQuotePrefix = Nothing
            , _cellXfId = Just 0
            , _cellXfAlignment = Just
                ( Alignment
                    { _alignmentHorizontal = Just CellHorizontalAlignmentCenter
                    , _alignmentIndent = Nothing
                    , _alignmentJustifyLastLine = Nothing
                    , _alignmentReadingOrder = Nothing
                    , _alignmentRelativeIndent = Nothing
                    , _alignmentShrinkToFit = Nothing
                    , _alignmentTextRotation = Nothing
                    , _alignmentVertical = Nothing
                    , _alignmentWrapText = Nothing
                    }
                )
            , _cellXfProtection = Nothing
            }  
        ]
    , _styleSheetFills =
        [ Fill
            { _fillPattern = Just
                ( FillPattern
                    { _fillPatternBgColor = Nothing
                    , _fillPatternFgColor = Nothing
                    , _fillPatternType = Just PatternTypeNone
                    }
                )
            }
        , Fill
            { _fillPattern = Just
                ( FillPattern
                    { _fillPatternBgColor = Nothing
                    , _fillPatternFgColor = Nothing
                    , _fillPatternType = Just PatternTypeGray125
                    }
                )
            }
        , Fill
            { _fillPattern = Just
                ( FillPattern
                    { _fillPatternBgColor = Just
                        ( Color
                            { _colorAutomatic = Nothing
                            , _colorARGB = Nothing
                            , _colorTheme = Nothing
                            , _colorTint = Nothing
                            }
                        )
                    , _fillPatternFgColor = Just
                        ( Color
                            { _colorAutomatic = Nothing
                            , _colorARGB = Just (DText.pack "FF00FF00")
                            , _colorTheme = Nothing
                            , _colorTint = Nothing
                            }
                        )
                    , _fillPatternType = Just PatternTypeSolid
                    }
                )
            }
        , Fill
            { _fillPattern = Just
                ( FillPattern
                    { _fillPatternBgColor = Just
                        ( Color
                            { _colorAutomatic = Nothing
                            , _colorARGB = Nothing
                            , _colorTheme = Nothing
                            , _colorTint = Nothing
                            }
                        )
                    , _fillPatternFgColor = Just
                        ( Color
                            { _colorAutomatic = Nothing
                            , _colorARGB = Just (DText.pack "FFC0C0C0")
                            , _colorTheme = Nothing
                            , _colorTint = Nothing
                            }
                        )
                    , _fillPatternType = Just PatternTypeSolid
                    }
                )
            }
        , Fill
            { _fillPattern = Just
                ( FillPattern
                    { _fillPatternBgColor = Just
                        ( Color
                            { _colorAutomatic = Nothing
                            , _colorARGB = Nothing
                            , _colorTheme = Nothing
                            , _colorTint = Nothing
                            }
                        )
                    , _fillPatternFgColor = Just
                        ( Color
                            { _colorAutomatic = Nothing
                            , _colorARGB = Just (DText.pack "FFFF0000")
                            , _colorTheme = Nothing
                            , _colorTint = Nothing
                            }
                        )
                    , _fillPatternType = Just PatternTypeSolid
                    }
                )
            }
        , Fill
            { _fillPattern = Just
                ( FillPattern
                    { _fillPatternBgColor = Just
                        ( Color
                            { _colorAutomatic = Nothing
                            , _colorARGB = Nothing
                            , _colorTheme = Nothing
                            , _colorTint = Nothing
                            }
                        )
                    , _fillPatternFgColor = Just
                        ( Color
                            { _colorAutomatic = Nothing
                            , _colorARGB = Just (DText.pack "FF00FF00")
                            , _colorTheme = Nothing
                            , _colorTint = Nothing
                            }
                        )
                    , _fillPatternType = Just PatternTypeSolid
                    }
                )
            }
        , Fill
            { _fillPattern = Just
                ( FillPattern
                    { _fillPatternBgColor = Just
                        ( Color
                            { _colorAutomatic = Nothing
                            , _colorARGB = Nothing
                            , _colorTheme = Nothing
                            , _colorTint = Nothing
                            }
                        )
                    , _fillPatternFgColor = Just
                        ( Color
                            { _colorAutomatic = Nothing
                            , _colorARGB = Just (DText.pack "FFFFFF33")
                            , _colorTheme = Nothing
                            , _colorTint = Nothing
                            }
                        )
                    , _fillPatternType = Just PatternTypeSolid
                    }
                )
            }
        , Fill
            { _fillPattern = Just
                ( FillPattern
                    { _fillPatternBgColor = Just
                        ( Color
                            { _colorAutomatic = Nothing
                            , _colorARGB = Nothing
                            , _colorTheme = Nothing
                            , _colorTint = Nothing
                            }
                        )
                    , _fillPatternFgColor = Just
                        ( Color
                            { _colorAutomatic = Nothing
                            , _colorARGB = Just (DText.pack "FFFF0000")
                            , _colorTheme = Nothing
                            , _colorTint = Nothing
                            }
                        )
                    , _fillPatternType = Just PatternTypeSolid
                    }
                )
            }
        ]
    , _styleSheetFonts =
        [ Font
            { _fontBold = Nothing
            , _fontCharset = Nothing
            , _fontColor = Just
                ( Color
                    { _colorAutomatic = Nothing
                    , _colorARGB = Nothing
                    , _colorTheme = Just 1
                    , _colorTint = Nothing
                    }
                )
            , _fontCondense = Nothing
            , _fontExtend = Nothing
            , _fontFamily = Just FontFamilySwiss
            , _fontItalic = Nothing
            , _fontName = Just (DText.pack "Calibri")
            , _fontOutline = Nothing
            , _fontScheme = Just FontSchemeMinor
            , _fontShadow = Nothing
            , _fontStrikeThrough = Nothing
            , _fontSize = Just 12.0
            , _fontUnderline = Nothing
            , _fontVertAlign = Nothing
            }
        ]
    , _styleSheetDxfs = []
    , _styleSheetNumFmts = fromList []
    }

--Defines the vaccine trials StyleSheet.
vaccinestylesheet :: StyleSheet
vaccinestylesheet = StyleSheet
    { _styleSheetBorders =
        [ Border
            { _borderDiagonalDown = Nothing
            , _borderDiagonalUp = Nothing
            , _borderOutline = Nothing
            , _borderBottom = Just
                ( BorderStyle
                    { _borderStyleColor = Nothing
                    , _borderStyleLine = Nothing
                    }
                )
            , _borderDiagonal = Just
                ( BorderStyle
                    { _borderStyleColor = Nothing
                    , _borderStyleLine = Nothing
                    }
                )
            , _borderEnd = Nothing
            , _borderHorizontal = Nothing
            , _borderLeft = Just
                ( BorderStyle
                    { _borderStyleColor = Nothing
                    , _borderStyleLine = Nothing
                    }
                )
            , _borderRight = Just
                ( BorderStyle
                    { _borderStyleColor = Nothing
                    , _borderStyleLine = Nothing
                    }
                )
            , _borderStart = Nothing
            , _borderTop = Just
                ( BorderStyle
                    { _borderStyleColor = Nothing
                    , _borderStyleLine = Nothing
                    }
                )
            , _borderVertical = Nothing
            }
        , Border
            { _borderDiagonalDown = Nothing
            , _borderDiagonalUp = Nothing
            , _borderOutline = Just False
            , _borderBottom = Just
                ( BorderStyle
                    { _borderStyleColor = Just
                        ( Color
                            { _colorAutomatic = Nothing
                            , _colorARGB = Just (DText.pack "FF474747")
                            , _colorTheme = Nothing
                            , _colorTint = Nothing
                            }
                        )
                    , _borderStyleLine = Just LineStyleThin
                    }
                )
            , _borderDiagonal = Just
                ( BorderStyle
                    { _borderStyleColor = Nothing
                    , _borderStyleLine = Nothing
                    }
                )
            , _borderEnd = Just
                ( BorderStyle
                    { _borderStyleColor = Just
                        ( Color
                            { _colorAutomatic = Nothing
                            , _colorARGB = Just (DText.pack "FF474747")
                            , _colorTheme = Nothing
                            , _colorTint = Nothing
                            }
                        )
                    , _borderStyleLine = Just LineStyleThin
                    }
                )
            , _borderHorizontal = Nothing
            , _borderLeft = Just
                ( BorderStyle
                    { _borderStyleColor = Just
                        ( Color
                            { _colorAutomatic = Nothing
                            , _colorARGB = Just (DText.pack "FF474747")
                            , _colorTheme = Nothing
                            , _colorTint = Nothing
                            }
                        )
                    , _borderStyleLine = Just LineStyleThin
                    }
                )
            , _borderRight = Just
                ( BorderStyle
                    { _borderStyleColor = Just
                        ( Color
                            { _colorAutomatic = Nothing
                            , _colorARGB = Just (DText.pack "FF474747")
                            , _colorTheme = Nothing
                            , _colorTint = Nothing
                            }
                        )
                    , _borderStyleLine = Just LineStyleThin
                    }
                )
            , _borderStart = Just
                ( BorderStyle
                    { _borderStyleColor = Just
                        ( Color
                            { _colorAutomatic = Nothing
                            , _colorARGB = Just (DText.pack "FF474747")
                            , _colorTheme = Nothing
                            , _colorTint = Nothing
                            }
                        )
                    , _borderStyleLine = Just LineStyleThin
                    }
                )
            , _borderTop = Just
                ( BorderStyle
                    { _borderStyleColor = Just
                        ( Color
                            { _colorAutomatic = Nothing
                            , _colorARGB = Just (DText.pack "FF474747")
                            , _colorTheme = Nothing
                            , _colorTint = Nothing
                            }
                        )
                    , _borderStyleLine = Just LineStyleThin
                    }
                )
            , _borderVertical = Nothing
            }
        ]
    , _styleSheetCellXfs =
        [ CellXf
            { _cellXfApplyAlignment = Nothing
            , _cellXfApplyBorder = Just True
            , _cellXfApplyFill = Nothing
            , _cellXfApplyFont = Nothing
            , _cellXfApplyNumberFormat = Nothing
            , _cellXfApplyProtection = Nothing
            , _cellXfBorderId = Just 1
            , _cellXfFillId = Just 0
            , _cellXfFontId = Just 0
            , _cellXfNumFmtId = Nothing
            , _cellXfPivotButton = Nothing
            , _cellXfQuotePrefix = Nothing
            , _cellXfId = Just 0
            , _cellXfAlignment = Nothing
            , _cellXfProtection = Nothing
            }
        , CellXf
            { _cellXfApplyAlignment = Just True
            , _cellXfApplyBorder = Just True
            , _cellXfApplyFill = Nothing
            , _cellXfApplyFont = Nothing
            , _cellXfApplyNumberFormat = Nothing
            , _cellXfApplyProtection = Nothing
            , _cellXfBorderId = Just 1
            , _cellXfFillId = Just 0
            , _cellXfFontId = Just 0
            , _cellXfNumFmtId = Nothing
            , _cellXfPivotButton = Nothing
            , _cellXfQuotePrefix = Nothing
            , _cellXfId = Just 0
            , _cellXfAlignment = Just
                ( Alignment
                    { _alignmentHorizontal = Just CellHorizontalAlignmentCenter
                    , _alignmentIndent = Nothing
                    , _alignmentJustifyLastLine = Nothing
                    , _alignmentReadingOrder = Nothing
                    , _alignmentRelativeIndent = Nothing
                    , _alignmentShrinkToFit = Nothing
                    , _alignmentTextRotation = Just 90
                    , _alignmentVertical = Nothing
                    , _alignmentWrapText = Nothing
                    }
                )
            , _cellXfProtection = Nothing
            }
        , CellXf
            { _cellXfApplyAlignment = Just True
            , _cellXfApplyBorder = Just True
            , _cellXfApplyFill = Just True
            , _cellXfApplyFont = Just True
            , _cellXfApplyNumberFormat = Just True
            , _cellXfApplyProtection = Nothing
            , _cellXfBorderId = Just 1
            , _cellXfFillId = Just 0
            , _cellXfFontId = Just 0
            , _cellXfNumFmtId = Nothing
            , _cellXfPivotButton = Nothing
            , _cellXfQuotePrefix = Nothing
            , _cellXfId = Just 0
            , _cellXfAlignment = Just
                ( Alignment
                    { _alignmentHorizontal = Just CellHorizontalAlignmentCenter
                    , _alignmentIndent = Nothing
                    , _alignmentJustifyLastLine = Nothing
                    , _alignmentReadingOrder = Nothing
                    , _alignmentRelativeIndent = Nothing
                    , _alignmentShrinkToFit = Nothing
                    , _alignmentTextRotation = Just 45
                    , _alignmentVertical = Nothing
                    , _alignmentWrapText = Nothing
                    }
                )
            , _cellXfProtection = Nothing
            }
        , CellXf
            { _cellXfApplyAlignment = Just True
            , _cellXfApplyBorder = Just True
            , _cellXfApplyFill = Just True
            , _cellXfApplyFont = Just True
            , _cellXfApplyNumberFormat = Just True
            , _cellXfApplyProtection = Nothing
            , _cellXfBorderId = Just 1
            , _cellXfFillId = Just 0
            , _cellXfFontId = Just 1
            , _cellXfNumFmtId = Nothing
            , _cellXfPivotButton = Nothing
            , _cellXfQuotePrefix = Nothing
            , _cellXfId = Just 0
            , _cellXfAlignment = Just
                ( Alignment
                    { _alignmentHorizontal = Just CellHorizontalAlignmentCenter
                    , _alignmentIndent = Nothing
                    , _alignmentJustifyLastLine = Nothing
                    , _alignmentReadingOrder = Nothing
                    , _alignmentRelativeIndent = Nothing
                    , _alignmentShrinkToFit = Nothing
                    , _alignmentTextRotation = Nothing
                    , _alignmentVertical = Nothing
                    , _alignmentWrapText = Nothing
                    }
                )
            , _cellXfProtection = Nothing
            }
        , CellXf
            { _cellXfApplyAlignment = Just True
            , _cellXfApplyBorder = Just True
            , _cellXfApplyFill = Just True
            , _cellXfApplyFont = Just True
            , _cellXfApplyNumberFormat = Just True
            , _cellXfApplyProtection = Nothing
            , _cellXfBorderId = Just 1
            , _cellXfFillId = Just 2
            , _cellXfFontId = Just 1
            , _cellXfNumFmtId = Nothing
            , _cellXfPivotButton = Nothing
            , _cellXfQuotePrefix = Nothing
            , _cellXfId = Just 0
            , _cellXfAlignment = Just
                ( Alignment
                    { _alignmentHorizontal = Just CellHorizontalAlignmentCenter
                    , _alignmentIndent = Nothing
                    , _alignmentJustifyLastLine = Nothing
                    , _alignmentReadingOrder = Nothing
                    , _alignmentRelativeIndent = Nothing
                    , _alignmentShrinkToFit = Nothing
                    , _alignmentTextRotation = Nothing
                    , _alignmentVertical = Nothing
                    , _alignmentWrapText = Nothing
                    }
                )
            , _cellXfProtection = Nothing
            }
        , CellXf
            { _cellXfApplyAlignment = Just True
            , _cellXfApplyBorder = Just True
            , _cellXfApplyFill = Just True
            , _cellXfApplyFont = Just True
            , _cellXfApplyNumberFormat = Just True
            , _cellXfApplyProtection = Nothing
            , _cellXfBorderId = Just 1
            , _cellXfFillId = Just 3
            , _cellXfFontId = Just 1
            , _cellXfNumFmtId = Nothing
            , _cellXfPivotButton = Nothing
            , _cellXfQuotePrefix = Nothing
            , _cellXfId = Just 0
            , _cellXfAlignment = Just
                ( Alignment
                    { _alignmentHorizontal = Just CellHorizontalAlignmentCenter
                    , _alignmentIndent = Nothing
                    , _alignmentJustifyLastLine = Nothing
                    , _alignmentReadingOrder = Nothing
                    , _alignmentRelativeIndent = Nothing
                    , _alignmentShrinkToFit = Nothing
                    , _alignmentTextRotation = Nothing
                    , _alignmentVertical = Nothing
                    , _alignmentWrapText = Nothing
                    }
                )
            , _cellXfProtection = Nothing
            }
        , CellXf
            { _cellXfApplyAlignment = Just True
            , _cellXfApplyBorder = Just True
            , _cellXfApplyFill = Just True
            , _cellXfApplyFont = Nothing
            , _cellXfApplyNumberFormat = Nothing
            , _cellXfApplyProtection = Nothing
            , _cellXfBorderId = Just 1
            , _cellXfFillId = Just 4
            , _cellXfFontId = Just 1
            , _cellXfNumFmtId = Nothing
            , _cellXfPivotButton = Nothing
            , _cellXfQuotePrefix = Nothing
            , _cellXfId = Just 0
            , _cellXfAlignment = Just
                ( Alignment
                    { _alignmentHorizontal = Just CellHorizontalAlignmentCenter
                    , _alignmentIndent = Nothing
                    , _alignmentJustifyLastLine = Nothing
                    , _alignmentReadingOrder = Nothing
                    , _alignmentRelativeIndent = Nothing
                    , _alignmentShrinkToFit = Nothing
                    , _alignmentTextRotation = Nothing
                    , _alignmentVertical = Nothing
                    , _alignmentWrapText = Nothing
                    }
                )
            , _cellXfProtection = Nothing
            }
        , CellXf
            { _cellXfApplyAlignment = Just True
            , _cellXfApplyBorder = Just True
            , _cellXfApplyFill = Just True
            , _cellXfApplyFont = Just True
            , _cellXfApplyNumberFormat = Just True
            , _cellXfApplyProtection = Nothing
            , _cellXfBorderId = Just 1
            , _cellXfFillId = Just 5
            , _cellXfFontId = Just 1
            , _cellXfNumFmtId = Nothing
            , _cellXfPivotButton = Nothing
            , _cellXfQuotePrefix = Nothing
            , _cellXfId = Just 0
            , _cellXfAlignment = Just
                ( Alignment
                    { _alignmentHorizontal = Just CellHorizontalAlignmentCenter
                    , _alignmentIndent = Nothing
                    , _alignmentJustifyLastLine = Nothing
                    , _alignmentReadingOrder = Nothing
                    , _alignmentRelativeIndent = Nothing
                    , _alignmentShrinkToFit = Nothing
                    , _alignmentTextRotation = Nothing
                    , _alignmentVertical = Nothing
                    , _alignmentWrapText = Nothing
                    }
                )
            , _cellXfProtection = Nothing
            }
        , CellXf
            { _cellXfApplyAlignment = Just True
            , _cellXfApplyBorder = Just True
            , _cellXfApplyFill = Just True
            , _cellXfApplyFont = Just True
            , _cellXfApplyNumberFormat = Just True
            , _cellXfApplyProtection = Nothing
            , _cellXfBorderId = Just 1
            , _cellXfFillId = Just 6
            , _cellXfFontId = Just 1
            , _cellXfNumFmtId = Nothing
            , _cellXfPivotButton = Nothing
            , _cellXfQuotePrefix = Nothing
            , _cellXfId = Just 0
            , _cellXfAlignment = Just
                ( Alignment
                    { _alignmentHorizontal = Just CellHorizontalAlignmentCenter
                    , _alignmentIndent = Nothing
                    , _alignmentJustifyLastLine = Nothing
                    , _alignmentReadingOrder = Nothing
                    , _alignmentRelativeIndent = Nothing
                    , _alignmentShrinkToFit = Nothing
                    , _alignmentTextRotation = Nothing
                    , _alignmentVertical = Nothing
                    , _alignmentWrapText = Nothing
                    }
                )
            , _cellXfProtection = Nothing
            }
        , CellXf
            { _cellXfApplyAlignment = Just True
            , _cellXfApplyBorder = Just True
            , _cellXfApplyFill = Just True
            , _cellXfApplyFont = Just True
            , _cellXfApplyNumberFormat = Just True
            , _cellXfApplyProtection = Nothing
            , _cellXfBorderId = Just 1
            , _cellXfFillId = Just 7
            , _cellXfFontId = Just 1
            , _cellXfNumFmtId = Nothing
            , _cellXfPivotButton = Nothing
            , _cellXfQuotePrefix = Nothing
            , _cellXfId = Just 0
            , _cellXfAlignment = Just
                ( Alignment
                    { _alignmentHorizontal = Just CellHorizontalAlignmentCenter
                    , _alignmentIndent = Nothing
                    , _alignmentJustifyLastLine = Nothing
                    , _alignmentReadingOrder = Nothing
                    , _alignmentRelativeIndent = Nothing
                    , _alignmentShrinkToFit = Nothing
                    , _alignmentTextRotation = Nothing
                    , _alignmentVertical = Nothing
                    , _alignmentWrapText = Nothing
                    }
                )
            , _cellXfProtection = Nothing
            }
        ]
    , _styleSheetFills =
        [ Fill
            { _fillPattern = Just
                ( FillPattern
                    { _fillPatternBgColor = Nothing
                    , _fillPatternFgColor = Nothing
                    , _fillPatternType = Just PatternTypeNone
                    }
                )
            }
        , Fill
            { _fillPattern = Just
                ( FillPattern
                    { _fillPatternBgColor = Nothing
                    , _fillPatternFgColor = Nothing
                    , _fillPatternType = Just PatternTypeGray125
                    }
                )
            }
        , Fill
            { _fillPattern = Just
                ( FillPattern
                    { _fillPatternBgColor = Just
                        ( Color
                            { _colorAutomatic = Nothing
                            , _colorARGB = Nothing
                            , _colorTheme = Nothing
                            , _colorTint = Nothing
                            }
                        )
                    , _fillPatternFgColor = Just
                        ( Color
                            { _colorAutomatic = Nothing
                            , _colorARGB = Just (DText.pack "FF00FF00")
                            , _colorTheme = Nothing
                            , _colorTint = Nothing
                            }
                        )
                    , _fillPatternType = Just PatternTypeSolid
                    }
                )
            }
        , Fill
            { _fillPattern = Just
                ( FillPattern
                    { _fillPatternBgColor = Just
                        ( Color
                            { _colorAutomatic = Nothing
                            , _colorARGB = Nothing
                            , _colorTheme = Nothing
                            , _colorTint = Nothing
                            }
                        )
                    , _fillPatternFgColor = Just
                        ( Color
                            { _colorAutomatic = Nothing
                            , _colorARGB = Just (DText.pack "FFC0C0C0")
                            , _colorTheme = Nothing
                            , _colorTint = Nothing
                            }
                        )
                    , _fillPatternType = Just PatternTypeSolid
                    }
                )
            }
        , Fill
            { _fillPattern = Just
                ( FillPattern
                    { _fillPatternBgColor = Just
                        ( Color
                            { _colorAutomatic = Nothing
                            , _colorARGB = Nothing
                            , _colorTheme = Nothing
                            , _colorTint = Nothing
                            }
                        )
                    , _fillPatternFgColor = Just
                        ( Color
                            { _colorAutomatic = Nothing
                            , _colorARGB = Just (DText.pack "FFFF0000")
                            , _colorTheme = Nothing
                            , _colorTint = Nothing
                            }
                        )
                    , _fillPatternType = Just PatternTypeSolid
                    }
                )
            }
        , Fill
            { _fillPattern = Just
                ( FillPattern
                    { _fillPatternBgColor = Just
                        ( Color
                            { _colorAutomatic = Nothing
                            , _colorARGB = Nothing
                            , _colorTheme = Nothing
                            , _colorTint = Nothing
                            }
                        )
                    , _fillPatternFgColor = Just
                        ( Color
                            { _colorAutomatic = Nothing
                            , _colorARGB = Just (DText.pack "FF00FF00")
                            , _colorTheme = Nothing
                            , _colorTint = Nothing
                            }
                        )
                    , _fillPatternType = Just PatternTypeSolid
                    }
                )
            }
        , Fill
            { _fillPattern = Just
                ( FillPattern
                    { _fillPatternBgColor = Just
                        ( Color
                            { _colorAutomatic = Nothing
                            , _colorARGB = Nothing
                            , _colorTheme = Nothing
                            , _colorTint = Nothing
                            }
                        )
                    , _fillPatternFgColor = Just
                        ( Color
                            { _colorAutomatic = Nothing
                            , _colorARGB = Just (DText.pack "FFFFFF33")
                            , _colorTheme = Nothing
                            , _colorTint = Nothing
                            }
                        )
                    , _fillPatternType = Just PatternTypeSolid
                    }
                )
            }
        , Fill
            { _fillPattern = Just
                ( FillPattern
                    { _fillPatternBgColor = Just
                        ( Color
                            { _colorAutomatic = Nothing
                            , _colorARGB = Nothing
                            , _colorTheme = Nothing
                            , _colorTint = Nothing
                            }
                        )
                    , _fillPatternFgColor = Just
                        ( Color
                            { _colorAutomatic = Nothing
                            , _colorARGB = Just (DText.pack "FFFF0000")
                            , _colorTheme = Nothing
                            , _colorTint = Nothing
                            }
                        )
                    , _fillPatternType = Just PatternTypeSolid
                    }
                )
            }
        ]
    , _styleSheetFonts =
        [ Font
            { _fontBold = Just True
            , _fontCharset = Nothing
            , _fontColor = Just
                ( Color
                    { _colorAutomatic = Nothing
                    , _colorARGB = Nothing
                    , _colorTheme = Just 1
                    , _colorTint = Nothing
                    }
                )
            , _fontCondense = Nothing
            , _fontExtend = Nothing
            , _fontFamily = Just FontFamilySwiss
            , _fontItalic = Nothing
            , _fontName = Just (DText.pack "Calibri")
            , _fontOutline = Nothing
            , _fontScheme = Just FontSchemeMinor
            , _fontShadow = Nothing
            , _fontStrikeThrough = Nothing
            , _fontSize = Just 12.0
            , _fontUnderline = Nothing
            , _fontVertAlign = Nothing
            }
        , Font
            { _fontBold = Nothing
            , _fontCharset = Nothing
            , _fontColor = Just
                ( Color
                    { _colorAutomatic = Nothing
                    , _colorARGB = Nothing
                    , _colorTheme = Just 1
                    , _colorTint = Nothing
                    }
                )
            , _fontCondense = Nothing
            , _fontExtend = Nothing
            , _fontFamily = Just FontFamilySwiss
            , _fontItalic = Nothing
            , _fontName = Just (DText.pack "Calibri")
            , _fontOutline = Nothing
            , _fontScheme = Just FontSchemeMinor
            , _fontShadow = Nothing
            , _fontStrikeThrough = Nothing
            , _fontSize = Just 12.0
            , _fontUnderline = Nothing
            , _fontVertAlign = Nothing
            }
        ]
    , _styleSheetDxfs = []
    , _styleSheetNumFmts = fromList []
    }

--Defines default _xlDefinedNames data type value.
defaultxldefinednames :: DefinedNames
defaultxldefinednames = DefinedNames []

--Defines default _xlCustomProperties data type value.
defaultxlcustomproperties :: Map Text Variant
defaultxlcustomproperties = (fromList []) ::  Map Text Variant

--Defines default _xlDateBase data type value.
defaultxldatebase :: DateBase
defaultxldatebase = DateBase1900 

{-------------------------}
