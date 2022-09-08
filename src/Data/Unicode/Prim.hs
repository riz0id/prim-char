{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Data.Unicode.Prim
-- Copyright   :  (c) Jacob Leach, 2022
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- TODO
--
-- @since 1.0.0
module Data.Unicode.Prim
  ( -- * GeneralCategory# 
    GeneralCategory#
      ( UppercaseLetter#,
        LowercaseLetter#,
        TitlecaseLetter#,
        NonSpacingMark#,
        OtherLetter#,
        ModifierLetter#,
        PrivateUse#,
        Surrogate#,
        Format#,
        Control#,
        ParagraphSeparator#,
        LineSeparator#,
        Space#,
        OtherSymbol#,
        ModifierSymbol#,
        CurrencySymbol#,
        MathSymbol#,
        OtherPunctuation#,
        FinalQuote#,
        InitialQuote#,
        OpenPunctuation#,
        DashPunctuation#,
        ConnectorPunctuation#,
        OtherNumber#,
        ClosePunctuation#,
        DecimalNumber#,
        LetterNumber#,
        EnclosingMark#,
        SpacingCombiningMark#,
        NotAssigned#
      ),
    generalCategory#,

    -- * Conversion 
    fromGeneralCategory,
    toGeneralCategory,
    fromInt#,
    toInt#,
  )
where

import Data.Bool.Prim (Bool# (False#, True#))
import Data.Bool.Prim qualified as Bool
import Data.Int.Prim (Int#)
import Data.Int.Prim qualified as Int
import Data.Coerce (coerce)
import GHC.Exts (Char#)
import Data.Char (GeneralCategory)
import qualified GHC.Base as GHC

-- GeneralCategory# ------------------------------------------------------------

-- | 'GeneralCategory#' is an unboxed enumeration of the Unicode general 
-- categories.
--
-- @since 1.0.0
newtype GeneralCategory# = GeneralCategory# Int#

-- | The unboxed analog to the 'Data.Char.UppercaseLetter' constructor.
--
-- @'toGeneralCategory' 'UppercaseLetter#' == 'Data.Char.UppercaseLetter'@
pattern UppercaseLetter# :: GeneralCategory#
pattern UppercaseLetter# = GeneralCategory# 0#

-- | The unboxed analog to the 'Data.Char.LowercaseLetter' constructor.
--
-- @'toGeneralCategory' 'LowercaseLetter#' == 'Data.Char.LowercaseLetter'@
pattern LowercaseLetter# :: GeneralCategory#
pattern LowercaseLetter# = GeneralCategory# 1#

-- | The unboxed analog to the 'Data.Char.TitlecaseLetter' constructor.
--
-- @'toGeneralCategory' 'TitlecaseLetter#' == 'Data.Char.TitlecaseLetter'@
pattern TitlecaseLetter# :: GeneralCategory#
pattern TitlecaseLetter# = GeneralCategory# 2#

-- | The unboxed analog to the 'Data.Char.ModifierLetter' constructor.
--
-- @'toGeneralCategory' 'ModifierLetter#' == 'Data.Char.ModifierLetter'@
pattern ModifierLetter# :: GeneralCategory#
pattern ModifierLetter# = GeneralCategory# 3#

-- | The unboxed analog to the 'Data.Char.OtherLetter' constructor.
--
-- @'toGeneralCategory' 'OtherLetter#' == 'Data.Char.OtherLetter'@
pattern OtherLetter# :: GeneralCategory#
pattern OtherLetter# = GeneralCategory# 4#

-- | The unboxed analog to the 'Data.Char.NonSpacingMark' constructor.
--
-- @'toGeneralCategory' 'NonSpacingMark#' == 'Data.Char.NonSpacingMark'@
pattern NonSpacingMark# :: GeneralCategory#
pattern NonSpacingMark# = GeneralCategory# 5#

-- | The unboxed analog to the 'Data.Char.SpacingCombiningMark' constructor.
--
-- @'toGeneralCategory' 'SpacingCombiningMark#' == 'Data.Char.SpacingCombiningMark'@
pattern SpacingCombiningMark# :: GeneralCategory#
pattern SpacingCombiningMark# = GeneralCategory# 6#

-- | The unboxed analog to the 'Data.Char.EnclosingMark' constructor.
--
-- @'toGeneralCategory' 'EnclosingMark#' == 'Data.Char.EnclosingMark'@
pattern EnclosingMark# :: GeneralCategory#
pattern EnclosingMark# = GeneralCategory# 7#

-- | The unboxed analog to the 'Data.Char.DecimalNumber' constructor.
--
-- @'toGeneralCategory' 'DecimalNumber#' == 'Data.Char.DecimalNumber'@
pattern DecimalNumber# :: GeneralCategory#
pattern DecimalNumber# = GeneralCategory# 8#

-- | The unboxed analog to the 'Data.Char.LetterNumber' constructor.
--
-- @'toGeneralCategory' 'LetterNumber#' == 'Data.Char.LetterNumber'@
pattern LetterNumber# :: GeneralCategory#
pattern LetterNumber# = GeneralCategory# 9#

-- | The unboxed analog to the 'Data.Char.OtherNumber' constructor.
--
-- @'toGeneralCategory' 'OtherNumber#' == 'Data.Char.OtherNumber'@
pattern OtherNumber# :: GeneralCategory#
pattern OtherNumber# = GeneralCategory# 10#

-- | The unboxed analog to the 'Data.Char.ConnectorPunctuation' constructor.
--
-- @'toGeneralCategory' 'ConnectorPunctuation#' == 'Data.Char.ConnectorPunctuation'@
pattern ConnectorPunctuation# :: GeneralCategory#
pattern ConnectorPunctuation# = GeneralCategory# 11#

-- | The unboxed analog to the 'Data.Char.DashPunctuation' constructor.
--
-- @'toGeneralCategory' 'DashPunctuation#' == 'Data.Char.DashPunctuation'@
pattern DashPunctuation# :: GeneralCategory#
pattern DashPunctuation# = GeneralCategory# 12#

-- | The unboxed analog to the 'Data.Char.OpenPunctuation' constructor.
--
-- @'toGeneralCategory' 'OpenPunctuation#' == 'Data.Char.OpenPunctuation'@
pattern OpenPunctuation# :: GeneralCategory#
pattern OpenPunctuation# = GeneralCategory# 13#

-- | The unboxed analog to the 'Data.Char.ClosePunctuation' constructor.
--
-- @'toGeneralCategory' 'ClosePunctuation#' == 'Data.Char.ClosePunctuation'@
pattern ClosePunctuation# :: GeneralCategory#
pattern ClosePunctuation# = GeneralCategory# 14#

-- | The unboxed analog to the 'Data.Char.InitialQuote' constructor.
--
-- @'toGeneralCategory' 'InitialQuote#' == 'Data.Char.InitialQuote'@
pattern InitialQuote# :: GeneralCategory#
pattern InitialQuote# = GeneralCategory# 15#

-- | The unboxed analog to the 'Data.Char.FinalQuote' constructor.
--
-- @'toGeneralCategory' 'FinalQuote#' == 'Data.Char.FinalQuote'@
pattern FinalQuote# :: GeneralCategory#
pattern FinalQuote# = GeneralCategory# 16#

-- | The unboxed analog to the 'Data.Char.OtherPunctuation' constructor.
--
-- @'toGeneralCategory' 'OtherPunctuation#' == 'Data.Char.OtherPunctuation'@
pattern OtherPunctuation# :: GeneralCategory#
pattern OtherPunctuation# = GeneralCategory# 17#

-- | The unboxed analog to the 'Data.Char.MathSymbol' constructor.
--
-- @'toGeneralCategory' 'MathSymbol#' == 'Data.Char.MathSymbol'@
pattern MathSymbol# :: GeneralCategory#
pattern MathSymbol# = GeneralCategory# 18#

-- | The unboxed analog to the 'Data.Char.CurrencySymbol' constructor.
--
-- @'toGeneralCategory' 'CurrencySymbol#' == 'Data.Char.CurrencySymbol'@
pattern CurrencySymbol# :: GeneralCategory#
pattern CurrencySymbol# = GeneralCategory# 19#

-- | The unboxed analog to the 'Data.Char.ModifierSymbol' constructor.
--
-- @'toGeneralCategory' 'ModifierSymbol#' == 'Data.Char.ModifierSymbol'@
pattern ModifierSymbol# :: GeneralCategory#
pattern ModifierSymbol# = GeneralCategory# 20#

-- | The unboxed analog to the 'Data.Char.OtherSymbol' constructor.
--
-- @'toGeneralCategory' 'OtherSymbol#' == 'Data.Char.OtherSymbol'@
pattern OtherSymbol# :: GeneralCategory#
pattern OtherSymbol# = GeneralCategory# 21#

-- | The unboxed analog to the 'Data.Char.Space' constructor.
--
-- @'toGeneralCategory' 'Space#' == 'Data.Char.Space'@
pattern Space# :: GeneralCategory#
pattern Space# = GeneralCategory# 22#

-- | The unboxed analog to the 'Data.Char.LineSeparator' constructor.
--
-- @'toGeneralCategory' 'LineSeparator#' == 'Data.Char.LineSeparator'@
pattern LineSeparator# :: GeneralCategory#
pattern LineSeparator# = GeneralCategory# 23#

-- | The unboxed analog to the 'Data.Char.ParagraphSeparator' constructor.
--
-- @'toGeneralCategory' 'ParagraphSeparator#' == 'Data.Char.ParagraphSeparator'@
pattern ParagraphSeparator# :: GeneralCategory#
pattern ParagraphSeparator# = GeneralCategory# 24#

-- | The unboxed analog to the 'Data.Char.Control' constructor.
--
-- @'toGeneralCategory' 'Control#' == 'Data.Char.Control'@
pattern Control# :: GeneralCategory#
pattern Control# = GeneralCategory# 25#

-- | The unboxed analog to the 'Data.Char.Format' constructor.
--
-- @'toGeneralCategory' 'Format#' == 'Data.Char.Format'@
pattern Format# :: GeneralCategory#
pattern Format# = GeneralCategory# 26#

-- | The unboxed analog to the 'Data.Char.Surrogate' constructor.
--
-- @'toGeneralCategory' 'Surrogate#' == 'Data.Char.Surrogate'@
pattern Surrogate# :: GeneralCategory#
pattern Surrogate# = GeneralCategory# 27#

-- | The unboxed analog to the 'Data.Char.PrivateUse' constructor.
--
-- @'toGeneralCategory' 'PrivateUse#' == 'Data.Char.PrivateUse'@
pattern PrivateUse# :: GeneralCategory#
pattern PrivateUse# = GeneralCategory# 28#

-- | The unboxed analog to the 'Data.Char.NotAssigned' constructor. 
--
-- @'toGeneralCategory' 'NotAssigned#' == 'Data.Char.NotAssigned'@
pattern NotAssigned# :: GeneralCategory#
pattern NotAssigned# <-
  ((\_ -> GeneralCategory# 29#) -> GeneralCategory# 29#)
  where
    NotAssigned# = GeneralCategory# 29#

{-# COMPLETE
  UppercaseLetter#
  , LowercaseLetter#
  , TitlecaseLetter#
  , NonSpacingMark#
  , OtherLetter#
  , ModifierLetter#
  , NotAssigned#
  , PrivateUse#
  , Surrogate#
  , Format#
  , Control#
  , ParagraphSeparator#
  , LineSeparator#
  , Space#
  , OtherSymbol#
  , ModifierSymbol#
  , CurrencySymbol#
  , MathSymbol#
  , OtherPunctuation#
  , FinalQuote#
  , InitialQuote#
  , OpenPunctuation#
  , DashPunctuation#
  , ConnectorPunctuation#
  , OtherNumber#
  , ClosePunctuation#
  , DecimalNumber#
  , LetterNumber#
  , EnclosingMark#
  , SpacingCombiningMark#
  #-}

-- | Obtain the Unicode general category of a 'Char#' value.
--
-- @since 1.0.0
generalCategory# :: Char# -> GeneralCategory#
generalCategory# c# = GeneralCategory# (wgencat# c#)

foreign import ccall unsafe "u_gencat"
  wgencat# :: Char# -> Int#

-- Conversion ------------------------------------------------------------------

-- | Convert a boxed 'GeneralCategory' enum to an unboxed 'GeneralCategory#' 
-- value. 
fromGeneralCategory :: GeneralCategory -> GeneralCategory#
fromGeneralCategory x = GeneralCategory# (GHC.dataToTag# x)

-- | Convert an unboxed 'GeneralCategory#' enum to a boxed 'GeneralCategory' 
-- value. 
--
-- @since 1.0.0
toGeneralCategory :: GeneralCategory# -> GeneralCategory
toGeneralCategory (GeneralCategory# x#) = GHC.tagToEnum# x# 

-- | Convert an 'Int#' value to the corresponding 'GeneralCategory#' enum if
-- the 'Int#' is between 0# and 29# (inclusive).
--
-- All 'Int#' values outside of the range @[0#, 29#]@ are will return the 
-- 'NotAssigned#' enum.
--
-- @since 1.0.0
fromInt# :: Int# -> GeneralCategory#
fromInt# x# = 
  case Bool.and# (Int.geInt# 0# x#) (Int.leInt# x# 29#) of 
    True# -> GeneralCategory# x#
    False# -> NotAssigned#

-- | Convert a 'GeneralCategory#' enum to an 'Int#' value. 
--
-- @since 1.0.0
toInt# :: GeneralCategory# -> Int#
toInt# = coerce
