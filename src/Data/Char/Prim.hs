{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Data.Char.Prim
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
module Data.Char.Prim
  ( -- * Char#
    Char#,
    fromChar#,
    toChar#,

    -- * Comparison
    gt#,
    ge#,
    eq#,
    ne#,
    lt#,
    le#,

    -- * Conversion
    fromInt#,
    toInt#,

    -- * Case Conversion
    toLower#,
    toUpper#,
    toTitle#,

    -- * Classification
    isControl#,
    isSpace#,
    isLower#,
    isUpper#,
    isAlpha#,
    isAlphaNum#,
    isPrint#,
    isDigit#,
    isOctDigit#,
    isHexDigit#,

    -- ** Subranges
    isAscii#,
    isAsciiLower#,
    isAsciiUpper#,
    isLatin1#,

    -- * Show
    show#,
    shows#,
  )
where

import Data.Bool.Prim (Bool#)
import Data.Bool.Prim qualified as Bool
import Data.Char (showLitChar)
import Data.Int.Prim (Int#)

import GHC.Exts (Char (C#), Char#)
import GHC.Exts qualified as GHC

-- Char# -----------------------------------------------------------------------

{-# RULES "Char# -> Char -> Char#" forall c. toChar# (fromChar# c) = c #-}

-- | TODO
--
-- @since 1.0.0
fromChar# :: Char# -> Char
fromChar# = C#
{-# INLINE [0] fromChar# #-}

-- | TODO
--
-- @since 1.0.0
toChar# :: Char -> Char#
toChar# (C# c#) = c#
{-# INLINE [0] toChar# #-}

-- Char# - Comparison ----------------------------------------------------------

infix 4 `gt#`, `ge#`, `eq#`, `ne#`, `lt#`, `le#`

-- | "Greater than" comparison on two 'Char#' values.
--
-- @since 1.0.0
gt# :: Char# -> Char# -> Bool#
gt# x# y# = Bool.unsafeFromInt# (GHC.gtChar# x# y#)

-- | "Greater than or equal to" comparison on two 'Char#' values.
--
-- @since 1.0.0
ge# :: Char# -> Char# -> Bool#
ge# x# y# = Bool.unsafeFromInt# (GHC.geChar# x# y#)

-- | "Equal to" comparison on two 'Char#' values.
--
-- @since 1.0.0
eq# :: Char# -> Char# -> Bool#
eq# x# y# = Bool.unsafeFromInt# (GHC.eqChar# x# y#)

-- | "Not equal to" comparison on two 'Char#' values.
--
-- @since 1.0.0
ne# :: Char# -> Char# -> Bool#
ne# x# y# = Bool.unsafeFromInt# (GHC.neChar# x# y#)

-- | "Less than" comparison on two 'Char#' values.
--
-- @since 1.0.0
lt# :: Char# -> Char# -> Bool#
lt# x# y# = Bool.unsafeFromInt# (GHC.ltChar# x# y#)

-- | "Less than or equal to" comparison on two 'Char#' values.
--
-- @since 1.0.0
le# :: Char# -> Char# -> Bool#
le# x# y# = Bool.unsafeFromInt# (GHC.leChar# x# y#)

-- Char# - Conversion ----------------------------------------------------------

-- | Converts a 'Char#' value to an 'Int#' value.
--
-- @since 1.0.0
fromInt# :: Int# -> Char#
fromInt# = GHC.chr#

-- | Converts a 'Char#' value to an 'Int#' value.
--
-- @since 1.0.0
toInt# :: Char# -> Int#
toInt# = GHC.ord#

-- Char# - Case Conversion -----------------------------------------------------

-- | TODO
--
-- @since 1.0.0
toLower# :: Char# -> Char#
toLower# = towlower#

-- | TODO
--
-- @since 1.0.0
toUpper# :: Char# -> Char#
toUpper# = towupper#

-- | TODO
--
-- @since 1.0.0
toTitle# :: Char# -> Char#
toTitle# = towtitle#

foreign import ccall unsafe "u_towlower"
  towlower# :: Char# -> Char#

foreign import ccall unsafe "u_towupper"
  towupper# :: Char# -> Char#

foreign import ccall unsafe "u_towtitle"
  towtitle# :: Char# -> Char#

-- Char# - Classification ------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
isControl# :: Char# -> Bool#
isControl# c# = Bool.unsafeFromInt# (iswcntrl# c#)

-- | TODO
--
-- @since 1.0.0
isSpace# :: Char# -> Bool#
isSpace# c# = Bool.unsafeFromInt# (iswspace# c#)

-- | TODO
--
-- @since 1.0.0
isLower# :: Char# -> Bool#
isLower# c# = Bool.unsafeFromInt# (iswlower# c#)

-- | TODO
--
-- @since 1.0.0
isUpper# :: Char# -> Bool#
isUpper# c# = Bool.unsafeFromInt# (iswupper# c#)

-- | TODO
--
-- @since 1.0.0
isAlpha# :: Char# -> Bool#
isAlpha# c# = Bool.unsafeFromInt# (iswalpha# c#)

-- | TODO
--
-- @since 1.0.0
isAlphaNum# :: Char# -> Bool#
isAlphaNum# c# = Bool.unsafeFromInt# (iswalnum# c#)

-- | TODO
--
-- @since 1.0.0
isPrint# :: Char# -> Bool#
isPrint# c# = Bool.unsafeFromInt# (iswprint# c#)

-- | TODO
--
-- @since 1.0.0
isDigit# :: Char# -> Bool#
isDigit# c# = Bool.and# (ge# c# '0'#) (le# c# '9'#)

-- | TODO
--
-- @since 1.0.0
isOctDigit# :: Char# -> Bool#
isOctDigit# c# = Bool.and# (ge# c# '0'#) (le# c# '7'#)

-- | TODO
--
-- @since 1.0.0
isHexDigit# :: Char# -> Bool#
isHexDigit# c# =
  let !isHexLower# = Bool.and# (ge# c# 'a'#) (le# c# 'f'#)
      !isHexUpper# = Bool.and# (ge# c# 'A'#) (le# c# 'F'#)
   in Bool.or# (isDigit# c#) (Bool.or# isHexLower# isHexUpper#)

foreign import ccall unsafe "u_iswalpha"
  iswalpha# :: Char# -> Int#

foreign import ccall unsafe "u_iswalnum"
  iswalnum# :: Char# -> Int#

foreign import ccall unsafe "u_iswcntrl"
  iswcntrl# :: Char# -> Int#

foreign import ccall unsafe "u_iswspace"
  iswspace# :: Char# -> Int#

foreign import ccall unsafe "u_iswprint"
  iswprint# :: Char# -> Int#

foreign import ccall unsafe "u_iswlower"
  iswlower# :: Char# -> Int#

foreign import ccall unsafe "u_iswupper"
  iswupper# :: Char# -> Int#

-- Char# - Classification - Subranges ------------------------------------------

-- | TODO
--
-- @since 1.0.0
isAscii# :: Char# -> Bool#
isAscii# c# = lt# c# '\x80'#

-- | TODO
--
-- @since 1.0.0
isAsciiLower# :: Char# -> Bool#
isAsciiLower# c# = Bool.and# (ge# c# 'a'#) (le# c# 'z'#)

-- | TODO
--
-- @since 1.0.0
isAsciiUpper# :: Char# -> Bool#
isAsciiUpper# c# = Bool.and# (ge# c# 'A'#) (le# c# 'Z'#)

-- | TODO
--
-- @since 1.0.0
isLatin1# :: Char# -> Bool#
isLatin1# c# = lt# c# '\xff'#

-- Char# - Show ----------------------------------------------------------------

-- | Display an 'Char#' value as an ordinary 'String'.
--
-- >>> show# 'F'#
-- "'F'"
--
-- @since 1.0.0
show# :: Char# -> String
show# c# = show (C# c#)
{-# INLINE show# #-}

-- | Display an 'Char#' value as a readable string.
--
-- @since 1.0.0
shows# :: Char# -> ShowS
shows# c# = showChar '\'' . showLitChar (C# c#) . showChar '\''
{-# INLINE shows# #-}
