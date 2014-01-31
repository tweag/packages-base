-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.StaticRef
-- Copyright   :  (c) Tweag I/O, 2014
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- References to static values.
--
-- The form @static e@ produces terms of type @StaticRef a@ when @e :: a@
-- and the StaticValues language extension is enabled.
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable #-}
module GHC.StaticRef where

import Data.Typeable (Typeable)

-- | A reference to a top-level value of type 'a'.
data StaticRef a = StaticRef { unStaticRef :: GlobalName }
  deriving (Show, Typeable)

-- | Global names identifying top-level values.
--
-- > GlobalName package_id package_installed_suffix module_name value_name
--
data GlobalName = GlobalName String String String String
  deriving (Show, Typeable)

