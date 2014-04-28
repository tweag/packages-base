-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Ref
-- Copyright   :  (C) 2014 EURL Tweag
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- Symbolic references to values.
--
-- References to values are usually implemented with memory addresses, and this
-- is practical when communicating values between the different pieces of a
-- single process.
--
-- When values are communicated across different processes running in possibly
-- different machines, though, addresses are no longer useful since each
-- process may use different addresses to store a given value.
--
-- To solve such concern, the references provided by this module indicate
-- package, module and name of a value. This information could be used to locate
-- the value in different processes.
--
-- Currently, the main use case for references is the StaticValues language
-- extension.
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable #-}
module GHC.Ref where

import Data.Typeable (Typeable)

-- | A reference to a top-level value of type 'a'.
data Ref a = Ref { unRef :: GlobalName }
  deriving (Show, Typeable)

-- | Global names identifying top-level values.
--
-- > GlobalName package_id installed_package_suffix module_name value_name
--
-- In essence a 'GlobalName' augments the information provided by
-- 'Language.Haskell.TH.Syntax.Name' with the information in the
-- @installed_package_suffix@ field.
--
-- The package identifier usually includes the package name and the package
-- version.
--
-- The installed package identifier
-- (Cabal:'Distribution.Package.InstalledPackageId') includes the package
-- identifier and oftenly a hash value that is needed to identify the package
-- when multiple variations of it are installed. Thus, a global name includes
-- this complementary information in the @installed_package_suffix@ field.
--
-- Beware that GHC has a notion of wired-in packages, like ghc-prim, base and
-- others. For these packages, the package version may not appear in the
-- package identifier but in the installed package suffix instead.
--
data GlobalName = GlobalName String String String String
  deriving (Show, Typeable)

