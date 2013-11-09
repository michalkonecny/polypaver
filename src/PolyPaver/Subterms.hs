{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
module PolyPaver.Subterms where

import PolyPaver.Form

import Data.Hashable

--import Data.Data (Data, Typeable)
--import Data.List (intercalate, sortBy)

addHashes ::
    Hashable a =>
    Term a -> Term Int
addHashes term =
    error "TODO"
