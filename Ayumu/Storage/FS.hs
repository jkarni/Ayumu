-- Pragmas {{{

-- }}}

--------------------------------------------------------------------------
-- |
-- Module      :   Ayumu.Storage.FS
-- Copyright   :   (c) Julian K. Arni
--
-- Maintainer  :  jkarni@gmail.com
-- Stability   :  unstable
--
-- Store serialized representations of an AyGr in a filesystem
--
--
-----------------------------------------------------------------------------

-- Exports {{{
module Ayumu.Storage.FS
    where

-- }}}
  
   
import Data.Binary
import Ayumu.AyDoc
import Ayumu.Storage.Common

toFile       ::   Binary a => FilePath -> AyGr a -> IO ()
toFile       =   encodeFile

fromFile     ::  Binary a => FilePath -> IO (AyGr a)
fromFile     =   decodeFile
