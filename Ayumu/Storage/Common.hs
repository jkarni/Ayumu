-- Pragmas {{{

-- }}}

--------------------------------------------------------------------------
-- |
-- Module      :   Ayumu.Storage.Common
-- Copyright   :   (c) Julian K. Arni
--
-- Maintainer  :  jkarni@gmail.com
-- Stability   :  unstable
--
-- A common set of utilities for storing AyDocs
--
--
-----------------------------------------------------------------------------


-- Exports {{{
module Ayumu.Storage.Common
    where

-- }}}


-- Imports {{{

import Ayumu.AyDoc
import Data.Graph.Inductive.Graph ( 
              Graph(..),
              DynGraph(..), 
              LEdge, 
              LNode,
              insEdges,
              insNodes,
              labEdges,
              labNodes)
import Control.Lens ( (^.))
import Data.Binary

-- }}}


instance Binary a => Binary (AyGr a) where
        get = unserializeGr'
        put = serializeGr'

 --------------------------------------------------------------------------
 -- Internals {{{
 --------------------------------------------------------------------------

type NsAndEs a b = ([LNode a], [LEdge b])

serializeGr'   :: Binary a => AyGr a -> Put
serializeGr' g = do put (toNodesAndEdges (g^.graph))
                    put (g^.revision)

unserializeGr' :: Binary a => Get (AyGr a)
unserializeGr' = do 
   nses <- get
   dst  <- get 
   return AyGr { 
               _graph = insEdges (snd nses) $ insNodes (fst nses) empty ,
               _revision = dst
               }

 
toNodesAndEdges   :: DynGraph g => g a b -> ([LNode a], [LEdge b])
toNodesAndEdges g = (labNodes g, labEdges g)

--}}}
