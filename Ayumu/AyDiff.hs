-----------------------------------------------------------------------------
-- |
-- Module      :  Ayumu.Diff
-- Copyright   :  (c) Julian K. Arni
--
-- Maintainer  :  jkarni@gmail.com
-- Stability   :  unstable
--
-- Flexible diffs
--
-----------------------------------------------------------------------------

module Ayumu.AyDiff
    where

--- Imports {{{

import Data.List ( partition )
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Control.Monad ( liftM, liftM2 )
import qualified Control.Lens as L
import Data.Algorithm.Diff ( Diff( First, Second, Both), getGroupedDiff  )
import Data.Graph.Inductive.Graph ( Node )
import Data.Maybe ( listToMaybe )
import Ayumu.AyDoc

-- }}}
-- | (Add and) commit changes to AyDoc
commit' :: AyDoc String ()
commit' = do
        f <- L.use (revision.file)
        fs <- liftM lines $ liftIO $ readFile f

        r <- revision.revNo L.<+= 1
        _ <- (revision.branches.L._2.commits) L.<%= zInsert r

        cur <- cdLines
        _ <- addDiff (getGroupedDiff (cur) ("start":fs ++ ["end"]))
        return ()

-- | Add a list of diffs to AyDoc over the current walk.
addDiff        :: Show a => [Diff [a]] -> AyDoc a ()
addDiff []     = return ()
addDiff xs     = addDiff' 0 xs



 --------------------------------------------------------------------------
 -- Utilities
 --
 --------------------------------------------------------------------------


isBoth, isFirst, isSecond     :: Diff a -> Bool
isBoth (Both _ _)             = True
isBoth _                      = False
isFirst (First _)             = True
isFirst _                     = False
isSecond                      = not . liftM2 (&&) isFirst isBoth


diffContent          :: Diff a -> a
diffContent (First a)  = a
diffContent (Second a) = a
diffContent (Both a _) = a

(<<) :: Monad m => m b -> m a -> m b
a << b = b >> a

-- TODO: check that recursive calls are getting passed the right node.
addDiff'          :: Show a => Node -> [Diff [a]] -> AyDoc a ()
addDiff' _ []     = return ()
addDiff' n ds     =
        let (x,xs)   = break isBoth ds
            (fs,sns) = partition isFirst x
            f        = listToMaybe $ map diffContent fs
            s        = listToMaybe $ map diffContent sns
            (bts,ms) = span isBoth xs
            --   this really needs to be fixed - remove head vvvv
            iter m   = addDiff' (m + (length $ diffContent $ head bts)) ms
            jnext a  = n + length a + 1
        in case (f, s) of
           (Just a, Just b)   -> addLines b n (jnext a)  << iter (jnext a)
           (Just a, Nothing)  -> delLines n (jnext a)    << iter (jnext a)
           (Nothing, Just b)  -> addLines b n (n + 1)    << iter (n + 1)
           (Nothing, Nothing) -> iter (n + 1)
