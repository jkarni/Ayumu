-- Pragmas {{{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

-- }}}
-----------------------------------------------------------------------------
-- |
-- Module      :  Ayumu.AyDoc
-- Copyright   :  (c) Julian K. Arni
--
-- Maintainer  :  jkarni@gmail.com
-- Stability   :  unstable
--
-- A state transformer with the revision graph and revision history as state.
--
--
-----------------------------------------------------------------------------

--- Exports {{{
module Ayumu.AyDoc
      where

--- }}}

--- Imports {{{

import Data.List ( nub)
import Data.Maybe ( mapMaybe )
import Data.Monoid
import Control.Monad.State ( StateT )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import qualified Control.Lens as L
import Data.Graph.Inductive.Graph
    ( Node,
      LNode,
      LEdge,
      Graph(labEdges, empty, labNodes, mkGraph, noNodes),
      nodes,
      DynGraph(..),
      suc,
      out,
      lab,
      insEdges,
      insNodes,
      insEdge,
      insNode)
import Data.Graph.Inductive.Tree ( Gr )

-- }}}
 --------------------------------------------------------------------------
 -- Branches and Commits {{{
 --------------------------------------------------------------------------

-- | Gates are edge labels. Used to identify a set of nodes as belonging to
-- a particular commit.
type Gate = Int
type Edge = LEdge Gate

-- Keep all the branches in a zipper, and all the commits in a branch
-- also in a zipper.
type Zipper a = ([a], a, [a])
-- | Branches is a list-zipper of "branch"es
type Branches = Zipper Branch
-- | An individual branch is a name and a zipper-list of commits. This is
-- the zipper
type Commits  = Zipper Gate


-- Zipper utility functions
zHeadwards, zTailwards, zFirst  :: Zipper a -> Zipper a
zHeadwards l@([], _,  _)        = l
zHeadwards   (hs,m,  ts)        = (init hs , last hs, m:ts)
zTailwards l@(_, _,  [])        = l
zTailwards   (hs,m,t:ts)        = (hs ++ [m],    t   , ts  )
zFirst     z@([],_,  _ )        = z
zFirst       (h:hs,m,ts)        = ( [], h , hs ++ [m] ++ ts)

zCursor               :: Zipper a -> a
zCursor (_, m, _)     = m

-- Inserts an element at the head of the zipper and focuses on that
-- element.
zInsert               :: a -> Zipper a -> Zipper a
zInsert a (hs, m, ts) = zFirst (a:hs, m, ts)

-- Return all elements at or to the right of a zipper.
zUntil               :: Zipper a -> [a]
zUntil (_, m, ts)    =  m:ts

data Branch = Branch {
    _name :: String ,
    _commits :: Commits
} deriving (Show, Read)

L.makeLenses ''Branch

 --}}}
 --------------------------------------------------------------------------
 -- Graph  {{{
 --------------------------------------------------------------------------

type Gr' a = Gr a Gate

instance Monoid (Gr' a) where
        mempty      = empty
        mappend a b = foldr ($) a toins
            where toins = [ insNode n | n <- labNodes b ] ++
                          [ insEdge e | e <- labEdges b ]

mkWalk            :: [a] -> Node -> [(LNode a, Edge)]
mkWalk [    ] _   = [                                         ]
mkWalk (x:[]) n   = ( (n, x), (n, n - 1, 0) ):[               ]
mkWalk (x:xs) n   = ( (n, x), (n, n + 1, 0) ):mkWalk xs (n + 1)

-- | Given a walk w and two nodes of a graph g, connect the walk to g at those
-- nodes
addWalk :: DynGraph g =>
     [(LNode a, Edge)]
    -> Node             -- ^ From node
    -> Node             -- ^ To node
    -> Gate
    -> g a Gate
    -> g a Gate
addWalk xs f t l g =
  let  ns             = fst $ unzip xs
       es             = init $ snd $ unzip xs
       start          = (f, fst $ head ns, l)
       end            = (fst $ last ns, t, 0)
       safeInsert     = insNodes $ filter (\(a,_) -> (not . (flip elem $ nodes g)) a) ns
  in insEdges (start:end:es) $ safeInsert g


-- | 'Delete' all nodes between f and t in a graph. This doesn't actually
-- delete any nodes, but instead create a new edge from f to t which "skips"
-- the nodes in between
delBetween         :: DynGraph g => x -> Node -> Node -> g a x -> g a x
delBetween l f t   = insEdge (f, t, l)


-- | Get a set of edges that will compose a path
getEdges' :: (Graph g) =>
        Node                -- ^ Starting node
        -> [Gate]            -- ^ list of gates (labels) to filter for
        -> g a Gate          -- ^ the graph to operate on
        -> [Edge]
getEdges' n gts g
        | null (suc g n)  = [   ]
        | otherwise       = next:getEdges' (snd' next) gts g
  where snd' (_,x,_) = x
        intersect'   = filter (\(_,_,x) -> x `elem` gts) (out g n)
        next         = head intersect'


-- | Return whatever nodes are needed from an original graph so that a list
-- of edges can actually be made a graph.
fillNodes      :: (Graph gr) => [Edge] -> gr a Gate -> [LNode a]
fillNodes es g =
    let ss   =   map (\(x,_,_) -> x) es
        ts   =   map (\(_,x,_) -> x)  es
        ns   =   nub (ss ++ ts)
    in filter (\(x,_) -> x `elem` ns) (labNodes g)


-- | Given a starting node, a list of gates (labels/commits) ordered by
-- 'preference' (i.e., take the first possible one in the list at each turn),
-- and a graph, get a walk from the start node till the end, taking always
-- the preferred turns.
getWalk'         :: (DynGraph g) => Node -> [Gate] -> g a Gate -> g a Gate
getWalk' n bs gr =
    let es   =   getEdges' n bs gr
        ns   =   fillNodes es gr
    in mkGraph ns es

-- | Make and connect walk
addLinesr   :: DynGraph g => [a] -> Node -> Node -> Node -> Gate -> g a Gate -> g a Gate
addLinesr a f t s l g =
        let nses = mkWalk a s
        in addWalk nses f t l g


-- }}}
 --------------------------------------------------------------------------
 -- AyDoc {{{
 -- Functions and types specific to the AyDoc monad
 --------------------------------------------------------------------------

----- Types ---------------------------------------------------------------

-- | DState keeps the information about revision history.
-- TODO: It might be best to split this up so (revNo on one side, everything
-- else on the other) so that state can be kept in Writer (graph, revno),
-- Reader (file, cfgs), and State (branches).
-- TODO: Using lenses no longer makes much sense, since types become much
-- less perspicuous. Switch out of them.
data DState   = DState {
    _revNo     :: Gate ,                           -- ^ latest rev1
    _file      :: FilePath ,                       -- ^ file we're working on
    _branches  :: Branches                         -- ^ A zipper of branches
} deriving (Show, Read)


data AyGr a = AyGr {
    _graph    :: Gr a Gate ,                     -- ^ the graph
    _revision :: DState                          -- ^ extra state data
} deriving (Show)

L.makeLenses ''AyGr
L.makeLenses ''DState


-- | DState constructor. Given a filepath, return an initial state for AyDoc.
initial     :: FilePath -> AyGr String
initial f   = L.set (revision.file) f initialDState

 --------------------------------------------------------------------------


-- | AyDoc itself: A state monad over IO that keeps both the document graph
-- and additional revision information.
type AyDoc a o = StateT (AyGr a) IO o



-- | Return the work tree as a list of lines
cdLines   :: (Show a, Monoid a) => AyDoc a [a]
cdLines   = do
     w <- curDoc'
     let ns = iterateSuc 0 w
     let ls = mapMaybe (lab w) ns
     _ <- (liftIO . print) ls
     return ls


-- | Return the walk corresponding to the current revision - i.e., the
-- 'work tree' equivalent.
curDoc'   :: AyDoc a (Gr a Gate)
curDoc'   = do
    cb <- L.use (revision.branches.L._2.commits)
    gr <- L.use graph
    return $ getWalk' 0 (zUntil cb) gr


addLines         :: Show a => [a] -> Node -> Node -> AyDoc a ()
addLines a f t   = do
    r  <- L.use (revision.revNo)
    n  <- L.use graph
    let nnodes = noNodes n
    _ <- graph L.<%= addLinesr a f t nnodes r
    return ()

delLines       :: Node -> Node -> AyDoc a ()
delLines f t   = do
    r <- L.use (revision.revNo)
    graph L.%= delBetween r f t
    return ()

-- | Get the current list of commits. This involves selecting the right
-- (current) branch and the right commit in that branch.
getCommit :: AyDoc a [Gate]
getCommit =  (L.use (revision.branches.L._2.commits)) >>= return . zUntil

--- }}}

 --------------------------------------------------------------------------
 -- Utils {{{
 --------------------------------------------------------------------------

iterateSuc   :: Graph gr => Node -> gr a b -> [Node]
iterateSuc x g
    | null $ suc g x  = []
    | otherwise       = next:iterateSuc next g
           where next = head $ suc g x


initialDState :: AyGr String
initialDState = AyGr {
    _graph      =   mkGraph [(0,"start"), (1,"end") ]
                            [(0, 1, 0) ] ,
    _revision   =   DState {
          _revNo    =   0  ,
          _file     =   "" ,
          _branches =  (
                            [] ,
                            Branch {
                                _name = "master" ,
                                _commits = ([], 0, [])
                                   },
                            []
                        )
                           }
                     }

-- Given a list of integers representing the lines in the current work
-- tree, 1 being the first, return the list of nodes that correspond to
-- those lines.
_linesToNode      :: [Int] -> AyDoc a [Node]
_linesToNode a    = do
    w <- curDoc'
    let ns = (iterate . (head .) . suc) w 0
    return $  map (ns !!) a

--- }}}
 --------------------------------------------------------------------------
 -- }}}
