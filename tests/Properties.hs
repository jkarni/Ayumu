
module Properties where

import Ayumu.AyDoc
 
import Test.QuickCheck
import Test.HUnit

import Data.Graph.Inductive.Graph as DG
import Data.Graph.Inductive.Tree as DGt
 
instance (Arbitrary a, Arbitrary b) =>
    Arbitrary (DGt.Gr a b) where
    arbitrary 
