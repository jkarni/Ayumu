{-# LANGUAGE NoMonomorphismRestriction #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Ayumu.O
-- Copyright   :  (c) Julian K. Arni
--
-- Maintainer  :  jkarni@gmail.com
-- Stability   :  unstable
--
-- IO-related tasks, including:
--   1) Processing diffs
--   2) Storing data
--   3) Updating files
--   4) Optparsing
--
--
-----------------------------------------------------------------------------

module Main where

--- Imports {{{

import Ayumu.AyDoc
import Ayumu.AyDiff
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Control.Monad.State ( runStateT )
import Control.Monad ( void )

-- }}}
 --------------------------------------------------------------------------
 -- Main {{{
 --------------------------------------------------------------------------
main :: IO ()
main = do
          putStrLn "Welcome."
          x <- runStateT mainloop (initial "data/Example.txt")
          putStrLn "Done."
          print x
          return ()


-- }}}

 --------------------------------------------------------------------------
 -- interactive (for dev purposes) {{{
 --------------------------------------------------------------------------

mainloop :: AyDoc String ()
mainloop =  processCmd >> mainloop


data Cmd = Commit | Checkout | PrintCur deriving (Show, Read)

processCmd :: AyDoc String ()
processCmd = do
        c <- liftIO getLine
        let cmd = read c::Cmd
        case cmd of
                     Commit    -> void commit'
                     Checkout  -> return ()
                     PrintCur  -> do
                         ls <- cdLines
                         _  <- (liftIO . putStrLn) (unlines ls)
                         return ()




-- }}}
