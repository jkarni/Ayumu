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
import Ayumu.Storage.Common
import Ayumu.Storage.FS
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Control.Monad.State ( runStateT )
import Control.Monad ( void )
import Options.Applicative
import Control.Applicative

-- }}}
 --------------------------------------------------------------------------
 -- Main {{{
 --------------------------------------------------------------------------
main :: IO ()
main = mainopts

mainopts :: IO ()
mainopts = execParser parseopts >>= entrypoint
    where
     parseopts = info (helper <*> opts)
        ( fullDesc
        <> progDesc "File version control"
        <> header "ayumu - flexible version control")

entrypoint          :: Opts -> IO ()
entrypoint (Opts a) = case a of
    (Commit (CommitOptions b)) -> return ()
    (Checkout (CheckoutOptions a b)) -> return ()
    (Init (InitOptions a)) -> return ()

{-main = do-}
          {-putStrLn "Welcome."-}
          {-x <- runStateT mainloop (initial "data/Example.txt")-}
          {-putStrLn "Done."-}
          {-print x-}
          {-return ()-}


-- }}}
 --------------------------------------------------------------------------
 -- Optparsing {{{
 --------------------------------------------------------------------------



opts :: Parser Opts
opts = Opts
    <$> subparser
    (command "init"       (info initOpts
                          (progDesc "Initialize version control for a file"))
    <> command "commit"   (info commitOpts
                          (progDesc "Add and commit current worktree"))
    <> command "checkout" (info checkoutOpts
                          (progDesc "Checkout a branch or commit"))
    )


initOpts :: Parser Command
initOpts = Init . InitOptions
    <$> strOption
        ( long "inputfile"
        <> short 'i'
        <> metavar "FILE"
        <> help "Initialize version control for FILE"
        )

commitOpts :: Parser Command
commitOpts = Commit . CommitOptions
    <$> switch
        ( long "printdiff"
        <> short 'p'
        <> help "Print the diff between new commits and previous commit")

checkoutOpts :: Parser Command
checkoutOpts = (Checkout .) . CheckoutOptions
    <$>  strOption
        ( long "branch"
        <> short 'b'
        <> metavar "BRANCHNAME"
        <> help "Checkout BRANCHNAME")
    <*> option
        ( long "commit"
        <> short 'c'
        <> metavar "COMMIT#"
        <> help "Checkout COMMIT#")

-- Types {{{

data Opts = Opts {
    cmd :: Command
} deriving (Show, Read)

data Command =
    Init InitOptions            |
    Commit CommitOptions        |
    Checkout CheckoutOptions
    deriving (Show, Read)

data InitOptions = InitOptions {
    inputfile :: FilePath
} deriving (Show, Read)

data CommitOptions = CommitOptions {
    printDiff :: Bool
} deriving (Show, Read)
data CheckoutOptions = CheckoutOptions {
    branch :: String ,
    commitNo :: Int
} deriving (Show, Read)
-- }}}

 -- }}}
 --------------------------------------------------------------------------
 -- interactive (for dev purposes) {{{
 --------------------------------------------------------------------------

{-mainloop :: AyDoc String ()-}
{-mainloop =  processCmd >> mainloop-}


{-data Cmd = Commit | Checkout | PrintCur deriving (Show, Read)-}

{-processCmd :: AyDoc String ()-}
{-processCmd = do-}
        {-c <- liftIO getLine-}
        {-let cmd = read c::Cmd-}
        {-case cmd of-}
                     {-Commit    -> void commit'-}
                     {-Checkout  -> return ()-}
                     {-PrintCur  -> do-}
                         {-ls <- cdLines-}
                         {-_  <- (liftIO . putStrLn) (unlines ls)-}
                         {-return ()-}




-- }}}

