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
import Ayumu.Init
import Ayumu.Storage.Common
import Ayumu.Storage.FS
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Control.Monad.State ( runStateT )
import Control.Monad ( void, forM_, forM )
import Control.Applicative
import System.Directory
import System.Posix.Files
import Options.Applicative
import Prelude hiding ( init )

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
        <> header "Ayumu -  A flexible version control system")

entrypoint          :: Opts -> IO ()
entrypoint (Opts a) = case a of
    (Commit o)   -> return ()
    (Checkout o) -> return ()
    (Init o)     -> void $ init o


{-main = do-}
          {-putStrLn "Welcome."-}
          {-x <- runStateT mainloop (initial "data/Example.txt")-}
          {-putStrLn "Done."-}
          {-print x-}
          {-return ()-}


-- }}}
 --------------------------------------------------------------------------
 -- Command paths {{{
 --------------------------------------------------------------------------

init    ::  InitOptions -> IO ()
init i  =  do 
              createRepository 
              cwd <- getCurrentDirectory
              touchFile (cwd ++ "/.ay/data/" ++ ( inputfile i ))


commit     :: CommitOptions -> IO ()
commit _   = do 
    cdw <- getCurrentDirectory
    fs <- getDirectoryContents cdw
    conts <- forM fs fromFile
    mapM_ (runStateT commit') conts
    return ()



 --  }}}
 --------------------------------------------------------------------------
 -- Optparsing {{{
 --------------------------------------------------------------------------


-- Parsers {{{

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
-- }}}
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

