module Ayumu.Init where 


import System.Directory ( createDirectory, 
                        doesDirectoryExist, 
                        getDirectoryContents,
                        getCurrentDirectory)
                        
import System.FilePath.Posix
import Control.Monad ( unless )

aydir :: IO FilePath
aydir = do 
           cwd <- getCurrentDirectory 
           return (cwd ++ "/.ay")

createRepository :: IO ()
createRepository = do
        d <- aydir
        ex <- doesDirectoryExist d
        unless ex $ do 
                   _ <- putStrLn $ "Creating " ++ d
                   createDirectory d
                   createDirectory $ d ++ "/data"
                   createDirectory $ d ++ "/revs"
                   createDirectory $ d ++ "/cfg"

getRepository :: IO (Maybe FilePath)
getRepository = getCurrentDirectory >>= fRepo where
    fRepo d = do
        ex <- doesDirectoryExist (d ++ "/.ay")
        if ex then return (Just $ d ++ "/.ay")
              else if (d == "/") then return Nothing
                                 else fRepo (takeDirectory d)
                  
