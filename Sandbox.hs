module Sandbox (tryProblem) where

import Problems (problems, Problem(..), Test)
import System.Process
import System.IO
import Control.Concurrent.Async
import Data.Unique
import System.Directory
import Data.List
import Data.Aeson hiding (json)
import Data.Text hiding (length)

data TestRes = TestRes { testSucc   :: Bool
                       , testErr    :: String
                       } deriving (Show)

data Mark = Mark { markSucc  :: Bool
                 , markTests :: [TestRes]
                 } deriving (Show)

instance ToJSON TestRes where
    toJSON (TestRes s e) = object [pack "success" .= s, pack "error" .= e]

instance ToJSON Mark where
    toJSON (Mark s t) = object [pack "success" .= s, pack "tests" .= t]

hGetAllLines :: Handle -> IO [String]
hGetAllLines h = do
    readable <- hIsReadable h
    end <- hIsEOF h
    
    if readable && not end then do
        x <- hGetLine h
        xs <- hGetAllLines h
        return (x:xs)
    else
        return []

success :: Test -> [String] -> Bool
success t o = (o !! (length o - 2) == "*Main> " ++ (snd t))

runTest :: Test -> String -> IO TestRes
runTest t c = do
    unique <- newUnique
    let dir = "/tmp/99haskell/" ++ (show $ hashUnique unique)
    createDirectoryIfMissing True dir
    writeFile (dir ++ "/Awesome.hs") c

    (Just hin, Just hout, Just herr, _) <-
        createProcess
            (proc "docker" ["run","-iv",dir ++ ":/mnt","haskell","/bin/bash"])
            { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }

    hPutStrLn hin "ghci /mnt/Awesome.hs"
    hPutStrLn hin $ fst t
    hClose hin

    output <- hGetAllLines hout
    hClose hout

    error <- hGetContents herr

    removeDirectoryRecursive dir
    return $ TestRes (success t output) (error)

tryProblem :: Int -> String -> IO Value
tryProblem i c = do
    allTests <- sequence [runTest t c | t <- tests (problems !! i)]
    return . toJSON $ Mark (elem True [testSucc x | x <- allTests]) allTests
