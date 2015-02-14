module Sandbox (tryProblem) where

import Problems (problems, Problem(..), Test)
import System.Process
import System.IO
import Control.Concurrent.Async
import Data.Unique
import System.Directory
import Data.List
import Data.Aeson hiding (json)
import Data.Text hiding (length, intercalate, drop)

data TestRes = TestRes { test       :: Test
                       , testSucc   :: Bool
                       , testRet    :: String
                       , testErr    :: String
                       } deriving (Show)

data Mark = Mark { markSucc  :: Bool
                 , markTests :: [TestRes]
                 } deriving (Show)

instance ToJSON TestRes where
    toJSON (TestRes t s r e) = object [ pack "test" .= t
                                      , pack "success" .= s
                                      , pack "returned" .= r
                                      , pack "error" .= e ]

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

returned :: [String] -> String
returned o = drop 7 $ o !! (length o - 2)

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
    return $ TestRes t (success t output) (returned output) error

tryProblem :: Int -> String -> IO Value
tryProblem i c = do
    allTests <- sequence [runTest t c | t <- tests (problems !! (i - 1))]
    return . toJSON $ Mark (elem True [testSucc x | x <- allTests]) allTests
