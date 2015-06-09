module Sandbox (tryProblem) where

import Problems (problems, Problem(..), Test)
import System.Process
import System.Timeout
import System.IO
import Control.Monad (void)
import Control.Concurrent.Async
import Data.Unique
import System.Directory
import Data.List
import Data.Aeson hiding (json)
import Data.Text (pack)

data TestRes = TestRes { test       :: Test
                       , testSucc   :: Bool
                       , testOut    :: String
                       } deriving (Show)

data Mark = Mark { markSucc  :: Bool
                 , markTests :: [TestRes]
                 } deriving (Show)

instance ToJSON TestRes where
    toJSON (TestRes t s o) = object [ pack "test" .= t
                                    , pack "success" .= s
                                    , pack "output" .= o ]

instance ToJSON Mark where
    toJSON (Mark s t) = object [pack "success" .= s, pack "tests" .= t]

trim :: Char -> String -> String
trim v = x . x where x = reverse . dropWhile (\x -> x == v)

hGetContentsEager :: Handle -> IO String
hGetContentsEager h = do
    readable <- hIsReadable h
    end <- hIsEOF h
    
    if readable && not end then do
        x <- hGetLine h
        y <- hGetContentsEager h
        return . trim '\n' $ x ++ "\n" ++ y
    else
        return ""

stripError :: String -> String
stripError []     = ""
stripError e | "/mnt/" `isPrefixOf` e = stripError $ drop 5 e
             | "<interactive>:" `isPrefixOf` e = stripError $ drop 19 e
             | "<interactive>" `isPrefixOf` e = stripError $ drop 13 e
             | "*** Exception" `isPrefixOf` e = stripError $ drop 4 e
stripError (x:xs) = case take 13 xs == "<interactive>" of
                    True -> [x]
                    False -> x : stripError xs

makeTestRes :: Test -> Bool -> String -> String -> TestRes
makeTestRes t s r e | length e > 0 = TestRes t s $ stripError e
                    | otherwise    = TestRes t s r

runTest :: Int -> String -> Test -> IO TestRes
runTest to c t = do
    unique <- newUnique
    let code = 'a':(show $ hashUnique unique) -- docker requires letter initial
    let dir = "/tmp/99haskell/" ++ code
    createDirectoryIfMissing True dir
    writeFile (dir ++ "/Main.hs") c

    (Just hin, Just hout, Just herr, hproc) <-
        createProcess
            (proc "docker" [ "run"
                           , "--name=" ++ code
                           , "--interactive=true"
                           , "--volume=" ++ dir ++ ":/mnt"
                           , "haskell"
                           , "/bin/bash" ])
            { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }

    hPutStrLn hin "ghci -v0 /mnt/Main.hs"
    hPutStrLn hin $ fst t
    hClose hin

    maybeOutput <- timeout (to * 1000000) (return =<< (hGetContentsEager hout))

    let output = case maybeOutput of Nothing -> ""
                                     Just a -> a

    error <- case maybeOutput of Nothing -> return $ "99Haskell: Timeout Error\
                                                \. Program did not exit after "
                                                ++ show to ++ " seconds."
                                 Just _  -> hGetContentsEager herr

    hClose hout
    hClose herr
    void $ waitForProcess hproc

    async $ do
        readProcess "docker" ["rm", "-f", code] ""
        removeDirectoryRecursive dir

    return $ makeTestRes t (output == snd t) output error

tryProblem :: Int -> String -> IO Value
tryProblem i c = do
    allTests <- mapConcurrently (runTest 5 c) (tests $ problems !! (i - 1))
    return . toJSON $ Mark (all (==True) [testSucc x | x <- allTests]) allTests
