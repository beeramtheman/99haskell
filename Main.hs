{-# LANGUAGE OverloadedStrings #-}

import Problems (Problem, problems)
import Sandbox
import Web.Scotty
import Network.Wai.Middleware.RequestLogger
import qualified Views
import qualified Styles
import System.Process
import System.IO
import System.Exit
import Control.Applicative
import Network.CGI hiding (setHeader)
import Data.Text.Lazy

runCode :: String -> IO (ExitCode, String, String)
runCode c = readProcessWithExitCode "docker"
            ["run", "-i", "haskell", "/bin/bash"]
            ("echo '" ++ c ++ "' | runhaskell")

reqs :: ScottyM ()
reqs = do
    get "/" $ Views.root (problems !! 0) 1

    get "/css/root.css" $ do
        setHeader "Content-Type" "text/css; charset=utf-8"
        text Styles.root

    get "/js/root.js" $ file "Script.js"

    post "/sandbox" $ do
        code <- param "code"
        try <- liftIO $ tryProblem 0 code
        json try

main :: IO ()
main = do
    scotty 3000 $ do
        middleware logStdoutDev
        reqs
