{-# LANGUAGE OverloadedStrings #-}

import Problems (Problem, problems)
import Sandbox
import Web.Scotty
import Web.Scotty.Session
import Network.Wai.Middleware.RequestLogger
import qualified Views
import qualified Styles
import System.Process
import System.IO
import System.Exit
import Control.Applicative
import Network.CGI hiding (setHeader)
import Data.Text.Lazy hiding (length)

validProblem :: Int -> Bool
validProblem p = p > 0 && p <= (length problems)

sendProblem :: Int -> ActionM ()
sendProblem p | validProblem p = Views.root p
              | otherwise = text "Out of range!"

reqs :: ScottyM ()
reqs = do
    get "/" $ sendProblem 1

    get (regex "^/([0-9]+)$") $ do
        num <- param "1"
        sendProblem $ read num

    get "/css/root.css" $ do
        setHeader "Content-Type" "text/css; charset=utf-8"
        text Styles.root

    get "/js/root.js" $ file "Script.js"

    post (regex "^/sandbox/([0-9]+)$") $ do
        num <- param "1"
        code <- param "code"

        if length code > 1500 then
            text "Why is your code so long smh"

        else if not (validProblem num) then
            text "Out of range!"

        else do
            try <- liftIO $ tryProblem num code
            json try

    get "/problems" Views.list

    get "/css/general.css" $ do
        setHeader "Content-Type" "text/css; charset=utf-8"
        text Styles.general

main :: IO ()
main = do
    scotty 3000 $ do
        middleware logStdoutDev
        reqs
