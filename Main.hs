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
import Data.Text.Lazy hiding (length, isInfixOf)
import Data.List (isInfixOf)

validProblem :: Int -> Bool
validProblem p = p > 0 && p <= (length problems)

sendProblem :: Int -> ActionM ()
sendProblem p | validProblem p = Views.root p
              | otherwise = text "All done! :D Add more problems: git.io/p668"

sandboxFilter :: Int -> String -> ActionM ()
sandboxFilter n c | length c > 1500         = text "Over 1500 characters."
                  | not $ validProblem n    = text "Out of range!"
                  | "import " `isInfixOf` c = text "Can not contain imports"
                  | otherwise               = json =<< liftIO (tryProblem n c)

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
        sandboxFilter num code

    get "/problems" Views.list

    get "/css/general.css" $ do
        setHeader "Content-Type" "text/css; charset=utf-8"
        text Styles.general

main :: IO ()
main = do
    scotty 3000 $ do
        middleware logStdoutDev
        reqs
