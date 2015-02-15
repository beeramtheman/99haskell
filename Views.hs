{-# LANGUAGE OverloadedStrings #-}

module Views (root) where

import Problems (Problem(..), problems)
import Data.Monoid (mempty)
import qualified Web.Scotty as S
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text
import Prelude hiding (head, div, id)

root :: Int -> S.ActionM ()
root i = S.html . renderHtml $ do
    head $ do
        H.title "99 Haskell"
        link ! rel "stylesheet" ! href "https://maxcdn.bootstrapcdn.com\
            \/font-awesome/4.3.0/css/font-awesome.min.css"
        link ! rel "stylesheet" ! href
            "https://fonts.googleapis.com/css?family=Open+Sans:400,400italic"
        link ! rel "stylesheet" ! href
            "https://fonts.googleapis.com/css?family=Source+Code+Pro"
        link ! rel "stylesheet" ! href "/css/root.css"

    body $ do
        div ! class_ "datax" ! dataAttribute "num" (toValue i) $ mempty
        div ! class_ "topbar" $ mempty
        header "99 Haskell"

        div ! class_ "io-wrap" $ do
            section ! class_ "in" $ do
                div ! class_ "dashboard" $ do
                    div ! class_ "problem" $ do
                        b $ do
                            toHtml i
                            ". "
                        toHtml . description $ problems !! (i - 1)

                    div ! class_ "control" $ do
                        a ! href "http://example.com" ! target "_blank" $ "Examples"
                        br
                        a ! href "http://example.com" ! target "_blank" $ "Solutions"

                div ! class_ "termbar" $ do
                    div ! class_ "left" $ do
                        H.span ! class_ "fullscreen fa fa-arrows-alt"
                               ! A.title "Fullscreen" $ mempty
                        H.span ! class_ "filename" $ "Awesome.hs"

                    div ! class_ "right" $ do
                        H.span ! class_ "run fa fa-play"
                               ! A.title "Run code" $ mempty
                        H.span ! class_ "stop fa fa-stop"
                               ! A.title "Stop running" $ mempty

                div ! id "terminal" $ toHtml . hint $ problems !! (i - 1)

            section ! class_ "out" $ mempty

        footer $ do
            div ! class_ "quote" $
                "\"I've got 99 problems but a side effect ain't one\""

            "A service made by "
            a ! href "http://bram.gg" $ "Bram Hoskin"
            " - "
            a ! href "/about" $ "About"

        script ! src "https://cdn.jsdelivr.net/ace/1.1.8/min/ace.js" $ mempty
        script ! src "/js/root.js" $ mempty
