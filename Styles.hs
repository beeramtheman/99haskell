{-# LANGUAGE OverloadedStrings #-}

module Styles (Styles.root) where

import Clay
import qualified Clay.Media as M
import Data.Text.Lazy
import qualified Data.Map as Map

editWidth = 648
minOutWidth = 590
sep = 20

bigScreen = editWidth + sep + minOutWidth

-- http://www.colourlovers.com/palette/49963/let_them_eat_cake
palette :: String -> Color
palette c = case Map.lookup c colors of
    Nothing -> transparent
    Just x -> x
    where colors = Map.fromList [ ("near black", "#212121")
                                , ("near white", "#dedede")
                                , ("decorative", "#c5e0dc")
                                , ("background", "#fbf9f5")
                                , ("bg borders", "#ece5ce")
                                , ("boldest bg", "#fcf6ef")
                                , ("bold walls", "#f1d4af")
                                , ("darkest bg", "#575757")
                                , ("dull walls", "#dbdbdb")
                                , ("dullest bg", "#f5f5f5")
                                , ("dull bg #2", "#ededed")
                                , ("test wrong", "#e86868")
                                , ("test right", "#6be868")
                                , ("big action", "#ededed")]

root :: Text
root = render $ do
    body ? do
        margin (px 0) (px 0) (px 0) (px 0)
        fontFamily ["Open Sans"] [serif]
        fontSize (px 16)
        color $ palette "near black"

    ".topbar" ? do
        height (px 6)
        backgroundColor $ palette "decorative"

    header ? do
        height (px 100)
        lineHeight (px 100)
        fontSize (px 20)
        textAlign (alignSide sideCenter)

    ".io-wrap" ? do
        position relative
        margin (px 0) auto (px 0) auto

        query Clay.all [M.minWidth (px bigScreen)] $ do
            width (px bigScreen)
            section # ".in" ? width (px editWidth)

            section # ".out" ? do
                position absolute
                top (px 0)
                right (px 0)
                width (px minOutWidth)
                maxHeight (pct 100)

        query Clay.all [M.maxWidth (px $ bigScreen - 1)] $ do
            width (px editWidth)
            section # ".out" ? margin (px sep) (px 0) (px 0) (px 0)

    section # ".out" ? do
        boxSizing borderBox
        fontSize (px 13)
        border solid (px 1) $ palette "dull walls"
        overflow auto
        backgroundColor $ palette "dullest bg"

        ".test" ? do
            position relative

            ":nth-child(even)" & do
                backgroundColor $ palette "dull bg #2"

            ".success" ? do
                position absolute
                top (px 0)
                left (px 0)
                width (px 10)
                height (pct 100)
                ".true" & backgroundColor (palette "test right")
                ".false" & backgroundColor (palette "test wrong")

            ".overview" ? do
                padding (px 10) (px 10) (px 10) (px 20)

    ".dashboard" ? do
        position relative
        marginBottom (px sep)

        ".problem" ? do
            width (other "calc(80% - 10px)")
            boxSizing borderBox
            padding (px 10) (px 10) (px 10) (px 10)
            border solid (px 1) $ palette "bg borders"
            backgroundColor $ palette "background"

        ".control" ? do
            position absolute
            top (px 0)
            right (px 0)
            width (other "calc(20% - 10px)")
            height (pct 100)
            boxSizing borderBox
            paddingTop (px 9)
            border solid (px 1) $ palette "bold walls"
            textAlign (alignSide sideCenter)
            backgroundColor $ palette "boldest bg"

    ".termbar" ? do
        height (px 30)
        padding (px 0) (px 10) (px 0) (px 10)
        borderRadius (px 5) (px 5) (px 0) (px 0)
        fontSize (px 20)
        color $ palette "near white"
        backgroundColor $ palette "darkest bg"

        ".left" ? do
            float floatLeft

            ".filename" ? do
                position absolute
                margin (px 0) (px 0) (px 0) (px 10)
                fontSize (px 14)

        ".right" ? float floatRight

        "span" ? do
            lineHeight (px 30)
            cursor pointer

    "#terminal" ? do
        height (px 300)
        fontFamily ["Source Code Pro"] [monospace]
        fontSize (px 14)
        boxShadow (px 0) (px 3) (px 6) (rgba 0 0 0 51)
        transition "box-shadow" 0.3 ease 0

    "#terminal.ace_focus" ? boxShadow (px 0) (px 3) (px 6) (rgba 0 0 0 102)

    footer ? do
        margin (px 50) (px 0) (px 8) (px 0)
        textAlign (alignSide sideCenter)

        ".quote" ? fontStyle italic
