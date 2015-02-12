{-# LANGUAGE OverloadedStrings #-}

module Styles (Styles.root) where

import Clay
import Data.Text.Lazy
import qualified Data.Map as Map

-- http://www.colourlovers.com/palette/49963/let_them_eat_cake
palette :: String -> Color
palette c = case Map.lookup c colors of
    Nothing -> transparent
    Just x -> x
    where colors = Map.fromList [ ("near black", "#212121")
                                , ("near white", "#DEDEDE")
                                , ("decorative", "#C5E0DC")
                                , ("background", "#FBF9F5")
                                , ("bg borders", "#ECE5CE")
                                , ("boldest bg", "#FCF6EF")
                                , ("bold walls", "#F1D4AF")
                                , ("darkest bg", "#575757")
                                , ("big action", "#E08E79")]

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

    ".wrap" ? do
        position relative
        width (px 648)
        maxWidth (pct 100)
        minWidth (px 320)
        margin (px 0) auto (px 0) auto

    header ? do
        height (px 100)
        lineHeight (px 100)
        fontSize (px 20)
        textAlign (alignSide sideCenter)

    ".dashboard" ? do
        position relative
        marginBottom (px 20)

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

        ".left" ? float floatLeft

        ".right" ? float floatRight

        "span" ? do
            lineHeight (px 30)
            cursor pointer

        ".stop" ? display none

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
