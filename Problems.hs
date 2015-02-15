module Problems where

import Data.Map

type Test = (String, String)

data Problem = Problem { description :: String
                       , hint        :: String
                       , tests       :: [Test]
                       } deriving Show

problems :: [Problem]
problems = [ Problem
                 "Make a function named 'myLast' that finds the last element of a list, assuming all lists are of length >= 1."
                 "myLast :: [a] -> a"
                 [ ("myLast [1,2,3]", "3")
                 , ("myLast ['x','y','z','q']", "'q'")
                 , ("myLast [\"foo\"]", "\"foo\"") ]

           , Problem
                 "Make a function named 'myButLast' that finds the second last element of a list, assuming all lists are of length >= 2."
                 "myButLast :: [a] -> a"
                 [ ("myButLast [1,2,3,4]", "3")
                 , ("myButLast ['a'..'z']", "'y'")
                 , ("myButLast \"qwerty\"", "'t'") ] ]
