module Problems where

import Data.Map

type Test = (String, String)

data Problem = Problem { description :: String
                       , hint        :: String
                       , tests       :: [Test]
                       } deriving Show

problems :: [Problem]
problems = [ Problem
                 "Create a function named 'myLast' that finds the last element of a list."
                 "myLast :: [a] -> a"
                 [("myLast [1,2,3]", "3")]
           , Problem
                 "Create a function named 'myButLast' that finds the second last element of a list."
                 "myButLast :: [a] -> a"
                 [("myButLast [1,2,3,4]", "3")] ]
