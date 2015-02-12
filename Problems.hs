module Problems where

import Data.Map

type Test = (String, String)

data Problem = Problem { description :: String
                       , examples    :: String
                       , tests       :: [Test]
                       } deriving Show

problems :: [Problem]
problems = [ Problem "Create a function named 'myLast' that finds the last element of a list." "Prelude> myLast [1,2,3,4]\n4\nPrelude> myLast ['x','y','z']\n'z'" [("myLast [1,2,3]", "3")] ]
