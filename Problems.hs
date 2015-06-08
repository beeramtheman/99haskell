module Problems where

import           Data.Map

type Test = (String, String)

data Problem = Problem { description :: String
                       , hint        :: String
                       , tests       :: [Test]
                       } deriving Show

problems :: [Problem]
problems = [ Problem -- 1
                 "Make a function named 'myLast' that finds the last element of a list."
                 "-- Welcome to 99 Haskell! Solve H-99[0] problems live in the browser to\n\
                 \-- strengthen your understanding of the language. Help out on GitHub[1].\n\
                 \-- [0] https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems\n\
                 \-- [1] https://github.com/bramgg/99haskell\n\n\
                 \myLast :: [a] -> a"
                 [ ("myLast [1,2,3,4]", "4")
                 , ("myLast ['x','y','z']", "'z'") ]

           , Problem -- 2
                 "Make a function named 'myButLast' that finds the second last element of a list."
                 "myButLast :: [a] -> a"
                 [ ("myButLast [1,2,3,4]", "3")
                 , ("myButLast ['a'..'z']", "'y'") ]

           , Problem -- 3
                 "Make a function named 'elementAt' that finds the element of a list at a specific 1-based index."
                 "elementAt :: [a] -> Int -> a"
                 [ ("elementAt [1,2,3] 2", "2")
                 , ("elementAt \"haskell\" 5", "'e'") ]

           , Problem -- 4
                 "Make a function named 'myLength' that finds the number of elements in a list."
                 "myLength :: [a] -> Int"
                 [ ("myLength [123, 456, 789]", "3")
                 , ("myLength \"Hello, World!\"", "13") ]

           , Problem -- 5
                "Make a function named 'myReverse' that reverses a list."
                "myReverse :: [a] -> [a]"
                [ ("myReverse \"A man, a plan, a canal, panama!\"", "\"!amanap ,lanac a ,nalp a ,nam A\"")
                , ("myReverse [1,2,3,4]", "[4,3,2,1]") ]

           , Problem -- 6
                "Make a function named 'isPalindrome' that finds whether a list is a palindrome (is the same read forward and backward)."
                "isPalindrome :: (Eq a) => [a] -> Bool"
                [ ("isPalindrome [1,2,3,1]", "False")
                , ("isPalindrome \"madamimadam\"", "True")
                , ("isPalindrome [1,2,4,8,16,8,4,2,1]", "True") ]

           , Problem -- 7
                "Make a function named 'flatten' that flattens a nested list structure."
                "data NestedList a = Elem a | List [NestedList a]\n\
                \flatten :: NestedList a -> [a]"
                [ ("flatten (Elem 5)", "[5]")
                , ("flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])", "[1,2,3,4,5]")
                , ("flatten (List [])", "[]") ]

           , Problem -- 8
                "Make a function named 'compress' removes consecutive duplicates of elements in a list without affecting the order."
                "compress :: Eq a => [a] -> [a]"
                [ ("compress \"aaaabccaadeeee\"", "\"abcade\"")
                , ("compress [1,1,2,3,1,2,2,3]", "[1,2,3,1,2,3]") ]

           , Problem -- 9
                "Make a function named 'pack' that packs consecutive duplicates and standalone elements into sublists."
                "pack :: Eq a => [a] -> [[a]]"
                [ ("pack \"aaaabccaadeeee\"", "[\"aaaa\",\"b\",\"cc\",\"aa\",\"d\",\"eeee\"]")
                , ("pack [1,1,2,3,1,2,2,3]", "[[1,1],[2],[3],[1],[2,2],[3]]") ]

           , Problem -- 10
                "Make a function named 'encode' that implements run-length encoding on a list. That is, consecutive duplicates and standalone elements are replaced with tuples of the format (number of occurences of element, element itself)."
                "encode :: Eq a => [a] -> [(Int, a)]"
                [ ("encode \"aaaabccaadeeee\"", "[(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]")
                , ("encode [1,1,2,3,1,2,2,3]", "[(2,1),(1,2),(1,3),(1,1),(2,2),(1,3)]") ]
           , Problem -- 11
                "Make a function named 'encodeModified' that implements a modified run-length encoding on a list. Instead of encoding the result as a tuple, use the provided ListItem."
                "data ListItem a = Single a | Multiple Int a\n\
                \  deriving Show\n\
                \encodeModified :: Eq a => [a] -> [ListItem a]"
                [ ("encodeModified \"aaaabccaadeeee\"", "[Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']") ]
            , Problem -- 12
                "Make a function named 'decodeModified' that decodes a modified run-length encoding, implemented in Problem 11."
                "data ListItem a = Single a | Multiple Int a\n\
                \  deriving Show\n\
                \decodeModified :: [ListItem a] -> [a]"
                [ ("decodeModified \"[Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e']\"", "\"aaaabccaadeeee\"") ]
            , Problem -- 13
                "Make a function named 'encodeDirect' that implements the run-length encoding directly. That is, without first generating sub-lists."
                "data ListItem a = Single a | Multiple Int a\n\
                \  deriving Show\n\
                \encodeDirect :: Eq a => [a] -> [ListItem a]"
                [ ("encodeDirect \"aaaabccaadeeee\"", "[Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']") ]
            , Problem -- 14
                "Make a function named 'dupli' that duplicates the elements of a list."
                "dupli :: [a] -> [a]"
                [ ("dupli \"abccd\"", "\"aabbccccdd\"" ) ]
            , Problem -- 15
                "Make a function named 'repli' that replicates the elements of a list a given numer of times."
                "repli :: [a] -> Int -> [a]"
                [ ("repli \"abc\" 3", "\"aaabbbccc\"" ) ]
            , Problem -- 16
                "Make a function named 'dropEvery' that drops every nth element of a list"
                "dropEvery :: [a] -> Int -> [a]"
                [ ("dropEvery \"abcdefghijk\" 3", "\"abdeghjk\"" ) ]
            , Problem -- 17
                "Make a function named 'split' which splits a list in two parts, where the index of the split is given."
                "split :: [a] -> Int -> ([a], [a])"
                [ ("split \"abcdefghijk\" 3", "(\"abc\",\"defghijk\")") ]
            ]

