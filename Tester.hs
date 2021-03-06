module Tester where

import LazyCrossCheck

main :: IO ()
main = test

test :: IO ()
test = lazyCrossCheck 5 "version" $
        (version1 --> version2) `with` [ ints ==> [1,2,3] ]

version1 :: Maybe Int -> Int
version1 Nothing = 1
version1 (Just x) = x

version2 :: Maybe Int -> Int
version2 Nothing = 1
version2 (Just _) = 2
