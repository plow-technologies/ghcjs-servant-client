module Main where

import Test.Hspec

main :: IO ()
main = do
  print "Hello from test"
  fail "it failed"
