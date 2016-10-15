module Main where

import qualified Network.Wai.Handler.Warp as Warp
import Server

main :: IO ()
main = Warp.run 3000 $ app
