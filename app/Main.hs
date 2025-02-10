module Main where

import qualified MyLib (someFunc)

main :: IO ()
main = do
  putStrLn MyLib.someFunc
