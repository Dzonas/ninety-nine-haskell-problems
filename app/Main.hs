module Main where

import qualified NinetyNine (someFunc)

main :: IO ()
main = do
  putStrLn NinetyNine.someFunc
