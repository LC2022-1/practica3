module Main where

import Test.DocTest

main :: IO ()
main = do
  putStrLn "Testing comment examples\n"
  putStrLn "Testing Propostions: " *> doctest ["src/Logic/Propositions.hs"]
  putStrLn "Testing Resolution: " *> doctest ["src/Logic/Resolution.hs"]
  putStrLn "Testing 3CNF encoding: " *> doctest ["src/Logic/ThreeCNF.hs"]
  putStrLn "Testing Tseitin encoding: " *> doctest ["src/Logic/Tseitin.hs"]
