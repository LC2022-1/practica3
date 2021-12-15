module Main where

import Logic.Propositions

main :: IO ()
main = print . fromInfix $ Var "alive"
