module Main where

import GameState
import Component
import Board


main = test0


test0 :: IO()
test0 = do 
   let
      gs0 = newGameState (10, 20) ctI
      (gs1, _) = nextStep MLeft gs0
      (gs2, _) = nextStep MLeft gs1
   print gs0
   print gs1
   print gs2

test1 = do
   let 
      c0 = newComponent 5 ctI
      pos0 = getAllPositions c0
      b0 = newEmptyBoard (10, 20)
      mb0 = mergeBordWithComponent c0 b0
   
   print pos0
   print mb0
