module Main where

import GameState
import Component


main::IO()
main = do 
   let
      gs0 = newGameState (10, 20) ctZ
      (gs1, _) = nextStep MRight gs0
      (gs2, _) = nextStep MRight gs1
   print gs0
   print gs1
   print gs2
