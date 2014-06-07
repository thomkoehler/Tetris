module Main where

import Component
import Board

--c0 = Component (2, 3) CtZ Or90


main::IO()
main = do 
   let
      c = newComponent 5 ctZ
      b = newEmptyBoard (10, 20)
   print b
