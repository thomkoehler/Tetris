module Main where

import Component
import Board

c0 = Component (2, 3) CtZ Or90


main::IO()
main = do 
   putStrLn . show $ getAllPositions c0