module Main where

import GameState
import Component
import Board

import System.Exit(exitSuccess)


main = test


test0 :: IO()
test0 = do 
   let
      gs0 = newGameState (10, 20) ctI
      (gs1, _) = nextStep MLeft gs0
      (gs2, _) = nextStep MRotClock gs1
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

   
test :: IO ()
test = do
   let
      gs = newGameState (10, 20) ctI
   gameLoop gs
   
   where
      gameLoop gameState = do
         print gameState
         (key : _) <- getLine
         let
            (nextGameState, exit) =
               case key of
                  'a' -> (fst $ nextStep MLeft gameState, False)
                  'd' -> (fst $ nextStep MRight gameState, False)
                  'w' -> (fst $ nextStep MRotUnclock gameState, False)
                  's' -> (fst $ nextStep MRotClock gameState, False)
                  ' ' -> (fst $ nextStep MNone gameState, False)
                  _   -> (gameState, True)
                  
         if exit 
            then exitSuccess 
            else gameLoop nextGameState 
            
   