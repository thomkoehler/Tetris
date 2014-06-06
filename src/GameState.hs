------------------------------------------------------------------------------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}

module GameState where

import Control.Lens.TH

import Board
import Component

------------------------------------------------------------------------------------------------------------------------


data GameState = GameState
   {
      _gsBoard :: Board,
      _gsCurrentComponent :: Component
   }

makeLenses ''GameState


instance Show GameState where
   show gs = undefined


mergeBoardAndComponent :: GameState -> Board
mergeBoardAndComponent gameState = newBoard (w, h) components 
   where
      board = gameState ^. gsBoard
      (w, h) = extent board
      componentType = gameState ^. _gsCurrentComponent . _cType
      componentPositions = getAllPositions $ gameState ^. gsCurrentComponent 
      components = revers $ foldl step [] [(x,y) | x <- [0..w - 1], y <- [0..h - 1]]
      step prev (x,y) = if elem (x,y) componentPositions
         then componentType
         else board `at` (x,y) 

------------------------------------------------------------------------------------------------------------------------