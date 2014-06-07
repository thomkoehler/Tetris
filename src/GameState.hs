------------------------------------------------------------------------------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}

module GameState where

import Control.Lens.TH
import Control.Lens

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
   show gameState = show $ mergeBoardAndComponent gameState


mergeBoardAndComponent :: GameState -> Board
mergeBoardAndComponent gameState = newBoard (w, h) components 
   where
      board = gameState ^. gsBoard
      (w, h) = extent board
      componentType = gameState ^. gsCurrentComponent . cType
      componentPositions = getAllPositions $ gameState ^. gsCurrentComponent 
      components = Prelude.reverse $ foldl step [] [(x,y) | x <- [0..w - 1], y <- [0..h - 1]]
      step prev pos = if elem pos componentPositions
         then componentType : prev
         else board `Board.at` pos : prev

------------------------------------------------------------------------------------------------------------------------