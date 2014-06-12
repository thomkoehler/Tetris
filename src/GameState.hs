------------------------------------------------------------------------------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}

module GameState
(
   nextStep,
   Move(..),
   newGameState
)
where

import Control.Lens.TH
import Control.Lens

import Board
import Component

------------------------------------------------------------------------------------------------------------------------

data Move = MNone | MLeft | MRight | MRotClock | MRotUnclock deriving(Show, Eq)

data GameState = GameState
   {
      _gsBoard :: Board,
      _gsCurrentComponent :: Component
   }

makeLenses ''GameState


instance Show GameState where
   show (GameState board currComponent) = show $ mergeBordWithComponent currComponent board
   
   
newGameState :: (Int, Int) -> ComponentType -> GameState
newGameState extend@(w, _) componentType = GameState (newEmptyBoard extend) $ newComponent (w `quot` 2) componentType


nextStep :: Move -> GameState -> (GameState, Bool)
nextStep move gs@(GameState board currentComponent)
   | move == MLeft || move == MRight          = (gs & gsCurrentComponent %~ translation (move == MRight) board, False)
   | move == MRotClock || move == MRotUnclock = (gs & gsCurrentComponent %~ rotate (move == MRotClock) board, False)
   | otherwise                                = (gs { _gsCurrentComponent = component }, isDown)
      where
         (component, isDown) = fall board currentComponent 

------------------------------------------------------------------------------------------------------------------------