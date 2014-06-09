------------------------------------------------------------------------------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}

module GameState
(
   nextStep,
   Move(..)
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
   

--TODO move :: Move -> GameState -> (GameState, Bool)
nextStep :: Move -> GameState -> (GameState, Bool)
nextStep move gs
   | move == MLeft || move == MRight = (gs & gsCurrentComponent %~ rotate (move == MRight), False)
   | otherwise                       = (gs, False)
   

------------------------------------------------------------------------------------------------------------------------