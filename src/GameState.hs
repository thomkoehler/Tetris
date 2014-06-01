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

------------------------------------------------------------------------------------------------------------------------