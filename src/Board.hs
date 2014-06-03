------------------------------------------------------------------------------------------------------------------------

{-# LANGUAGE TypeOperators #-}

module Board
(
   Board, 
   ComponentType(..), 
   Board.extent,
   at
) where

import Data.Array.Repa as R

------------------------------------------------------------------------------------------------------------------------

data ComponentType = CtI | CtJ | CtL | CtS | CtT | CtZ | CtO
   deriving(Enum, Show)

newtype Board = Board (Array D DIM2 (Maybe ComponentType))


extent :: Board -> (Int, Int)
extent (Board a) = 
   let
      (Z :. h :. w) = R.extent a
   in
      (w, h)
      
at :: Board -> (Int, Int) -> Maybe ComponentType
at (Board a) (x, y) = a ! (Z :. y :. x)

------------------------------------------------------------------------------------------------------------------------