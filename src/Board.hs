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

data ComponentType = Ct_I | Ct_J | Ct_L | Ct_S | Ct_T | Ct_Z | Ct_O
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