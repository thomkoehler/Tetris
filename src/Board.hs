------------------------------------------------------------------------------------------------------------------------

{-# LANGUAGE TypeOperators, GeneralizedNewtypeDeriving, StandaloneDeriving #-}

module Board
(
   Board, 
   ComponentType, ctEmpty, ctI, ctJ, ctL, ctS, ctT, ctZ, ctO,
   Board.extent,
   at,
   newBoard
) where

import Data.Array.Repa as R

------------------------------------------------------------------------------------------------------------------------

newtype ComponentType = ComponentType Int deriving (Eq, Ord, Enum)
(ctEmpty:ctI:ctJ:ctL:ctS:ctT:ctZ:ctO:_) = [ComponentType 0..]

ctEmpty, ctI, ctJ, ctL, ctS, ctT, ctZ, ctO :: ComponentType


instance Show ComponentType  where
   show ct 
      | ct == ctEmpty = "."
      | ct == ctI = "I"
      | ct == ctJ = "J"
      | ct == ctL = "L"
      | ct == ctS = "S"
      | ct == ctT = "T"
      | ct == ctZ = "Z"
      | ct == ctO = "O"
      | otherwise = error "Unknown ComponentType encountered in show ComponentType."


newtype Board = Board (Array U DIM2 Int)


newBoard :: (Int, Int) -> [ComponentType] -> Board
newBoard (w, h) list = Board $ fromListUnboxed (Z :. h :. w) $ Prelude.map fromEnum list


extent :: Board -> (Int, Int)
extent (Board a) = 
   let
      (Z :. h :. w) = R.extent a
   in
      (w, h)

      
at :: Board -> (Int, Int) -> ComponentType
at (Board a) (x, y) = ComponentType $ a ! (Z :. y :. x)


instance Show Board where
   show board = reverse $ foldl step "" [(x,y) | x <- [0..w - 1], y <- [0..h - 1]]
      where
         (w, h) = Board.extent board
         step prev pos = 
            let
               [c] = show $ board `at` pos
            in
               c : prev

------------------------------------------------------------------------------------------------------------------------