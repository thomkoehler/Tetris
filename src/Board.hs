------------------------------------------------------------------------------------------------------------------------

{-# LANGUAGE TypeOperators, GeneralizedNewtypeDeriving, StandaloneDeriving #-}

module Board
(
   Board, 
   ComponentType, ctEmpty, ctI, ctJ, ctL, ctS, ctT, ctZ, ctO,
   Board.extent,
   at,
   newEmptyBoard
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


newEmptyBoard :: (Int, Int) -> Board
newEmptyBoard (w, h) = Board $ fromListUnboxed (Z :. h :. w) $ replicate (w * h) $ fromEnum ctEmpty 


extent :: Board -> (Int, Int)
extent (Board a) = 
   let
      (Z :. h :. w) = R.extent a
   in
      (w, h)

      
at :: Board -> (Int, Int) -> ComponentType
at (Board a) (x, y) = ComponentType $ a ! (Z :. y :. x)

 
instance Show Board where
   show board = top Prelude.++ ground
      where
         top = reverse $ foldl step "" [(x,y) | y <- [0..h - 1], x <- [0..w - 1]]
         ground = replicate (w + 2) '#'
         (w, h) = Board.extent board
         step prev (x, y) = 
            if x == 0
               then c : '#' : prev
               else if x == w - 1
                  then '\n' : '#' : c : prev
                  else c : prev
            
            where
               [c] = show $ board `at` (x, y)   
           

mergeBordWithComponent :: Component -> Board -> Board
mergeBordWithComponent = computeUnboxedS unboxed
   where
       

{--


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

--}

------------------------------------------------------------------------------------------------------------------------
