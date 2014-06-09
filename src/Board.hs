------------------------------------------------------------------------------------------------------------------------

module Board
(
   Board, 
   Board.extent,
   Board.at,
   newEmptyBoard,
   mergeBordWithComponent
) where

import Data.Array.Repa as R
import Control.Lens

import Component

------------------------------------------------------------------------------------------------------------------------

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
at (Board a) (x, y) = toEnum $ a ! (Z :. y :. x)

 
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
               [c] = show $ board `Board.at` (x, y)   
           
           
mergeBordWithComponent :: Component -> Board -> Board
mergeBordWithComponent component (Board array) = Board $ computeUnboxedS $ R.traverse array id step
   where
      componentPositions = getAllPositions component
      componentType = component ^. cType
      step getFun pos@(Z :. y :. x) =
         if elem (x, y) componentPositions
            then fromEnum componentType
            else getFun pos


collision :: Board -> Component -> Bool
collision board component =
   foldl step False (zip [0..(width - 1)] [0..(height - 1)]) 
   where
      (width, height) = Board.extent board
      (posX, posY) = component ^. cPosition
      bitmap = componentBitmap (component ^. cType) $ component ^. cOrientation
      step :: Bool -> (Int, Int) -> Bool
      step coll (x, y) = coll || bitmap ! (Z :. y :. x) && (board  `Board.at` (posX + x, posY + y) == ctEmpty)   

   
transformComponent :: (Component -> Component) -> Board -> Component -> (Component, Bool)
transformComponent transformFun board component = if collision board newComponent
   then (component, True)
   else (newComponent, False)
   where
      newComponent = transformFun component


rotate :: Bool -> Board -> Component -> Component
rotate clockwise board component = res
   where
      (res, _) = transformComponent transformFun board component
      transformFun c = c & cOrientation %~ rotateOrientation clockwise
    

translation :: Bool -> Board -> Component -> Component
translation right board component = res
   where
      (res, _) = transformComponent transformFun board component
      inc n = n + 1
      dec n = n - 1
      operation = if right then inc else dec
      transformFun c = c & cPosition . _1 %~ operation


fall :: Board -> Component -> (Component, Bool) 
fall = transformComponent transformFun
   where
      transformFun c = c & cPosition . _2 %~ (+1)


------------------------------------------------------------------------------------------------------------------------
