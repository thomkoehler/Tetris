
------------------------------------------------------------------------------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}

module Component where


import Data.Array.Repa
import Data.Maybe(isJust)
import Control.Lens.TH
import Control.Lens
   
import Board as B

------------------------------------------------------------------------------------------------------------------------

data Orientation = Or0 | Or90 | Or180 | Or270  deriving(Enum, Show)

data Component = Component
   {
      _cPosition :: !(Int, Int),
      _cType :: !ComponentType,
      _cOrientation :: !Orientation 
   }
   
makeLenses ''Component

   
type Bitmap = Array U DIM2 Bool


ctO0 :: Bitmap
ctO0 = fromListUnboxed (Z :. 2 :. 2) [True, True,
                                      True, True]
                                      
ctI0 :: Bitmap
ctI0 = fromListUnboxed (Z :. 4 :. 4) [False, False ,False, False,
                                      True, True, True, True, 
                                      False, False, False, False, 
                                      False, False, False, False]
                                      
ctI1 :: Bitmap
ctI1 = fromListUnboxed (Z :. 4 :. 4) [False, False ,True, False,
                                      False, False, True, False, 
                                      False, False, True, False, 
                                      False, False, True, False]                          

ctJ0 :: Bitmap
ctJ0 = fromListUnboxed (Z :. 3 :. 3) [False, False ,False,
                                      True, True, True, 
                                      False, False, True] 
 
ctJ1 :: Bitmap
ctJ1 = fromListUnboxed (Z :. 3 :. 3) [False, True ,False,
                                      False, True, False, 
                                      True, True, False]
                                     
ctJ2 :: Bitmap
ctJ2 = fromListUnboxed (Z :. 3 :. 3) [False, False ,False,
                                      True, False, False, 
                                      True, True, True]
                                      
ctJ3 :: Bitmap
ctJ3 = fromListUnboxed (Z :. 3 :. 3) [False, True ,True,
                                      False, True, False, 
                                      False, True, False]                                      

ctL0 :: Bitmap
ctL0 = fromListUnboxed (Z :. 3 :. 3) [False, False ,False,
                                      True, True, True, 
                                      True, False, False] 
 
ctL1 :: Bitmap
ctL1 = fromListUnboxed (Z :. 3 :. 3) [True, True ,False,
                                      False, True, False, 
                                      False, True, False]
                                     
ctL2 :: Bitmap
ctL2 = fromListUnboxed (Z :. 3 :. 3) [False, False ,False,
                                      False, False, True, 
                                      True, True, True]
                                      
ctL3 :: Bitmap
ctL3 = fromListUnboxed (Z :. 3 :. 3) [False, True ,False,
                                      False, True, False, 
                                      False, True, True]                                      

ctS0 :: Bitmap
ctS0 = fromListUnboxed (Z :. 3 :. 3) [False, False ,False,
                                      False, True, True, 
                                      True, True, False] 
 
ctS1 :: Bitmap
ctS1 = fromListUnboxed (Z :. 3 :. 3) [True, False, False,
                                      True, True, True, 
                                      False, False, True]
                                     
ctT0 :: Bitmap
ctT0 = fromListUnboxed (Z :. 3 :. 3) [False, False ,False,
                                      True, True, True, 
                                      False, True, False] 
 
ctT1 :: Bitmap
ctT1 = fromListUnboxed (Z :. 3 :. 3) [False, True, False,
                                      True, True, False, 
                                      False, True, False]
                                     
ctT2 :: Bitmap
ctT2 = fromListUnboxed (Z :. 3 :. 3) [False, False ,False,
                                      False, True, False, 
                                      True, True, True]
                                      
ctT3 :: Bitmap
ctT3 = fromListUnboxed (Z :. 3 :. 3) [False, True, False,
                                      False, True, True, 
                                      False, True, False]                                      

ctZ0 :: Bitmap
ctZ0 = fromListUnboxed (Z :. 3 :. 3) [False, False ,False,
                                      True, True, False, 
                                      False, True, True] 
 
ctZ1 :: Bitmap
ctZ1 = fromListUnboxed (Z :. 3 :. 3) [False, False, True,
                                      False, True, True, 
                                      False, True, False]

componentBitmap :: ComponentType -> Orientation -> Bitmap
componentBitmap Ct_I orientation = [ctI0, ctI1, ctI0, ctI1] !! fromEnum orientation
componentBitmap Ct_J orientation = [ctJ0, ctJ1, ctJ2, ctJ1] !! fromEnum orientation
componentBitmap Ct_L orientation = [ctL0, ctL1, ctL2, ctL1] !! fromEnum orientation
componentBitmap Ct_S orientation = [ctS0, ctS1, ctS0, ctS1] !! fromEnum orientation
componentBitmap Ct_T orientation = [ctT0, ctT1, ctT2, ctT1] !! fromEnum orientation
componentBitmap Ct_Z orientation = [ctZ0, ctZ1, ctZ0, ctZ1] !! fromEnum orientation
componentBitmap Ct_O _ = ctO0


rotateOrientation :: Bool -> Orientation -> Orientation
rotateOrientation True Or270 = Or0
rotateOrientation True o = succ o
rotateOrientation False Or0 = Or270
rotateOrientation False o = pred o

 
collision :: Board -> Component -> Bool
collision board component =
   foldl fun False (zip [0..(width - 1)] [0..(height - 1)]) 
   where
      (width, height) = B.extent board
      (posX, posY) = component ^. cPosition
      bitmap = componentBitmap (component ^. cType) $ component ^. cOrientation
      fun :: Bool -> (Int, Int) -> Bool
      fun coll (x, y) = coll || bitmap ! (Z :. y :. x) && isJust (board  `B.at` (posX + x, posY + y))   

   
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
translation right board component = undefined

fall :: Board -> Component -> (Bool, Component) 
fall = error "fall not impelemented"

------------------------------------------------------------------------------------------------------------------------