
------------------------------------------------------------------------------------------------------------------------

{-# LANGUAGE TemplateHaskell, TypeOperators, GeneralizedNewtypeDeriving, StandaloneDeriving #-}

module Component
(
   Component,
   Position,
   ComponentType, ctEmpty, ctI, ctJ, ctL, ctS, ctT, ctZ, ctO, 
   cPosition, cType, cOrientation,
   newComponent,
   Orientation(..), 
   rotateOrientation,
   getAllPositions,
   componentBitmap
) 
where


import Data.Array.Repa as R
import Control.Lens.TH
import Control.Lens
import System.Random
   
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


instance Random ComponentType where
   random gen = 
      let
         (i,newGen) = randomR (fromEnum ctI, fromEnum ctO) gen
      in
         (ComponentType i, newGen)
         
   randomR (fromR, toR) gen =
      let
         (i,newGen) = randomR (fromEnum fromR, fromEnum toR) gen
      in
         (ComponentType i, newGen)


data Orientation = Or0 | Or90 | Or180 | Or270  deriving(Enum, Show)

type Position = (Int, Int)

data Component = Component
   {
      _cPosition :: !Position,
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
componentBitmap ct orientation
   | ct == ctI = [ctI0, ctI1, ctI0, ctI1] !! fromEnum orientation
   | ct == ctJ = [ctJ0, ctJ1, ctJ2, ctJ3] !! fromEnum orientation
   | ct == ctL = [ctL0, ctL1, ctL2, ctL3] !! fromEnum orientation
   | ct == ctS = [ctS0, ctS1, ctS0, ctS1] !! fromEnum orientation
   | ct == ctT = [ctT0, ctT1, ctT2, ctT3] !! fromEnum orientation
   | ct == ctZ = [ctZ0, ctZ1, ctZ0, ctZ1] !! fromEnum orientation
   | ct == ctO = ctO0
   | otherwise = error "Unknown ComponentType encountered in componentBitmap."

   
newComponent :: Int -> ComponentType -> Component
newComponent posX ct = Component (posX, 0) ct Or0


rotateOrientation :: Bool -> Orientation -> Orientation
rotateOrientation True Or270 = Or0
rotateOrientation True o = succ o
rotateOrientation False Or0 = Or270
rotateOrientation False o = pred o

 
getAllPositions :: Component -> [Position]
getAllPositions component = foldl step [] [(x,y) | x <- [0..w - 1], y <- [0..h - 1]]
   where
      bitmap = componentBitmap (component ^. cType) $ component ^. cOrientation  
      (Z :. h :. w) = R.extent bitmap
      (posX, posY) = component ^. cPosition
      step prev (x,y) = if bitmap ! (Z :. y :. x) then (posX + x, posY + y) : prev else prev   


------------------------------------------------------------------------------------------------------------------------