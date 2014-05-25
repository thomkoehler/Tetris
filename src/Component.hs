
------------------------------------------------------------------------------------------------------------------------

module Component where


import Data.Array.Repa
   
import Board(Board(..))

------------------------------------------------------------------------------------------------------------------------

data Component = Component
   {
      cPosition :: !(Int, Int),
      cType :: !ComponentType,
      cOrientation :: !Orientation 
   }


data ComponentType = Ct_I | Ct_J | Ct_L | Ct_S | Ct_T | Ct_Z | Ct_O
   deriving(Enum, Show)

   
data Orientation = Or0 | Or90 | Or180 | Or270 
   deriving(Enum, Show)

   
type Bitmap = Array U DIM2 Int


ctO0 :: Bitmap
ctO0 = fromListUnboxed (Z :. 4 :. 4) [0, 0 ,0, 0,
                                      0, 1, 1, 0, 
                                      0, 1, 1, 0, 
                                      0, 0, 0, 0]
                                      
ctI0 :: Bitmap
ctI0 = fromListUnboxed (Z :. 4 :. 4) [0, 0 ,0, 0,
                                      1, 1, 1, 1, 
                                      0, 0, 0, 0, 
                                      0, 0, 0, 0]
                                      
ctI1 :: Bitmap
ctI1 = fromListUnboxed (Z :. 4 :. 4) [0, 0 ,1, 0,
                                      0, 0, 1, 0, 
                                      0, 0, 1, 0, 
                                      0, 0, 1, 0]                          

ctJ0 :: Bitmap
ctJ0 = fromListUnboxed (Z :. 3 :. 3) [0, 0 ,0,
                                      1, 1, 1, 
                                      0, 0, 1] 
 
ctJ1 :: Bitmap
ctJ1 = fromListUnboxed (Z :. 3 :. 3) [0, 1 ,0,
                                      0, 1, 0, 
                                      1, 1, 0]
                                     
ctJ2 :: Bitmap
ctJ2 = fromListUnboxed (Z :. 3 :. 3) [0, 0 ,0,
                                      1, 0, 0, 
                                      1, 1, 1]
                                      
ctJ3 :: Bitmap
ctJ3 = fromListUnboxed (Z :. 3 :. 3) [0, 1 ,1,
                                      0, 1, 0, 
                                      0, 1, 0]                                      
                                       



 

rotate :: Board -> Component -> Component
rotate = error "rotate not impelemented"

translation :: Board -> Component -> Component
translation = error "translation not impelemented"

fall :: Board -> Component -> (Bool, Component) 
fall = error "fall not impelemented"

------------------------------------------------------------------------------------------------------------------------