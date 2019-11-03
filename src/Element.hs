module Element where

import qualified Data.Vector as V
import TagTypes

type R = Double

-- type ElementTag = Int
-- type NodeTag = Int


--data Node = Node Tag [R] deriving (Show, Eq)

data ElementType = Line | Triangle | Tetrahedron deriving (Show, Eq)

data Element = Element
   { elementType :: ElementType
   , elementTag :: ElementTag
   , elementNodeTags :: [NodeTag]
   , elementNodes :: [[R]]
   } deriving (Eq)

instance Show Element where
   show el = show (elementType el)++ "("++ show (elementTag el) ++ "): " ++
             show (elementNodeTags el) ++
             show (elementNodes el)

numNodes :: ElementType -> Int
numNodes et = case et of
   Line -> 2
   Triangle -> 3
   Tetrahedron -> 4

mkElement :: ElementType -> Int -> [Int] -> [[R]] -> Element
mkElement tp tag ntags ncoord =
   -- let tp' = mkElementType tp
   Element tp (ElementTag tag) (map NodeTag ntags) ncoord

mkElementType :: Int -> ElementType
mkElementType n = case n of
   1 -> Line
   2 -> Triangle
   _ -> error $ "Element type " ++ show n ++ " not implemented"
