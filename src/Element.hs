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


-- integration scheme
-- outputs points, weigths
gaussIntegrationScheme :: Element -> Int -> [([Double], Double)]
gaussIntegrationScheme element rank =
   case (elementType element) of
      Triangle ->
         case rank of
            1 -> [([1/3, 1/3] , 0.5)]
            2 -> [([1/6, 1/6], 1/3)
                 ,([4/6, 1/6], 1/3)
                 ,([1/6, 4/6], 1/3)]
      Line ->
         case rank of
            1 -> [([1/2] , 1)]
            -- TODO : Find a few more digits for these
            2 -> [([0.788675], 0.5)
                 ,([0.211325], 0.5)]



-- Scalar basis functions in elements
nodeScalarBasis :: Element -> Int -> [R] -> R
nodeScalarBasis (Element elementType _ _ nodes) i pnt
   = case (elementType) of
      Triangle ->
         let
            [n1,n2,n3] = nodes
            [pu, pv] = pnt
         in
            case i of
               0 -> 1 - pu - pv
               1 -> pu
               2 -> pv
      Line ->
         let
            [n1,n2] = nodes
            [pu] = pnt
         in
            case i of
               0 -> 1 - pu
               1 -> pu
