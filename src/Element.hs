module Element where

import qualified Data.Vector as V
import TagTypes
import Data.List (transpose) -- temporary

import qualified Data.Matrix as Mat

type R = Double

type Vector = Mat.Matrix R
type Covector = Mat.Matrix R

type Jacobian = Mat.Matrix R

mkVector n elems = Mat.fromList n 1 elems
mkCovector n elems = Mat.fromList 1 n elems


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
nodeScalarBasis (Element elementType _ _ _) i pnt
   = case (elementType) of
      Triangle ->
         let
            [pu, pv] = pnt
         in
            case i of
               0 -> 1 - pu - pv
               1 -> pu
               2 -> pv
      Line ->
         let
            [pu] = pnt
         in
            case i of
               0 -> 1 - pu
               1 -> pu

-- Derivatives of scalar basis functions in elements
nodeScalarBasisDiff :: Element -> Int -> [R] -> Covector
nodeScalarBasisDiff (Element elementType _ _ _) i pnt
   = case (elementType) of
      Triangle ->
         let
            [pu, pv] = pnt
         in
            case i of
               0 -> mkCovector 2 [-1,1]
               1 -> mkCovector 2 [1,0]
               2 -> mkCovector 2 [0,1]
      Line ->
         let
            [pu] = pnt
         in
            case i of
               0 -> mkCovector 1 [-1]
               1 -> mkCovector 1 [1]

isopJacobian :: Element -> [R] -> Mat.Matrix R
isopJacobian element pnt =
   case (elementType element) of
      Triangle ->
            -- jacobian is doox / doou and x is same for column
            -- row major ordering [[e11, e12, e13], [e21, e22, e23]] etc...
            Mat.multStd dNs ns
            where
               ns = Mat.fromLists $ elementNodes element -- 3 x 3
               bf i = nodeScalarBasisDiff element i pnt
               -- dNs = Mat.fromLists $
               --    map (\i -> nodeScalarBasisDiff element i pnt) [0,1,2]
               dNs = Mat.transpose $ bf 0 Mat.<-> bf 1 Mat.<-> bf 2 -- 2 x 3
