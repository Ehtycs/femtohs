module Element where

type R = Double
type ElementTag = Int
type NodeTag = Int

--data Node = Node Tag [R] deriving (Show, Eq)

data ElementType = Line | Triangle | Tetrahedron deriving (Show, Eq)

data Element = Element
   { elementType :: ElementType
   , elementTag :: ElementTag
   , elementNodes :: [NodeTag]
   } deriving (Eq)

instance Show Element where
   show el = show (elementType el)++ "("++ show (elementTag el) ++ "): " ++ 
             show (elementNodes el)

numNodes :: ElementType -> Int
numNodes et = case et of
   Line -> 2
   Triangle -> 3
   Tetrahedron -> 4

mkElement :: ElementType -> Int -> [Int] -> Element
mkElement tp tag ntags =
   -- let tp' = mkElementType tp
   Element tp tag ntags

mkElementType :: Int -> ElementType
mkElementType n = case n of
   1 -> Line
   2 -> Triangle
   _ -> error $ "Element type " ++ show n ++ " not implemented"
