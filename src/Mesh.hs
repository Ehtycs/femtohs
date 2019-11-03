module Mesh where

import qualified Data.Map as M
import qualified Data.Vector as V
import Element
import TagTypes

type MeshHierarchy = M.Map DimTag (M.Map EntityTag [Element])

data Mesh = Mesh
   { meshHierarchy :: [MeshHierarchy]
   , meshNodes :: V.Vector [Double]
   } deriving (Eq, Show)
