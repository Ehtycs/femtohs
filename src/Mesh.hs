module Mesh where

import qualified Data.Map as M
import qualified Data.Vector as V
import Element
import TagTypes

type MeshHierarchy = M.Map DimTag (M.Map EntityTag [Element])

type NodeData = V.Vector [Double]
type ElementData = [(DimTag, [(EntityTag, [Element])])]

data Mesh = Mesh
   { meshHierarchy :: MeshHierarchy
   , meshNodes :: NodeData
   } deriving (Eq, Show)


mkMesh :: (NodeData, ElementData) -> Mesh
mkMesh (nodes, pgroups) =
   Mesh { meshHierarchy = hierarchy, meshNodes = nodes }
   where
      hierarchy =
         M.fromList $ map (\(dt, etl) ->  (dt, M.fromList etl)) pgroups

meshGetPhysical dt mesh =
   M.lookup dt (meshHierarchy mesh)

meshGetEntityOf dt ent mesh = do
   physgroup <- M.lookup dt (meshHierarchy mesh)
   M.lookup ent physgroup

defineDomain :: [DimTag] -> Mesh -> [Element]
defineDomain dts mesh' =
   concat $ map lookupPhysgroupOrError dts
   where
      mesh = meshHierarchy mesh'
      lookupPhysgroupOrError dt =
         case (M.lookup dt mesh) of
            Just physGroup -> concat $ M.elems physGroup
            Nothing -> error $ "Physical group " ++ show dt ++ " not found!"


      -- elements = map
      --    (\em -> case(em) of
      --       Just emap -> concat $ M.elems emap
      --       Nothing -> error "Physical group " ++ show
