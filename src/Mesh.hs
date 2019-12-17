module Mesh where

import qualified Data.Map as M
import qualified Data.Vector as V
import Element
import TagTypes
import Domain
import Data.Containers.ListUtils (nubOrd)

import Debug.Trace

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

getNodesByTag :: NodeData -> [NodeTag] -> [[Double]]
getNodesByTag nds tags =
   map (nds V.!) intTags
      where
         intTags = map (\(NodeTag tag) -> tag) tags

defineDomain :: [DimTag] -> Mesh -> Domain
defineDomain dts mesh' =
   Domain elements uniqueNodes nodes
   where
      mesh = meshHierarchy mesh'
      elements = concat $ map lookupPhysgroupOrError dts
      nodes = getNodesByTag (meshNodes mesh') uniqueNodes
      uniqueNodes = nubOrd $ concat $ map elementNodeTags elements
      lookupPhysgroupOrError dt =
         case (M.lookup dt mesh) of
            Just physGroup ->
               concat $ M.elems physGroup
            Nothing -> error $ "Physical group " ++ show dt ++ " not found!"
