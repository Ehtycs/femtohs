module GmshInterface where

{-
Module: GmshInterface
Description: Interface layer for  GMSH API.

Functions and data structures for reading and writing mesh and view data.
-}

import GmshAPI
import Element
import qualified Data.Map as M
import qualified Data.Vector as V
import Control.Monad (forM, forM_)
import TagTypes
import Domain
import Field
import Data.Maybe (fromMaybe)

-- Get a given list of indices from vect. 
mkNodeCoordinateStash :: V.Vector a -> [Int] -> [a]
mkNodeCoordinateStash vect inds = map (vect V.!) inds

reshape :: Int -> [a] -> [[a]]
reshape _ [] = []
reshape dim xs = take dim xs : reshape dim (drop dim xs)

type NodeData = V.Vector [Double]
type ElementData = [(DimTag, [(EntityTag, [Element])])]

-- Reads elements and nodes from GMSH and returns a hierarchy of
-- physical domains (with dimension), entities and elements to be further
-- processed.
gmshReadMeshData :: IO (NodeData, ElementData)
gmshReadMeshData = do

   (ntags, coords, _) <- gmshModelMeshGetNodes Nothing Nothing Nothing Nothing

   let ncoords = V.fromList $ reshape 3 coords
   let nodeCoordinateStash = mkNodeCoordinateStash ncoords

   out <- forM [1,2,3] $ \dim -> do
      -- fetch physical groups of dimension dim
      physGroups <- gmshModelGetPhysicalGroups $ Just dim
      pgroupsElements <- forM physGroups $ \(_, tag) -> do
         let jdim = Just dim
         -- get all geometrical entities and iterate over those
         etags <- gmshModelGetEntitiesForPhysicalGroup dim tag
         etagElements <- forM etags $ \etag -> do
            let jetag = Just etag
            -- get all element types inside this geometrical entity and
            -- iterate those...
            etypes <- gmshModelMeshGetElementTypes jdim jetag
            etypeElements <- forM etypes $ \etype -> do
               -- get the element tags and node tags of type etype
               -- inside the geometrical entity
               (elementTags, nodeTags) <-
                  gmshModelMeshGetElementsByType etype jetag Nothing Nothing
               -- construct the elements and shove them in a list
               let etype' = mkElementType etype
               let nnodes = numNodes etype'
               -- gmsh numbers nodes starting from 1, we start from 0
               let gnop = reshape nnodes $ map (\x -> x-1) nodeTags
               elements <- forM (zip elementTags gnop) $ \ (et, ntags) -> do
                  let ncord = nodeCoordinateStash ntags
                  return $ mkElement etype' et ntags ncord
               return elements
            -- just concatenate all different element types into one list
            -- no need to have a Map at this level
            return (EntityTag etag, concat etypeElements)
         -- build a Map , keys entity tags and value the list of elements
         -- inside that entity.
         return (DimTag (dim,tag), etagElements)
      -- Return the list of (dimTag, Map) pairs, since dimTag already contains
      -- the dimension, no need to have an extra layer
      return pgroupsElements

   return $ (ncoords, (concat out))

gmshExportField :: Domain -> Maybe String -> Field Double -> IO()
gmshExportField domain nameMaybe field = do
   let name = fromMaybe "" nameMaybe
   view <- gmshViewAdd name Nothing
   let (Field vals) = field
   let vals' = map (:[]) vals
   -- Gmsh numbers nodes from 1 , we do it from 0
   let nodetags = map (\(NodeTag n) -> n+1) $ domainNodes domain
   -- very cheap to start labeling STEPS FROM ZERO
   gmshViewAddModelData view 0 "" "NodeData" nodetags vals' Nothing (Just 1) Nothing
