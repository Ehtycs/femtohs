import GmshAPI

import Control.Monad (forM, forM_)
--import Control.Applicative
import Mesh
import Element
import Domain

import qualified Data.Map.Strict as M

makeElements :: Int -> [Int] -> [Int] -> [Element]
makeElements elementType elementTags nodeTags =
   zipWith (mkElement elemType) elementTags ntags
   where
      ntags = reshape (numNodes elemType) nodeTags
      elemType = mkElementType elementType

reshape :: Int -> [a] -> [[a]]
reshape _ [] = []
reshape dim xs = take dim xs : reshape dim (drop dim xs)

main :: IO ()
main = do
   gmshInitialize [] Nothing
   gmshModelOccAddRectangle 0 0 0 1 1 Nothing Nothing
   gmshModelOccAddRectangle 1 1 0 1 1 Nothing Nothing
   gmshModelOccAddRectangle (-1) (-1) 0 1 1 Nothing Nothing

   gmshModelOccSynchronize
   points <- gmshModelGetEntities (Just 0)
   gmshModelOccSetMeshSize points 1.0
   gmshModelOccSynchronize
   disk <- gmshModelAddPhysicalGroup 2 [1] Nothing
   disk <- gmshModelAddPhysicalGroup 2 [2] Nothing
   disk <- gmshModelAddPhysicalGroup 2 [3] Nothing
   gmshModelMeshGenerate $ Just 2

   (ntags, ncoords, _) <- gmshModelMeshGetNodes Nothing Nothing Nothing Nothing

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
               let gnop = reshape nnodes nodeTags
               elements <- forM (zip elementTags gnop) $ \ (et, ntags) -> do
                  return $ mkElement etype' et ntags
               return elements
            -- just concatenate all different element types into one list
            -- no need to have a Map at this level
            return (etag, concat etypeElements)
         -- build a Map , keys entity tags and value the list of elements
         -- inside that entity.
         return ((dim,tag), M.fromList etagElements)
      -- Return the list of (dimTag, Map) pairs, since dimTag already contains
      -- the dimension, no need to have an extra layer
      return pgroupsElements

   -- construct the final form :
   -- Map DimTag (Map EntityTag [Element])
   let hierarchy = M.fromList (concat out)

   print hierarchy

   case (M.lookup (1,1) hierarchy >>= M.lookup 1) of
      Just x -> forM_ x print
      Nothing -> print "Not found"


   -- forM (concat $ concat $ concat out) $ \(et, nts) -> do
   --    putStr "Etag: "
   --    print et
   --    putStrLn "Ntags: "
   --    print nts
   --    putStrLn ""

   --gmshFltkRun
   gmshFinalize
