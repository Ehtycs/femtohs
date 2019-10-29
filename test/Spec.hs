import GmshAPI

import Control.Monad (forM)

import Mesh
import Element
import Domain

numNodes :: Int -> Int
numNodes etype =
   case etype of
      1 -> 2 -- 2-node line
      2 -> 3 -- 3-node triangle
      4 -> 4 -- 4-node tetrahedron

mkElement :: (Element a) => Int -> a
mkElement 1 tag [a,b] = Line tag (a,b)
mkElement 2 tag [a,b,c] = Triangle tag (a,b,c)


mkElements etype etags nodes =
   map fun (zip etags (reshape (numNodes etype) nodes))
   where
      fun (etag, nodes) = mkElement etype etag nodes

reshape :: Int -> [a] -> [[a]]
reshape _ [] = []
reshape dim xs = take dim xs : reshape dim (drop dim xs)

main :: IO ()
main = do
   gmshInitialize [] Nothing
   gmshModelOccAddDisk 0 0 0 1 1 Nothing
   gmshModelOccSynchronize
   disk <- gmshModelAddPhysicalGroup 2 [1] Nothing
   pgroups <- gmshModelGetPhysicalGroups $ Just 2
   out <- forM pgroups $ \(dim, tag) -> do
      etags <- gmshModelGetEntities dim tag
      forM etags $ \etag -> do
         etypes <- gmshModelMeshGetElementTypes dim etag
         forM etypes $ \etype -> do
            (elementTags, nodeTags) <- gmshModelMeshGetElementsByType etype etag Nothing Nothing
            return ()
   print asd
   print disk
   --gmshFltkRun
   gmshFinalize
