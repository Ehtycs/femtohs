import GmshAPI

import Control.Monad (forM, forM_)
--import Control.Applicative
import Mesh
import Element
import Domain
import GmshInterface
import TagTypes
import Field

import qualified Data.Map.Strict as M

makeMesh = do
   gmshInitialize [] Nothing
   gmshModelOccAddRectangle 0 0 0 1 1 Nothing Nothing
   gmshModelOccAddRectangle 1 1 0 1 1 Nothing Nothing
   gmshModelOccAddRectangle (-1) (-1) 0 1 1 Nothing Nothing

   gmshModelOccSynchronize
   points <- gmshModelGetEntities (Just 0)
   gmshModelOccSetMeshSize points 1.0
   gmshModelOccSynchronize
   gmshModelAddPhysicalGroup 2 [1,2] Nothing
   gmshModelAddPhysicalGroup 2 [3] Nothing
   gmshModelMeshGenerate $ Just 2

   mesh <- mkMesh <$> gmshReadMeshData
   return mesh

main :: IO ()
main = do
   putStrLn ""
   putStrLn "Running test code..."
   gmshInitialize [] Nothing
   gmshModelOccAddRectangle 0 0 0 1 1 Nothing Nothing
   gmshModelOccAddRectangle 1 1 0 1 1 Nothing Nothing
   gmshModelOccAddRectangle (-1) (-1) 0 1 1 Nothing Nothing

   gmshModelOccSynchronize
   points <- gmshModelGetEntities (Just 0)
   gmshModelOccSetMeshSize points 1.0
   gmshModelOccSynchronize
   gmshModelAddPhysicalGroup 2 [1,2] Nothing
   gmshModelAddPhysicalGroup 2 [3] Nothing
   gmshModelMeshGenerate $ Just 2

   mesh <- mkMesh <$> gmshReadMeshData

   let domain = defineDomain (DimTag <$> [(2,1), (2,2)]) mesh
   let field = fromFunction domain (\[x,y,z] -> y)

   gmshExportField domain (Just "Asd") field
   gmshFltkRun                 
   gmshFinalize
