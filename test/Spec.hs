import GmshAPI

import Control.Monad (forM, forM_)
--import Control.Applicative
import Mesh
import Element
import Domain
import GmshInterface
import TagTypes

import qualified Data.Map.Strict as M

main :: IO ()
main = do
   putStrLn "Running test code..."
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

   mesh <- mkMesh <$> gmshReadMeshData

   case (meshGetEntityOf (DimTag (2,1)) (EntityTag 1)) mesh of
      Just x -> forM_ x print
      Nothing -> putStrLn "Not found"

   --gmshFltkRun
   gmshFinalize
