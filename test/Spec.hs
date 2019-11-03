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

   hierarchy <- gmshReadMeshData

   case (M.lookup (DimTag (2,1)) hierarchy >>= M.lookup (EntityTag 1)) of
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
