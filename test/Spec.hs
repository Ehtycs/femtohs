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
   gmshModelAddPhysicalGroup 2 [1] Nothing
   gmshModelMeshGenerate $ Just 2

   mesh <- mkMesh <$> gmshReadMeshData

   let domain = defineDomain [DimTag (2,1)] mesh

   let element = head $ domainElements domain

   let field = fromFunction domain (\[x,y,z] -> x + y + z)

   let a = [[1,2,3], [4,5,6]]
   let b = [[10,20], [100,200], [1000,2000]]
   let [(intp, _)] = gaussIntegrationScheme element 1
   let jac = isopJacobian element intp
   print jac
   print $ elementNodes element
   print field
   print domain
   print $ meshNodes mesh

   -- -- test provided by numpy
   -- let test1 = matmul a b == [[ 3210,  6420],[ 6540, 13080]]
   --
   -- let test2 = matmul b a == [[   90,   120,   150],
   --                            [  900,  1200,  1500],
   --                            [ 9000, 12000, 15000]]
   -- print test1
   -- print test2



   -- case (meshGetEntityOf (DimTag (2,1)) (EntityTag 1)) mesh of
   --    Just x -> forM_ x print
   --    Nothing -> putStrLn "Not found"

   --gmshFltkRun
   gmshFinalize
