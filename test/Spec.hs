{-# LANGUAGE DataKinds #-}


import GmshAPI

import Control.Monad (forM, forM_)
--import Control.Applicative
import Mesh
import Element
import Domain
import GmshInterface
import TagTypes
import Field

import qualified Eigen.SparseMatrix as Eig

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

foreach = flip map

cartProduct xs ys = [(x,y) | x <- xs, y <- ys]

gaussIntegrate ips f = sum $ map (\(ip, iw)-> f(ip) * iw) ips

makeLocalStiffMat :: Element -> [(NodeTag, NodeTag, Double)]
makeLocalStiffMat element = map fun dofs
  where  
    intpoints = gaussIntegrationScheme element 1
    ntags = zip [0..] $ elementNodeTags element
    dofs = cartProduct ntags ntags
    basf i = nodeScalarBasis element i
    linearForm i j p = (basf i p)*(basf j p)
    fun ((i,gi), (j,gj)) = (gi, gj, gaussIntegrate intpoints $ linearForm i j) 
    
makeStiffnessMatrix :: Domain -> [(NodeTag, NodeTag, Double)]
makeStiffnessMatrix  = concat . map makeLocalStiffMat . domainElements

unpackNodeTag :: (NodeTag, NodeTag, Double) -> (Int,Int,Double)             
unpackNodeTag ((NodeTag a), (NodeTag b), x) = (a,b,x) 

main :: IO ()
main = do
   mesh <- makeMesh

   let domain = defineDomain (DimTag <$> [(2,1), (2,2)]) mesh
   let field = fromFunction domain (\[x,y,z] -> y)

   gmshExportField domain (Just "Asd") field

   let stiffmats = map unpackNodeTag $ makeStiffnessMatrix domain

   let stiffmat = Eig.fromList stiffmats :: Eig.SparseMatrixXd 16 16

   print stiffmat               
   
--   gmshFltkRun                 
   gmshFinalize
