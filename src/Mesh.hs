module Mesh where

data Mesh = Mesh
   { meshDomains :: [String]
   , meshName :: String
   } deriving (Eq, Show)
