module Domain where

import Element
import TagTypes

data Domain = Domain
   { domainElements :: [Element]
   , domainNodes :: [NodeTag]
   , domainNodeCoordinates :: [[Double]]
   } deriving (Show, Eq)
