module Domain where

import Element

data Domain = Domain
   { domainElements :: [Element]
   , domainNodes :: [[Double]]
   } deriving (Show, Eq)
