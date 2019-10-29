module Domain where

data Domain = Domain
   { domainElements :: [String]
   } deriving (Show, Eq)
