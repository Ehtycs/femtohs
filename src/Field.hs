module Field where

import Domain

data Field a = Field [a] deriving (Show, Eq)

fromFunction :: (Num a) => Domain -> ([Double] -> a) -> Field a
fromFunction dom fun
   = Field $ map fun nodes
      where
         nodes = domainNodes dom
