{-# LANGUAGE GADTs #-}
module Zoom.Demand where


-- | Demands are used to allow the method of attaining parameters to run tasks 
--   to be independent of the frontend. Int, Bool, and Doubles are Readable, 
--   but exist for the sake of using tasks with value backends that return proper values.
--   The two string parameters are the key (the 'name' of the value), and a description.
data Demand a where 
  Int    :: String -> String -> Demand Int
  Bool   :: String -> String -> Demand Bool
  Double :: String -> String -> Demand Double
  String :: String -> String -> Demand String
  Read   :: Read b => String -> String -> Demand b

data Fulfilled a = Fulfilled {demandName :: String, value :: a}

-- | The Retrieve class takes a 'source' parameter and a demand and uses the source
--   in some meaningful way to retrieve the demanded value. Use this if you want to make  
--   a new frontend for zoom tasks.   
class Retrieve s where
  retrieve :: s -> Demand a -> IO (Fulfilled a)
  importFrom :: s -> String
  
makeDemand = undefined
retrieveKeyValuePairs = undefined
stringForKey = undefined