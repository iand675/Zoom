{-# LANGUAGE GADTs, DeriveDataTypeable #-}
module Zoom.Demand.CLI where 
import Zoom.Demand
import Data.Typeable

data CLI = CLI
         deriving (Typeable)
                    
instance Retrieve CLI where
  retrieve c (Int n d) = do
    putStrLn $ "Please enter an int value for " ++ n ++ ":"
    val <- getLine
    return $ Fulfilled n (read val)
  importFrom = const "Zoom.Demand.CLI"
