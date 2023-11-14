{-# LANGUAGE DeriveGeneric #-}

module Structure (
    Post(..)
) where

import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)

data Post = Post { 
    id :: Int, 
    title :: String, 
    content :: String, 
    timestamp :: Maybe UTCTime,
    link :: Maybe String
  } deriving (Show, Generic)
