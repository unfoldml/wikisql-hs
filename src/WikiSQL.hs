{-# language DeriveGeneric #-}
{-# language OverloadedStrings #-}
module WikiSQL where

import GHC.Generics (Generic)
-- aeson
import Data.Aeson (FromJSON(..), ToJSON(..))
-- conduit
import Conduit (runConduitRes, ConduitT, sinkList, printC)
-- jsonl-conduit
import JSONL.Conduit (sourceFileC)
-- text
import Data.Text (Text)

data Item = Item {
  phase :: Int
  , question :: Text
  , sql :: Sql
  , table_id :: TableId
                 } deriving (Eq, Show, Generic)
instance FromJSON Item
instance ToJSON Item

newtype TableId = TableId Text deriving (Eq, Show, Generic)
instance FromJSON TableId
instance ToJSON TableId

data Sql = Sql {
  conds :: Conds
  , sel :: Int
  , agg :: Int 
               } deriving (Eq, Show, Generic)
instance FromJSON Sql
instance ToJSON Sql

data Conds = Conds Int Int Text deriving (Eq, Show, Generic)
instance FromJSON Conds
instance ToJSON Conds

{-
{
   "phase":1,
   "question":"who is the manufacturer for the order year 1998?",
   "sql":{
      "conds":[
         [
            0,
            0,
            "1998"
         ]
      ],
      "sel":1,
      "agg":0
   },
   "table_id":"1-10007452-3"
}
-}



{-
{
   "id":"1-1000181-1",
   "header":[
      "State/territory",
      "Text/background colour",
      "Format",
      "Current slogan",
      "Current series",
      "Notes"
   ],
   "types":[
      "text",
      "text",
      "text",
      "text",
      "text",
      "text"
   ],
   "rows":[
      [
         "Australian Capital Territory",
         "blue/white",
         "Yaa\u00b7nna",
         "ACT \u00b7 CELEBRATION OF A CENTURY 2013",
         "YIL\u00b700A",
         "Slogan screenprinted on plate"
      ], ...
-}
