{-# LANGUAGE LambdaCase #-}
{-# language DeriveGeneric #-}
{-# language OverloadedStrings #-}
{-# options_ghc -Wno-unused-imports #-}
-- | WikiSQL - A large crowd-sourced dataset for developing natural language interfaces for relational databases.
--
-- References
--
-- 1. Zhong et al, Seq2SQL: Generating Structured Queries from Natural Language using Reinforcement Learning https://arxiv.org/abs/1709.00103
--
-- 2. https://github.com/salesforce/WikiSQL
module WikiSQL (-- * Dataset
  Item(..), Sql(..),
  AggOp(..), Cond(..), CondOp(..), CondValue(..),
    -- * Table
    TableId, Table(..), Type(..), Row(..))where

import Data.Void (Void)
import GHC.Generics (Generic)
-- aeson
import Data.Aeson (FromJSON(..), ToJSON(..), (.:), withObject, withScientific, eitherDecode')
import Data.Aeson.Types (Parser, Value(..))
-- bytestring
import qualified Data.ByteString.Lazy as LBS (ByteString)
-- conduit
import Conduit (runConduitRes, ConduitT, (.|), sinkList, printC, MonadResource)
-- jsonl-conduit
import JSONL.Conduit (sourceFileC)
--
import Data.Scientific (toRealFloat)
-- text
import Data.Text (Text)

-- sourceDataset :: (MonadResource m) => FilePath -> ConduitT Item Void m res -> IO res
-- sourceDataset fp sink = runConduitRes $ sourceFileC fp .| sink


-- | Question, query and table ID
data Item = Item {
  phase :: Int -- ^ phase in which the dataset was collected. We collected WikiSQL in two phases
  , question :: Text -- ^ natural language question written by the worker
  , sql :: Sql -- ^ SQL query corresponding to the question
  , table_id :: TableId -- ^ ID of the table to which this question is addressed
                 } deriving (Eq, Show, Generic)
instance FromJSON Item
-- instance ToJSON Item

data Table = Table {
  tId :: TableId
  , tHeader :: [Text] -- ^ list of column names in the table
  , tTypes :: [Type]
  , tRows :: [Row]
                   } deriving (Eq, Show)
instance FromJSON Table where
  parseJSON = withObject "Table" $ \ o -> Table <$>
    o .: "id" <*>
    o .: "header" <*>
    o .: "types" <*>
    o .: "rows"

data Type = TyText deriving (Eq, Show, Generic)
instance FromJSON Type

data Row = Row [Text] deriving (Eq, Show, Generic)
instance FromJSON Row

newtype TableId = TableId Text deriving (Eq, Show, Generic)
instance FromJSON TableId
instance ToJSON TableId

data Sql = Sql {
  conds :: [Cond]
  , sel :: Int -- ^ numerical index of the column that is being selected. You can find the actual column from the table
  , agg :: AggOp -- ^ aggregation operator that is being used
               } deriving (Eq, Show, Generic)
instance FromJSON Sql
-- instance ToJSON Sql


-- agg_ops = ['', 'MAX', 'MIN', 'COUNT', 'SUM', 'AVG']
newtype AggOp = AggOp (Maybe AggOp_) deriving (Eq, Show)
data AggOp_ = AOMax | AOMin | AOCount | AOSum | AOAvg deriving (Eq, Show)
instance FromJSON AggOp where
  parseJSON = withScientific "AggOp" $ \ a -> AggOp <$>
    case a of
      1 -> pure $ Just AOMax
      2 -> pure $ Just AOMin
      3 -> pure $ Just AOCount
      4 -> pure $ Just AOSum
      5 -> pure $ Just AOAvg
      _ -> pure Nothing

-- cond_ops = ['=', '>', '<', 'OP']
data Cond = Cond Int CondOp CondValue deriving (Eq, Show, Generic)
instance FromJSON Cond
-- instance ToJSON Cond

-- | Condition value
newtype CondValue = CondValue (Either Double Text) deriving (Eq, Show)
instance FromJSON CondValue where
  parseJSON o = CondValue <$> case o of
    Number n -> pure $ Left $ toRealFloat n
    String t -> pure $ Right t
    _ -> fail "CondValue expects either a float or a string"

data CondOp = COEq | COGt | COLt | COOp deriving (Eq, Show)
instance FromJSON CondOp where
  parseJSON = withScientific "CondOp" $ \ a -> case a of
    0 -> pure COEq
    1 -> pure COGt
    2 -> pure COLt
    _ -> pure COOp

t0 :: LBS.ByteString
t0 = "{\"phase\":1,   \"question\":\"who is the manufacturer for the order year 1998?\",   \"sql\":{      \"conds\":[[0, 0, \"1998\" ] ],      \"sel\":1,      \"agg\":0   },   \"table_id\":\"1-10007452-3\"}"




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
