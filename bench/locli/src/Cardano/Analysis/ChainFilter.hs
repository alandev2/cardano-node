{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-deprecations -Wno-orphans #-}
{- HLINT ignore "Use head" -}
module Cardano.Analysis.ChainFilter (module Cardano.Analysis.ChainFilter) where

import Cardano.Prelude hiding (head)

import Data.Aeson

import Cardano.Slotting.Slot (EpochNo (..),  SlotNo (..))


-- | Conditions for chain subsetting
data ChainFilter
  = CBlock BlockCond
  | CSlot  SlotCond
  deriving (FromJSON, Generic, NFData, Show, ToJSON)

-- | Block classification -- primary for validity as subjects of analysis.
data BlockCond
  = BUnitaryChainDelta        -- ^ All timings account for
                              --    processing of a single block.
  | BFullnessGEq       Double -- ^ Block fullness is above fraction.
  | BFullnessLEq       Double -- ^ Block fullness is below fraction.
  | BSizeGEq           Word64
  | BSizeLEq           Word64
  deriving (FromJSON, Generic, NFData, Show, ToJSON)

deriving instance NFData EpochNo

data SlotCond
  = SlotGEq        SlotNo
  | SlotLEq        SlotNo
  | EpochGEq       EpochNo
  | EpochLEq       EpochNo
  | SlotHasLeaders
  deriving (FromJSON, Generic, NFData, Show, ToJSON)

cfIsSlotCond, cfIsBlockCond :: ChainFilter -> Bool
cfIsSlotCond  = \case { CSlot{}  -> True; _ -> False; }
cfIsBlockCond = \case { CBlock{} -> True; _ -> False; }

catSlotFilters :: [ChainFilter] -> [SlotCond]
catSlotFilters = go [] where
  go :: [SlotCond] -> [ChainFilter] -> [SlotCond]
  go acc = \case
    [] -> reverse acc
    CSlot c:rest -> go (c:acc) rest
    _:rest       -> go    acc  rest
