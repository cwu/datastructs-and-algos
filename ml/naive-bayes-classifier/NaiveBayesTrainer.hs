module NaiveBayesTrainer (
  Counts,
  NaiveBayesTrainer(NBT),
  empty,
  recordFeatures
) where

import Data.Map (Map, (!))
import qualified Data.Map as Map

import qualified Data.List as List

type Counts = Int

data NaiveBayesTrainer c f = NBT {
    classFeatures :: Map c (Map f Counts),
    classCounts   :: Map c Counts
  } deriving (Eq, Show)


empty :: NaiveBayesTrainer c f
empty = NBT { classFeatures = Map.empty, classCounts = Map.empty }

recordFeatures :: (Ord c, Ord f) => c -> [f] -> NaiveBayesTrainer c f ->
                                    NaiveBayesTrainer c f
recordFeatures class_ features nbt = NBT {
    classFeatures = Map.insert class_ featureCounts' (classFeatures nbt),
    classCounts   = Map.insertWith' (+) class_ 1 (classCounts nbt)
  }
  where
    featureCounts  = Map.findWithDefault Map.empty class_ (classFeatures nbt)
    featureCounts' = List.foldl' (flip keyInc') featureCounts features

{- Helper Functions -}

keyInc' :: (Ord k, Num v) => k -> Map k v -> Map k v
keyInc' k = Map.insertWith' (+) k 1
