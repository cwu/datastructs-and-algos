module NaiveBayesClassifier (
  Probability,
  NaiveBayesClassifier(NBC),
  fromTrainer,
  probability
) where

import NaiveBayesTrainer (Counts)
import qualified NaiveBayesTrainer as NBT

import Data.Map (Map, (!))
import qualified Data.Map as Map

type Probability = Double

data NaiveBayesClassifier c f = NBC {
    classProbabilities       :: Map c Probability,
    featureProbabilities     :: Map f Probability,
    conditionalProbabilities :: Map (f, c) Probability,
    samples                  :: Counts
  } deriving (Eq, Show)

empty :: NaiveBayesClassifier c f
empty = NBC {
    classProbabilities = Map.empty,
    featureProbabilities = Map.empty,
    conditionalProbabilities = Map.empty,
    samples = 0
  }

fromTrainer :: (Ord c, Ord f) => NBT.NaiveBayesTrainer c f ->
                                 NaiveBayesClassifier c f
fromTrainer (NBT.NBT classFeatures classCounts) = NBC {
    classProbabilities       = deriveProbabilities (const s) classCounts,
    featureProbabilities     = deriveProbabilities (const s) featureCounts,
    conditionalProbabilities = Map.unions compositeKeyMaps,
    samples                  = s
  }
  where
    s                = Map.foldl' (+) 0 classCounts
    featureCounts    = Map.unionsWith (+) $ Map.elems classFeatures
    deriveCFProb c   = deriveProbabilities $ const $ classCounts ! c
    cFProbs          = Map.mapWithKey deriveCFProb classFeatures
    compositeKeyMaps = Map.elems $ Map.mapWithKey (\k -> appendKey k) cFProbs


{- Helper Functions -}
deriveProbabilities :: Ord k => (k -> Counts) -> Map k Counts -> Map k Probability
deriveProbabilities findTotal = Map.mapWithKey convertToProbability
  where
    convertToProbability k c = fromIntegral c / fromIntegral (findTotal k)

appendKey :: (Ord k1, Ord k2) => k1 -> Map k2 v -> Map (k2, k1) v
appendKey k = Map.mapKeys (\k' -> (k', k))
