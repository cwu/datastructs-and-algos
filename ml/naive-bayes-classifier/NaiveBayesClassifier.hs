module NaiveBayesClassifier (
  LogProbability,
  NaiveBayesClassifier(NBC),
  fromTrainer,
  classify,
  analyze
) where

import NaiveBayesTrainer (Counts)
import qualified NaiveBayesTrainer as NBT

import Data.Map (Map, (!))
import qualified Data.Map as Map

import qualified Data.List as List

type LogProbability = Double

data NaiveBayesClassifier c f = NBC {
    priorsMap       :: Map c LogProbability,
    conditionalsMap :: Map (f, c) LogProbability,
    samples         :: Counts
  } deriving (Eq, Show)

-- constants d will be the default probability for unknown features
(alpha, d)            = (1.0, 2.0)

empty :: NaiveBayesClassifier c f
empty = NBC {
    priorsMap       = Map.empty,
    conditionalsMap = Map.empty,
    samples         = 0
  }

fromTrainer :: (Ord c, Ord f) => NBT.NaiveBayesTrainer c f ->
                                 NaiveBayesClassifier c f
fromTrainer (NBT.NBT classFeatures classCounts) = NBC {
    priorsMap       = logProbabilities (const numSamples) classCounts,
    conditionalsMap = Map.unions compositeKeyMaps,
    samples         = numSamples
  }
  where
    numSamples       = Map.foldl' (+) 0 classCounts
    featureCounts    = Map.unionsWith (+) $ Map.elems classFeatures
    deriveCFProb c   = logProbabilities $ const $ classCounts ! c
    cFProbs          = Map.mapWithKey deriveCFProb classFeatures
    compositeKeyMaps = Map.elems $ Map.mapWithKey appendKey cFProbs

classify :: (Ord c, Ord f) => NaiveBayesClassifier c f -> [f] -> c
classify nbc features = (fst . argmax') $ analyze nbc features
  where
    -- note this assumes the list has numbers > 0
    argmax'         = List.foldl1' (\m t -> if snd t > snd m then t else m)

analyze :: (Ord c, Ord f) => NaiveBayesClassifier c f -> [f] -> [(c, LogProbability)]
analyze nbc features = zip classifications likelihoods
  where
    classifications = Map.keys $ priorsMap nbc
    likelihoods     = map (scoreClass nbc features) classifications

{- Helper Functions -}
scoreClass :: (Ord c, Ord f) => NaiveBayesClassifier c f -> [f] -> c ->
                                LogProbability
scoreClass nbc features class_ = List.foldl' (+) prior conditionalScores
  where
    prior             = priorsMap nbc ! class_
    conditionalScores = map (\f -> Map.findWithDefault (log (1/d)) (f, class_) (conditionalsMap nbc)) features

logProbabilities :: Ord k => (k -> Counts) -> Map k Counts ->
                             Map k LogProbability
logProbabilities findTotal = Map.mapWithKey laplaceSmoothen
  where
    laplaceSmoothen k c = log $
      (fromIntegral c + alpha) / (fromIntegral (findTotal k) + d * alpha)

appendKey :: (Ord k1, Ord k2) => k1 -> Map k2 v -> Map (k2, k1) v
appendKey k = Map.mapKeys (\k' -> (k', k))
