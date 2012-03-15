import NaiveBayesTrainer (NaiveBayesTrainer)
import qualified NaiveBayesTrainer as NBT

import NaiveBayesClassifier (NaiveBayesClassifier)
import qualified NaiveBayesClassifier as NBC

data SpamClassification = Spam | Ham deriving (Ord, Eq, Show)

nbt :: NaiveBayesTrainer SpamClassification String
nbt =
  NBT.recordFeatures Spam (words "viagara foo bar is done") $
  NBT.recordFeatures Spam (words "viagara some other stuff") $
  NBT.recordFeatures Ham  (words "this is so not spam") $
  NBT.recordFeatures Ham  (words "haskell is great") $
  NBT.recordFeatures Ham  (words "need some more testing")  $
  NBT.recordFeatures Ham  (words "omg wtff") $
  NBT.empty

nbc :: NaiveBayesClassifier SpamClassification String
nbc = NBC.fromTrainer nbt
