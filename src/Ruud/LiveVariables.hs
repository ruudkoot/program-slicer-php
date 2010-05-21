module EmbellishedMonotoneFramework.Analysis.LiveVariables where

newtype LiveVariables = LiveVariables ()

instance EmbellishedMonotoneFramework LiveVariables () where
    lHat = undefined

