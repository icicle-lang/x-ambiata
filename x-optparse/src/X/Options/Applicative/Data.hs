module X.Options.Applicative.Data where

-- | Applications can be run for real
-- | or just return a description of what they are about to do
data RunType =
  DryRun | RealRun
  deriving (Eq, Show)

data SafeCommand a =
  Version
  | RunCommand RunType a
  deriving (Eq, Show)
