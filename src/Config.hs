module Config
  ( Config (..),
    defaultConf,
  )
where

import MathBlock (InterpolationMethod (..))

data Config = Config
  { method :: MathBlock.InterpolationMethod,
    startPoints :: Int,
    step :: Double
  }
  deriving (Show)

defaultConf :: Config
defaultConf =
  Config
    { method = Linear,
      startPoints = 2,
      step = 0.5
    }
