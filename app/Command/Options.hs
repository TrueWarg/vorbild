module Command.Options
  ( Option(..)
  , Destination(..)
  )
  where

data Option 
    = Option 
    {
         templatePath :: FilePath
       , destination :: Destination
    }
    deriving (Show)

data Destination
    = NoSpec
    | Dir FilePath
    deriving (Show)
