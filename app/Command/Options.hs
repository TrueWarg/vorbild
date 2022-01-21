module Command.Options
  ( Option(..)
  , Source(..)
  )
  where

data Option 
    = FromTempatePath 
    {
         templatePath :: FilePath
       , destination :: Source
    }
    | FromTemplateName
    {
         name :: String
       , templatesSrc :: Source
    }
    deriving (Show)

data Source
  = NoSpec
  | Dir FilePath
  deriving (Show)
