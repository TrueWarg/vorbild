module CommandParse
  ( Option(..)
--   , parse
  )
  where

import Data.Maybe (fromMaybe)
import Options.Applicative

data Option 
    = FromSource 
    {
         templatePath :: FilePath
       , destination :: ResultDestination
    }
    | FromTemplateName
    {
         name :: String
       , templatesSrcPath :: FilePath
    }

data ResultDestination
  = NoSpec
  | Dir FilePath

data 

