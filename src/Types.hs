module Epigraph.Types where

type LongOption  = String
type ShortOption = Maybe Char
type Predicate   = Maybe (String -> Bool)
type HelpString  = Maybe String
type Coercion    = Maybe (String -> String)

data OptionMode = Argument
                | Flag
                deriving (Eq, Show)

-- time for a record

data Option = Option { long      :: String
                     , mode      :: OptionMode
                     , short     :: Maybe Char
                     , predicate :: Maybe (String -> Bool)
                     , coercion  :: Maybe (String -> String)
                     , help      :: Maybe String
                     } 

instance Show Option where
  show (Option l m s _ _ h) = show l ++ show m ++ show s ++ show h
