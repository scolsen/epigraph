module Epigraph where

import qualified Data.List as L
import System.Environment
import Data.Maybe 

type OptionLong      = String
type OptionShort     = Maybe Char
type OptionPredicate = Maybe (String -> Bool)
type OptionHelp      = Maybe String
type OptionCoercion  = Maybe (String -> String)
type OptionStub      = (String, OptionMode)
type OptionSpec      = (OptionLong, OptionMode, OptionShort, OptionPredicate, OptionCoercion, OptionHelp)

data OptionMode = Argument
                | Flag
                deriving (Eq, Show)

data Argument = StringArgument String
              | BoolArgument Bool
              | NoArgument
              deriving (Eq, Show)

data Option = Option { long         :: String
                     , mode         :: OptionMode
                     , short        :: Maybe Char
                     , predicate    :: (String -> Bool)
                     , coercion     :: (String -> String)
                     , help         :: Maybe String
                     , hasPredicate :: Bool
                     , hasCorecion  :: Bool
                     } 

instance Show Option where
  show (Option l m s _ _ h p c) = "Option: " 
                                  ++ " long: " ++ show l 
                                  ++ " type: " ++ show m 
                                  ++ " short: " ++ show s
                                  ++ " help: " ++ show h 
                                  ++ " Has predicate: " ++  show p 
                                  ++ " Has coercion: " ++  show c 
                                  ++ "\n" 

data ProcessedOption = Processed Option Argument
                     | UnrecognizedOption String
                     deriving Show

-- | Construct Options

quickOptions :: [OptionStub] -> [Option]
quickOptions (x:xs) = (Option (fst x) (snd x) s (const True) id Nothing False False):quickOptions xs
                    where s = setShort (fst x) (x:xs)
quickOptions [] = []

options :: [OptionSpec] -> [Option]
options ((x, y, z, m, n, k):xs) = (Option x y z (fromMaybe (const True) m) (fromMaybe id n) k p c):options xs
                                where p = isJust m
                                      c = isJust n
options [] = []

-- | Construct Option Argument Pairs

setArgument :: Option -> String -> Argument
setArgument o s 
            | flag                      = BoolArgument True
            | s == ""                   = NoArgument
            | argument && predicate o s = StringArgument $ coercion o s
            | otherwise                 = NoArgument
            where argument = mode o == Argument
                  flag     = mode o == Flag

-- | Get CLI Values

isOption :: String -> Option -> Bool
isOption x o = x == long o || fromMaybe False ((==head x) <$> short o)

getOption :: String -> [Option] -> String -> ProcessedOption
getOption s os a
          | null go   = UnrecognizedOption s
          | otherwise = Processed (head go) $ setArgument (head go) a
          where go = filter (isOption s) os

isUnrecognized :: ProcessedOption -> Bool
isUnrecognized (UnrecognizedOption _) = True
isUnrecognized _ = False

hasMode :: ProcessedOption -> Maybe OptionMode
hasMode (Processed o a) = Just $ mode o
hasMode _               = Nothing

getOptions :: [String] -> [Option] -> [ProcessedOption]
getOptions (s:y:ss) os
           | hasMode go == Just Argument = go:getOptions ss os
           | otherwise              = go:getOptions (y:ss) os 
           where go = getOption s os y
getOptions (s:[]) os = (getOption s os ""):[]
getOptions [] _      = []

recognized :: [ProcessedOption] -> [ProcessedOption]
recognized xs = filter (not . isUnrecognized) xs
recognized [] = []

-- | Autopopulate Option short name

firstLetters :: [String] -> [Char]
firstLetters (x:xs) = (head x):firstLetters xs
firstLetters [] = []

isUnique :: Char -> [Char] -> Bool
isUnique x cs = (length $ filter (\c -> c == x) cs) == 1

isUniqueShort :: String -> [String] -> Bool
isUniqueShort x xs = isUnique (head x) (firstLetters xs)

optionNames :: [(String, OptionMode)] -> [String]
optionNames (x:xs) = (fst x):optionNames xs  
optionNames []     = []

setShort :: String -> [(String, OptionMode)] -> Maybe Char
setShort x os 
         | (isUniqueShort x $ optionNames os) == True = Just $ head x
         | otherwise = Nothing
