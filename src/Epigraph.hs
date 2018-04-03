module Epigraph where

import qualified Epigraph.Types as Option

import qualified Data.List as L
import System.Environment
import Data.Maybe 

-- Functions

--Determine if a string is an option.
isOption :: String -> Option -> Bool
isOption x (Option s y _) = fromMaybe False $ (||) 
                            <$> Just (x == s) 
                            <*> ((==head x) <$> y)

getOption :: String -> [Option] -> Maybe Option
getOption s os = L.find (isOption s) os

--getOptions :: [String] -> [Option] -> [(String, b)]
--getOptions (x:y:xs) os 
--           | any (isOption x) os = 
--               case optionType <$> opt of
--                 Just Argument -> (:) (Runtime Arg y) $ getOptions xs os
--                 Just Flag     -> (:) (Runtime Flg) $ getOptions (y:xs) os
--           | otherwise  = getOptions (y:xs) os
--           where opt = getOption x os
--getOptions (x:[]) os = (Runtime (getOption x os) Nothing):getOptions [] os
--getOptions []     _ = []

mktuple :: Option -> String -> (String, b)
mktuple o y 
        | argument && validArgument y = (optionLong o, coerce o y) 
        | otherwise                      = (optionLong o, True)
        where argument = Option.mode o == Argument

validArgument :: Option -> String -> Bool
validArgument o y
              | Option.predicate o == Nothing = True
              | otherwise = fromMaybe False $ Option.predicate <*> pure y

-- Coerce the argumet.
coerce :: Option -> String -> Maybe String
coerce o y
       | Option.coercion == Nothing = y
       | otherwise = Option.coercion <*> pure y

quickOptions :: [(String, OptionType)] -> [Option]
quickOptions (x:xs) = (Option (fst x) (snd x) Nothing Nothing Nothing Nothing):quickOptions xs
quickOptions [] = []
