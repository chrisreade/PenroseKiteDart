{-|
Module      : Tgraph.Try
Description : Result types for partial functions
Copyright   : (c) Chris Reade, 2021
License     : BSD-style
Maintainer  : chrisreade@mac.com
Stability   : experimental

Try is a synonym for Either String, which is used for results of partial operations
which return either Right something when defined or Left string when there is a problem
(where string is a failure report).
This is to allow computation to continue in failure cases without necessarily raising an error.
This module contains functions associated with Try results.
-}

module Tgraph.Try
  ( -- * Try - result types with failure reporting (for partial operations).
  Try
  , onFail
  , nothingFail
  , runTry
  , ifFail
  , isFail
  , concatFails
  , ignoreFails
  , atLeastOne
  , noFails
  ) where

import Data.Either(fromRight, lefts, rights, isLeft)


-- | Try is a synonym for Either String.  Used for results of partial functions
-- which return either Right something when defined or Left string when there is a problem
-- where string is a failure report.
-- Note: Either String (and hence Try) is a monad, and this is used frequently for combining  partial operations.
type Try a = Either String a

-- | onFail s exp - inserts s at the front of failure report if exp fails with Left report
onFail:: String -> Try a -> Try a
onFail s = either (Left . (s++)) Right

-- | Converts a Maybe Result into a Try result by treating Nothing as a failure
-- (the string s is the failure report on failure).
-- Usually used as infix (exp `nothingFail` s)
nothingFail :: Maybe b -> String -> Try b
nothingFail a s = maybe (Left s) Right a

-- |Extract the (Right) result from a Try, producing an error if the Try is Left s.
-- The failure report is passed to error for an error report.
runTry:: Try a -> a
runTry = either error id

-- |ifFail a tr - extracts the (Right) result from tr but returning a if tr is Left s.
ifFail :: a -> Try a -> a
ifFail = fromRight 

-- |a try result is a failure if it is a Left
isFail:: Try a -> Bool
isFail = isLeft
   
-- |Combines a list of Trys into a single Try with failure overriding success.
-- It concatenates all failure reports if there are any and returns a single Left r.
-- Otherwise it produces Right rs where rs is the list of all (successful) results.
-- In particular, concatFails [] = Right []
concatFails:: [Try a] -> Try [a]
concatFails ls = case lefts ls of
                 [] -> Right $ rights ls
                 other -> Left $ mconcat other -- concatenates strings for single report

-- |Combines a list of Trys into a list of the successes, ignoring any failures.
-- In particular, ignoreFails [] = []
ignoreFails:: [Try a] -> [a]
ignoreFails = rights

-- | atLeastOne rs - returns the list of successful results if there are any, but fails with an error otherwise.
-- The error report will include the concatenated reports from the failures. 
atLeastOne:: [Try a] -> [a]
atLeastOne [] = error "atLeastOne: applied to empty list.\n"
atLeastOne results = case ignoreFails results of
                 [] -> runTry $ onFail "atLeastOne: no successful results.\n" $ concatFails results
                 other -> other 

-- | noFails rs - returns the list of successes when all cases succeed, but fails with
-- an error and a concatenated failure report of all failures if there is at least one failure.
-- In particular, noFails [] = []
noFails:: [Try a] -> [a]
noFails = runTry . concatFails
