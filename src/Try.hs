{-|
Module      : Try
Description : Result types for partial functions
Copyright   : (c) Chris Reade, 2021
License     : BSD-style
Maintainer  : chrisreade@mac.com
Stability   : experimental

Try is a synonym for Either ShowS, and is used for results of partial operations
which return either Right something when defined or Left report when there is a problem
(where report is a failure report).
This is to allow computation to continue in failure cases without necessarily raising an error.
This module contains functions associated with Try results.
-}

{-# LANGUAGE FlexibleInstances #-} -- needed for instance Show (ShowS)
{-# OPTIONS_GHC -Wno-orphans #-}   -- needed for instance Show (ShowS)

module Try
  ( -- * Try - result types with failure reporting (for partial operations).
  Try
  , onFail
  , nothingFail
  , failReport
  , failReports
  , runTry
  , ifFail
  , isFail
  , concatFails
  , ignoreFails
  , tryAtLeastOne
  , atLeastOne
  -- , noFails
  ) where

import Data.Either(fromRight, lefts, rights, isLeft)


-- | Try is a synonym for Either ShowS.  Used for results of partial functions
-- which return either Right something when defined or Left r when there is a problem
-- where r is a (prepending) failure report.
-- Note: ShowS = String -> String makes prepending Strings efficient as composition
-- Note: Either ShowS (and hence Try) is a monad, and this is used frequently for combining  partial operations.
type Try a = Either ShowS a

-- | onFail s exp - prepends s at the front of a failure report if exp fails with Left report
-- but does nothing otherwise.
onFail:: String -> Try a -> Try a
onFail s = either (Left . (pure s <>)) Right --either (Left . (pure s .)) Right

-- |failReport s - creates a failure (Left), prepending s for the failure report
failReport :: String -> Try a
failReport  = Left . (<>)

-- |failReports ss - creates a failure (Left), concatenating ss for the failure report
failReports :: [String] -> Try a
failReports = Left . mconcat . fmap (<>) --failReport . mconcat
     -- Note: failReport . mconcat  concatenates strings
     -- but Left . mconcat . fmap (<>)  composes functions   

-- | nothingFail a s - Converts a Maybe Result (a) into a Try result by treating Nothing as a failure
-- (the String s is used for the failure report on failure).
-- Usually used as infix (exp `nothingFail` s)
nothingFail :: Maybe b -> String -> Try b
nothingFail a s = maybe (failReport s) Right a

-- |Extract the (Right) result from a Try, raising an error if the Try is Left r.
-- The failure report (from Left r) is converted to a Stirng and passed to error.
runTry:: Try a -> a
runTry = either (error . ($ "")) id

-- |ifFail a tr - extracts the (Right) result from tr but returning a if tr is Left _ .
ifFail :: a -> Try a -> a
ifFail = fromRight

-- |a try result is a failure if it is a Left
isFail:: Try a -> Bool
isFail = isLeft

-- |Combines a list of Trys into a single Try with failure overriding success.
-- It concatenates all failure reports if there are any and returns a single Left r.
-- Otherwise it produces Right rs where rs is the list of all (successful) results.
-- In particular, concatFails [] = Right [] (so is NOT a fail)
concatFails:: [Try a] -> Try [a]
concatFails ls = case lefts ls of
                 [] -> Right $ rights ls
                 other -> Left $ mconcat other -- concatenates reports for single report

-- |Combines a list of Trys into a list of the successes, ignoring any failures.
-- In particular, ignoreFails [] = []
ignoreFails:: [Try a] -> [a]
ignoreFails = rights

-- | tryAtLeastOne rs - returns Right with the list of successful results if there are any,
-- but Left with a fail report otherwise.
-- The error report will include the concatenated reports from multiple failures. 
tryAtLeastOne:: [Try a] -> Try [a]
tryAtLeastOne [] = failReport "atLeastOne: applied to empty list.\n"
tryAtLeastOne results = case ignoreFails results of
                 [] -> onFail "atLeastOne: no successful results.\n" $ concatFails results
                 other -> Right other

-- | atLeastOne rs - returns the list of successful results if there are any, but fails with an error otherwise.
-- The error report will include the concatenated reports from multiple failures. 
atLeastOne:: [Try a] -> [a]
atLeastOne = runTry . tryAtLeastOne

-- |Cheating - a ShowS function is "shown" by applying it to a String
instance Show ShowS where
    show r = show "<function> = (" ++ r "" ++ show " ++)"
