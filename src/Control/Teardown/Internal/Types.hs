{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Control.Teardown.Internal.Types where

import RIO
import RIO.Time (NominalDiffTime)

#if MIN_VERSION_base(4,9,0)
import GHC.Generics (Generic)
#endif

import           Data.Text.Prettyprint.Doc (Pretty, pretty, (<+>))
import qualified Data.Text.Prettyprint.Doc as Pretty


--------------------------------------------------------------------------------

type Description = Text

-- | Result from a 'Teardown' sub-routine
data TeardownResult
  -- | Result is composed by multiple teardown sub-routines
  = BranchResult
    {
      -- | Text description of parent teardown spec
      resultDescription :: !Description
      -- | Sum of elapsed time on sub-routines execution
    , resultElapsedTime :: !NominalDiffTime
      -- | Tells if any sub-routines failed
    , resultDidFail     :: !Bool
      -- | Results of inner sub-routines
    , resultListing     :: ![TeardownResult]
    }
  -- | Result represents a single teardown sub-routine
  | LeafResult
    {
      -- | Text description of sub-routine
      resultDescription :: !Description
      -- | Elapsed time on sub-routine execution
    , resultElapsedTime :: !NominalDiffTime
      -- | Exception from sub-routine
    , resultError       :: !(Maybe SomeException)
    }
  -- | Represents a stub cleanup operation (for lifting pure values)
  | EmptyResult
    {
      -- | Text description of faked sub-routine
      resultDescription :: !Description
    }
  deriving (Generic, Show)

instance Pretty TeardownResult where
  pretty result =
    case result of
      EmptyResult {resultDescription} ->
        "✓" <+> pretty resultDescription <+> Pretty.parens "empty"
      LeafResult {resultDescription, resultElapsedTime, resultError} ->
        case resultError of
          Nothing ->
            "✓" <+> pretty resultDescription <+> Pretty.parens (pretty $ show resultElapsedTime)
          Just err ->
            Pretty.hang 2
              (Pretty.vsep ["✘"
                            <+> pretty resultDescription
                            <+> Pretty.parens (pretty $ show resultElapsedTime)
                           , "|" <+> pretty (show err) ])
      BranchResult {resultDidFail, resultDescription, resultElapsedTime, resultListing} ->
        let
          symbolDoc = if resultDidFail then "✘" else "✓"
        in
          symbolDoc
          <+> pretty resultDescription
          <+> Pretty.parens (pretty $ show resultElapsedTime)
          <> Pretty.hardline
          <> Pretty.indent 2 (Pretty.align (Pretty.vsep (map pretty resultListing)))

instance Display TeardownResult where
  display = displayShow . pretty

getElapsedTime :: TeardownResult -> NominalDiffTime
getElapsedTime result =
  case result of
    BranchResult {resultElapsedTime} -> resultElapsedTime
    LeafResult {resultElapsedTime}   -> resultElapsedTime
    EmptyResult {}                   -> 0

instance NFData TeardownResult where
  rnf result =
    case result of
      EmptyResult !_desc ->
        ()
      LeafResult !_desc !_time !_err ->
        ()
      BranchResult !_desc !_time !_didFail listing ->
        rnf listing

-- | Sub-routine that performs a resource cleanup operation
newtype Teardown
  = Teardown (IO TeardownResult)
  deriving (Generic)

instance NFData Teardown where
  rnf !_ = ()

-- | A record that __is__ or __contains__ a 'Teardown' sub-routine should
-- instantiate this typeclass
class HasTeardown teardown where
  -- | Executes teardown sub-routine returning a "TeardownResult"
  getTeardown :: teardown -> Teardown

-- | A resource or sub-routine that can be transformed into a 'Teardown'
-- operation
class IResource resource where
  newTeardown :: Text -> resource -> IO Teardown
