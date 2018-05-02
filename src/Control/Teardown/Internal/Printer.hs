{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Control.Teardown.Internal.Printer where

import RIO         hiding ((<>))

import           Data.Typeable (typeOf)
import qualified RIO.Text      as Text

import Data.Text.Prettyprint.Doc

import Control.Teardown.Internal.Types

treeTrunk :: Int -> Int -> Doc ann
treeTrunk start level = hcat (map (\_ -> "    ") [1 .. start])
  <> hcat (map (\_ -> "   |") [start .. level - 1])

-- | Renders an ASCII Tree with the "TeardownResult" of a "Teardown" sub-routine
-- execution
prettyTeardownResult :: TeardownResult -> Doc ann
prettyTeardownResult result = render 0 0 result <> hardline
 where
  renderError start level (SomeException err) =
    let
      (fstErrLine, errLines) = case Text.lines (tshow err) of
        [] ->
          error "Expecting reported error to have a line of content, got none"

        (fstErrLine' : errLines') -> (fstErrLine', errLines')

      errorReport =
        treeTrunk (start - 1) (level + 1)
          <>  ">"
          <>  indent 2 (pretty (show (typeOf err)) <> ":")
          <+> pretty (Text.unpack fstErrLine)
          :   map
                (\l -> treeTrunk (start - 1) (level + 1) <> ">" <> indent
                  2
                  (pretty $ Text.unpack l)
                )
                errLines
    in
      vcat errorReport

  renderTree start level disposeResults = case disposeResults of
    [] -> mempty
    [lastResult] ->
      treeTrunk start (level + 1) <> render (start + 1) (level + 1) lastResult
    (r : results) ->
      treeTrunk start (level + 1)
        <>   render     start (level + 1) r
        <> hardline
        <> renderTree start level       results

  render start level disposeResult = case disposeResult of
    EmptyResult desc -> "`-" <+> "✓" <+> pretty (Text.unpack desc) <+> "(empty)"

    LeafResult desc elapsed Nothing ->
      "`-" <+> "✓" <+> pretty (Text.unpack desc) <+> pretty ("(" <> show elapsed <> ")")

    LeafResult desc elapsed (Just err) ->
      "`-"
        <+>  "✘"
        <+>  pretty (Text.unpack desc)
        <+>  pretty ("(" <> show elapsed <> ")")
        <> hardline
        <> renderError start level err

    BranchResult desc elapsed didFail results -> vcat
      [ "`-"
      <+> (if didFail then "✘" else "✓")
      <+> pretty (Text.unpack desc)
      <+> parens (pretty $ show elapsed)
      , renderTree start level results
      ]
