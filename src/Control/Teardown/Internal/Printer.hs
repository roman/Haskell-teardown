{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Control.Teardown.Internal.Printer where

import RIO

import           Data.Typeable (typeOf)
import qualified RIO.Text      as Text

import Text.PrettyPrint.ANSI.Leijen hiding ((<>))

import Control.Teardown.Internal.Types

treeTrunk :: Int -> Int -> Doc
treeTrunk start level = hcat (map (\_ -> text "    ") [1 .. start])
  <> hcat (map (\_ -> text "   |") [start .. level - 1])

-- | Renders an ASCII Tree with the "TeardownResult" of a "Teardown" sub-routine
-- execution
prettyTeardownResult :: TeardownResult -> Doc
prettyTeardownResult result = render 0 0 result <> hardline
 where
  renderError start level (SomeException err) =
    let
      (fstErrLine, errLines) = case Text.lines (tshow err) of
        [] ->
          error "Expecting reported error to have a line of content, got none"

        (fstErrLine':errLines') -> (fstErrLine', errLines')

      errorReport =
        treeTrunk (start - 1) (level + 1)
          <>  ">"
          <>  indent 2 (text (show (typeOf err)) <> ":")
          <+> text (Text.unpack fstErrLine)
          :   map
                ( \l -> treeTrunk (start - 1) (level + 1) <> ">" <> indent
                  2
                  (text $ Text.unpack l)
                )
                errLines
    in
      vcat errorReport

  renderTree start level disposeResults = case disposeResults of
    [] -> mempty
    [lastResult] ->
      treeTrunk start (level + 1) <> render (start + 1) (level + 1) lastResult
    (r:results) ->
      treeTrunk start (level + 1)
        <>   render     start (level + 1) r
        <$$> renderTree start level       results
  render start level disposeResult = case disposeResult of
    EmptyResult desc -> "`-" <+> "✓" <+> text (Text.unpack desc) <+> "(empty)"

    LeafResult desc elapsed Nothing ->
      "`-" <+> "✓" <+> text (Text.unpack desc) <+> text
        ("(" <> show elapsed <> ")")

    LeafResult desc elapsed (Just err) ->
      "`-"
        <+>  "✘"
        <+>  text (Text.unpack desc)
        <+>  text ("(" <> show elapsed <> ")")
        <$$> renderError start level err

    BranchResult desc elapsed didFail results -> vcat
      [ "`-"
      <+> (if didFail then "✘" else "✓")
      <+> text (Text.unpack desc)
      <+> text ("(" <> show elapsed <> ")")
      , renderTree start level results
      ]
