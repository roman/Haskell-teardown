{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Control.Teardown.Internal.Printer where

import Protolude hiding ((<>))

import qualified Data.Text     as Text
import           Data.Typeable (typeOf)

import Control.Teardown.Internal.Core
import Data.Monoid                    ((<>))
import Text.PrettyPrint.ANSI.Leijen   hiding ((<>))

treeTrunk :: Int -> Int -> Doc
treeTrunk start level =
  hcat (map (\_ -> text "    ") [1..start]) <>
  hcat (map (\_ -> text "   |") [start..pred level])

renderTeardownReport :: TeardownResult -> Doc
renderTeardownReport result =
    render 0 0 result <> hardline
  where
    renderError start level (SomeException err) =
      let
        (fstErrLine:errLines) =
          Text.lines (show err)

        errorReport =
          treeTrunk (pred start) (succ level)
           <> ">"
           <> indent 2 (text (show (typeOf err)) <> ":")
           <+> text (Text.unpack fstErrLine)
          : map (\l ->
                    treeTrunk (pred start) (succ level)
                    <> ">"
                    <> indent 2 (text $ Text.unpack l))
                 errLines
      in
        vcat errorReport

    renderTree start level disposeResults =
      case disposeResults of
        [] ->
          mempty
        [ lastResult ] ->
          treeTrunk start (succ level) <> render (succ start) (succ level) lastResult
        (r : results) ->
          treeTrunk start (succ level) <> render start (succ level) r <$$> renderTree start level results
    render start level disposeResult =
      case disposeResult of
        EmptyResult desc ->
            "`-"
             <+> "✓"
             <+> text (Text.unpack desc)
             <+> "(empty)"

        LeafResult desc elapsed Nothing ->
            "`-"
             <+> "✓"
             <+> text (Text.unpack desc)
             <+> text ("(" <> show elapsed <> ")")

        LeafResult desc elapsed (Just err) ->
            "`-"
             <+> "✘"
             <+> text (Text.unpack desc)
             <+> text ("(" <> show elapsed <> ")")
             <$$> renderError start level err

        BranchResult desc elapsed didFail results ->
          vcat [ "`-"
                 <+> (if didFail then "✘" else "✓")
                 <+> text (Text.unpack desc)
                 <+> text ("(" <> show elapsed <> ")")
               , renderTree start level results
               ]
