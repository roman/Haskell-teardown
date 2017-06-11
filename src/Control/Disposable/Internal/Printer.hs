{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Control.Disposable.Internal.Printer where

import Protolude hiding ((<>))

import Data.Typeable (typeOf)
import qualified Data.Text as Text

import Data.Monoid ((<>))
import Text.PrettyPrint.ANSI.Leijen hiding ((<>))
import Control.Disposable.Internal.Disposable

treeTrunk :: Int -> Int -> Doc
treeTrunk start level =
  hcat (map (\_ -> text "    ") [1..start]) <>
  hcat (map (\_ -> text "   |") [start..pred level])


renderTeardownReport :: DisposeResult -> Doc
renderTeardownReport result =
    render 0 0 result <> hardline
  where
    renderError start level (SomeException err) =
      let
        (fstErrLine:errLines) =
          Text.lines (show err)

        errorReport =
          [treeTrunk (pred start) (succ level)
           <> indent 2 (text (show (typeOf err)) <> ":")
           <+> text (Text.unpack fstErrLine)]
          ++ map (\l ->
                    treeTrunk (pred start) (succ level)
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
            ("`-"
             <+> "✓"
             <+> text (Text.unpack desc)
             <+> "(empty)")

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
                 <+> black (if didFail then "✘" else "✓")
                 <+> text (Text.unpack desc)
                 <+> text ("(" <> show elapsed <> ")")
               , renderTree start level results
               ]


-- foobar :: IO ()
-- foobar = do
--   baruta <- newDisposable "baruta" (return ())
--   bqto   <- newDisposable "barquisimeto" (return ())
--   csc    <- concatDisposables "caracas" [baruta]
--   venezuela <- concatDisposables "venezuela" [bqto, csc]
--   new_west <- newDisposable "new westminster" (return ())
--   vancouver  <- concatDisposables "vancouver" [new_west]
--   calgary   <- newDisposable "calgary" (error "Some Error Message" >> return ())
--   canada <- concatDisposables "canada" [ vancouver, calgary ]
--   colombia <- newDisposable "colombia" (return ())
--   mexico <- newDisposable "mexico" (return ())
--   earth <- concatDisposables "earth" [ colombia, canada, mexico, venezuela ]

--   result <- dispose earth
--   putDoc $ renderTeardownReport result
