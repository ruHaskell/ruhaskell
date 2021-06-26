{-# LANGUAGE OverloadedStrings #-}

module Markup.Authors where

import           Prelude hiding (div)

import           Control.Monad ((<=<))
import           Hakyll (Compiler, makeItem)
import           Hakyll.Web.Template
import           Text.Blaze.Html.Renderer.Pretty (renderHtml)
import           Text.Blaze.Html5
import qualified Text.Blaze.Html5.Attributes as A

authorsTemplate :: Compiler Template
authorsTemplate = compileTemplateItem <=< makeItem  . renderHtml $ raw

raw :: Html
raw = do
    h1 "$title$"
    div ! A.class_ "tags-cloud" $
        "$authorsCloud$"
