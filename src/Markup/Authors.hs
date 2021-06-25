{-# LANGUAGE OverloadedStrings #-}

module Markup.Authors where

import           Prelude hiding (div)

import           Hakyll.Web.Template
import           Text.Blaze.Html.Renderer.Pretty (renderHtml)
import           Text.Blaze.Html5
import qualified Text.Blaze.Html5.Attributes as A
import           Hakyll                        (makeItem, Compiler)
import           Control.Monad                 ( (<=<) )

authorsTemplate :: Compiler Template
authorsTemplate = compileTemplateItem <=< makeItem  . renderHtml $ raw

raw :: Html
raw = do
    h1 "$title$"
    div ! A.class_ "tags-cloud" $
        "$authorsCloud$"
