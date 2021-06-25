{-# LANGUAGE OverloadedStrings #-}

module Markup.Tags where

import           Prelude hiding (div)

import           Hakyll.Web.Template (Template, compileTemplateItem)
import           Text.Blaze.Html.Renderer.Pretty (renderHtml)
import           Text.Blaze.Html5 (Html, div, h1, (!))
import           Text.Blaze.Html5.Attributes (class_)
import           Hakyll                        (makeItem, Compiler)
import           Control.Monad                 ( (<=<) )

tagsTemplate :: Compiler Template
tagsTemplate = compileTemplateItem <=< makeItem  . renderHtml $ raw

raw :: Html
raw = do
    h1 "$title$"
    div ! class_ "tags-cloud" $
        "$tagsCloud$"
