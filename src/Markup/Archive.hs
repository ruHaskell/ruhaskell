{-# LANGUAGE OverloadedStrings #-}

module Markup.Archive (
    archiveTemplate
) where

import           Hakyll.Web.Template
import           Text.Blaze.Html.Renderer.Pretty (renderHtml)
import           Text.Blaze.Html5

import           Markup.PostList (postList)
import           Hakyll                        (makeItem, Compiler)
import           Control.Monad                 ( (<=<) )

archiveTemplate :: Compiler Template
archiveTemplate = compileTemplateItem <=< makeItem  . renderHtml $ raw

raw :: Html
raw = do
    h1 "$title$"
    postList
