{-# LANGUAGE OverloadedStrings #-}

module Markup.Archive (
    archiveTemplate
) where

import           Hakyll.Web.Template
import           Text.Blaze.Html.Renderer.Pretty (renderHtml)
import           Text.Blaze.Html5

import           Control.Monad ((<=<))
import           Hakyll (Compiler, makeItem)
import           Markup.PostList (postList)

archiveTemplate :: Compiler Template
archiveTemplate = compileTemplateItem <=< makeItem  . renderHtml $ raw

raw :: Html
raw = do
    h1 "$title$"
    postList
