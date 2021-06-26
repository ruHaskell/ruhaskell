{-# LANGUAGE OverloadedStrings #-}

module Markup.Posts (
    postsTemplate
) where

import           Prelude hiding (div)

import           Hakyll.Web.Template
import           Text.Blaze.Html.Renderer.Pretty (renderHtml)
import           Text.Blaze.Html5
import qualified Text.Blaze.Html5.Attributes as A

import           Control.Monad ((<=<))
import           Hakyll (Compiler, makeItem)
import           Markup.PostList (postList)

postsTemplate :: Compiler Template
postsTemplate = compileTemplateItem <=< makeItem  . renderHtml $ raw

raw :: Html
raw = do
    h1 "$title$"
    div ! A.id "taggedPosts" $ postList
