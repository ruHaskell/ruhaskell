{-# LANGUAGE OverloadedStrings #-}

module Markup.Posts (
    postsTemplate
) where

import           Prelude hiding (div)

import           Hakyll.Web.Template
import           Text.Blaze.Html.Renderer.Pretty (renderHtml)
import           Text.Blaze.Html5
import qualified Text.Blaze.Html5.Attributes as A

import           Markup.PostList (postList)

postsTemplate :: Template
postsTemplate = readTemplate . renderHtml $ raw

raw :: Html
raw = do
    h1 "$title$"
    div ! A.id "taggedPosts" $ postList
