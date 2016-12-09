{-# LANGUAGE OverloadedStrings #-}

module Markup.Posts (
    postsTemplate
) where

import           Markup.PostList                    ( postList )

import           Prelude                            hiding ( div, span )
import           Text.Blaze.Html5
import qualified Text.Blaze.Html5.Attributes        as A
import           Text.Blaze.Html.Renderer.Pretty    ( renderHtml )
import           Hakyll.Web.Template

postsTemplate :: Template
postsTemplate = readTemplate . renderHtml $ raw

raw :: Html
raw = do
    h1 "$title$"
    div ! A.id "taggedPosts" $ postList
