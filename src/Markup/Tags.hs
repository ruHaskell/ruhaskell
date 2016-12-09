{-# LANGUAGE OverloadedStrings #-}

module Markup.Tags where

import           Prelude                            hiding ( div, span )
import           Text.Blaze.Html5
import qualified Text.Blaze.Html5.Attributes        as A
import           Text.Blaze.Html.Renderer.Pretty    ( renderHtml )
import           Hakyll.Web.Template

tagsTemplate :: Template
tagsTemplate = readTemplate . renderHtml $ raw

raw :: Html
raw = do
    h1 "$title$"
    div ! A.class_ "tags-cloud" $
        "$tagsCloud$"
