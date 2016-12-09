{-# LANGUAGE OverloadedStrings #-}

module Markup.Index (
    indexTemplate
) where

import           Markup.PostList                    ( postList )

import           Prelude                            hiding ( div, span )
import           Text.Blaze.Html5
import qualified Text.Blaze.Html5.Attributes        as A
import           Text.Blaze.Html.Renderer.Pretty    ( renderHtml )
import           Hakyll.Web.Template

indexTemplate :: Template
indexTemplate = readTemplate . renderHtml $ raw

raw :: Html
raw = do
    h1 "Статьи"
    postList
    div ! A.class_ "archive-button" $ 
        a ! A.class_ "btn btn-outline-primary"
          ! A.href "/archive.html"
          ! customAttribute "role" "button" $ do
            span "$others$"
            preEscapedToHtml ("&nbsp;" :: String)
            i ! A.class_ "fa fa-angle-double-right"
              ! customAttribute "aria-hidden" "true" $ ""
