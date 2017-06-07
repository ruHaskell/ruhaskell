{-# LANGUAGE OverloadedStrings #-}

module Links (
    createLinksPage
) where

import           Control.Monad.Trans (lift)
import           Hakyll              (applyTemplate, compile, constField,
                                      create, defaultContext, idRoute, makeItem,
                                      relativizeUrls, route)

import           Markup.Links        (linksTemplate)
import           Markup.Default      (defaultTemplate)
import           Misc                (TagsReader)

createLinksPage :: TagsReader
createLinksPage =
    lift . create ["links.html"] $ do
        route idRoute
        compile $
            makeItem ""
            >>= applyTemplate linksTemplate indexContext
            >>= applyTemplate defaultTemplate indexContext
            >>= relativizeUrls
  where
    indexContext = mconcat [constField "title" "Полезные ссылки", defaultContext]
