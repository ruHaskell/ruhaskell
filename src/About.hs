{-# LANGUAGE OverloadedStrings #-}

module About (
    createAboutPage
) where

import           Control.Monad.Trans (lift)
import           Hakyll (applyTemplate, compile, constField, create,
                         defaultContext, idRoute, makeItem, relativizeUrls,
                         route)

import           Markup.About (aboutTemplate)
import           Markup.Default (defaultTemplate)
import           Misc (TagsReader)

createAboutPage :: TagsReader
createAboutPage =
    lift . create ["about.html"] $ do
        route idRoute
        compile $ do
            aboutTemp   <- aboutTemplate
            defaultTemp <- defaultTemplate
            makeItem ""
              >>= applyTemplate aboutTemp indexContext
              >>= applyTemplate defaultTemp indexContext
              >>= relativizeUrls
  where
    indexContext = mconcat [constField "title" "О нас", defaultContext]
