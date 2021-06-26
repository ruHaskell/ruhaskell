{-# LANGUAGE OverloadedStrings #-}

module RSSFeed (
    setupRSSFeed
) where

import           Context (postContext)
import           Misc (TagsReader, aHost)

import           Control.Monad.Reader
import           Hakyll

-- Формируем стандартную RSS-ленту, на основе последних 10 публикаций.
setupRSSFeed :: TagsReader
setupRSSFeed = do
    tagsAndAuthors <- ask
    lift $ create ["feed.xml"] $ do
        route idRoute
        compile $ do
            let feedContext = mconcat [ postContext tagsAndAuthors
                                      , constField "description" ""
                                      ]
            last10Posts <- fmap (take 10) . recentFirst =<< loadAll "posts/**"
            renderRss feedConfiguration
                      feedContext
                      last10Posts
    return ()
  where
    feedConfiguration = FeedConfiguration { feedTitle       = "ruHaskell"
                                          , feedDescription = "Русскоязычное сообщество Haskell-разработчиков"
                                          , feedAuthorName  = "Денис Шевченко"
                                          , feedAuthorEmail = "me@dshevchenko.biz"
                                          , feedRoot        = aHost
                                          }
