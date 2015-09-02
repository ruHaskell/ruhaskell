{-
    Модуль, отвечающий за работу с RSS.
    https://github.com/ruHaskell/ruhaskell
    Все права принадлежат русскоязычному сообществу Haskell-разработчиков, 2015 г.
-}

{-# LANGUAGE OverloadedStrings #-}

module RSSFeed (
    setupRSSFeed,
    setupITunesRSSFeed
) where

import Misc                 (aHost, TagsReader)
import Context              (postContext)
import Control.Monad.Reader
import Hakyll

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration { feedTitle       = "ruHaskell"
                                      , feedDescription = "Русскоязычное сообщество Haskell-разработчиков"
                                      , feedAuthorName  = "Денис Шевченко"
                                      , feedAuthorEmail = "me@dshevchenko.biz"
                                      , feedRoot        = aHost
                                      }

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
            -- Учитываем 10 последних статей.
            last10Posts <- fmap (take 10) . recentFirst =<< loadAll "posts/**"
            renderRss feedConfiguration
                      feedContext
                      last10Posts
    return ()

-- Формируем RSS-ленту подкаста для iTunes, на основе последних 10 выпусков.
setupITunesRSSFeed :: TagsReader
setupITunesRSSFeed = do
    tagsAndAuthors <- ask
    lift $ create ["itunes-feed.xml"] $ do
        route idRoute
        compile $ do
            allIssues <- recentFirst =<< loadAll "posts/cast/*"
            let iTunesRSSContext = mconcat [ listField "issues" (postContext tagsAndAuthors) (return allIssues)
                                           , defaultContext
                                           ]
            -- Формируем страницу itunes-feed.xml, применяя к ней шаблон и контекст.
            makeItem "" >>= loadAndApplyTemplate "templates/itunes-feed.xml" iTunesRSSContext
    return ()
