{-
    Модуль, отвечающий за работу с RSS. 
    https://github.com/denisshevchenko/ruhaskell
    Все права принадлежат русскоязычному сообществу Haskell-разработчиков, 2015 г.
-}

{-# LANGUAGE OverloadedStrings #-}

module RSSFeed (
    setupRSSFeed
) where

import Data.Monoid          (mconcat)
import Misc                 (aHost)
import Context              (postContext)
import Misc                 (TagsReader)
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

