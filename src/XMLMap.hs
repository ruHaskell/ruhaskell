{-
    Модуль, отвечающий за построение XML-карты сайта.
    https://github.com/ruHaskell/ruhaskell
    Все права принадлежат русскоязычному сообществу Haskell-разработчиков, 2015-2016 г.
-}

{-# LANGUAGE OverloadedStrings #-}

module XMLMap (
    createXMLMap
) where

import           Control.Monad.Reader
import           Hakyll

import           Context (postContext)
import           Markup.Sitemap (sitemapTemplate)
import           Misc (TagsReader, aHost)


createXMLMap :: TagsReader
createXMLMap = do
    tagsAndAuthors <- ask
    lift $ create ["sitemap.xml"] $ do
        route idRoute
        compile $ do
            allPosts <- recentFirst =<< loadAll "posts/**"
            let sitemapContext = mconcat [ listField "entries" (postContext tagsAndAuthors) (return allPosts)
                                         , constField "host" aHost
                                         , defaultContext
                                         ]
            sitemapTemp <- sitemapTemplate
            makeItem "" >>= applyTemplate sitemapTemp sitemapContext
    return ()
