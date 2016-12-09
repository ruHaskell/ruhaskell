{-
    Модуль, отвечающий за построение XML-карты сайта.
    https://github.com/ruHaskell/ruhaskell
    Все права принадлежат русскоязычному сообществу Haskell-разработчиков, 2015-2016 г.
-}

{-# LANGUAGE OverloadedStrings #-}

module XMLMap (
    createXMLMap
) where

import Context              ( postContext )
import Misc                 ( aHost, TagsReader )
import Markup.Sitemap       ( sitemapTemplate )

import Control.Monad.Reader
import Hakyll

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
            makeItem "" >>= applyTemplate sitemapTemplate sitemapContext
    return ()
