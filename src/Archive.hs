{-
    Модуль, отвечающий за формирование архива (списка всех статей).
    https://github.com/denisshevchenko/ruhaskell
    Все права принадлежат русскоязычному сообществу Haskell-разработчиков, 2015 г.
-}

{-# LANGUAGE OverloadedStrings #-}

module Archive (
    createPageWithAllPosts
) where

import Data.Monoid          (mconcat)
import Context              (postContext)
import Misc                 (TagsReader)
import Control.Monad.Reader
import Hakyll

createPageWithAllPosts :: TagsReader
createPageWithAllPosts = do
    tagsAndAuthors <- ask
    lift $ create ["archive.html"] $ do
        route idRoute
        compile $ do
            allPosts <- recentFirst =<< loadAll "posts/**"
            let archiveContext = mconcat [ listField "posts" (postContext tagsAndAuthors) (return allPosts) 
                                         , constField "title" "Архив статей"                   
                                         , defaultContext
                                         ]

            makeItem "" >>= loadAndApplyTemplate "templates/archive.html" archiveContext
                        >>= loadAndApplyTemplate "templates/default.html" archiveContext
                        >>= relativizeUrls
    return ()

