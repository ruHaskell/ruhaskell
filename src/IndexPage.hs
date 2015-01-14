{-
    Модуль, отвечающий за формирование главной страницы.
    https://github.com/denisshevchenko/ruhaskell
    Все права принадлежат русскоязычному сообществу Haskell-разработчиков, 2015 г.
-}

{-# LANGUAGE OverloadedStrings #-}

module IndexPage (
    createIndexPage
) where

import Data.Monoid          (mconcat)
import Context              (postContext)
import Misc                 (TagsReader)
import Control.Monad.Reader
import Hakyll

createIndexPage :: TagsReader
createIndexPage = do
    tagsAndAuthors <- ask
    lift $ create ["index.html"] $ do
        route idRoute
        compile $ do
            -- На главной странице будет отражено 7 последних публикаций.
            last7Posts <- fmap (take 7) . recentFirst =<< loadAll "posts/**"
            let indexContext = mconcat [ listField "posts" (postContext tagsAndAuthors) (return last7Posts) 
                                       , constField "title" "Русскоязычное сообщество Haskell-разработчиков"
                                       , defaultContext
                                       ]
            
            makeItem "" >>= loadAndApplyTemplate "templates/index.html" indexContext
                        >>= loadAndApplyTemplate "templates/default.html" indexContext
                        >>= relativizeUrls
    return ()

