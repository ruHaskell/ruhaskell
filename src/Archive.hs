{-# LANGUAGE OverloadedStrings #-}

module Archive (
    createPageWithAllPosts
) where

import Context              ( postContext )
import Misc                 ( TagsReader )
import Markup.Archive       ( archiveTemplate )
import Markup.Default       ( defaultTemplate )

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
                                         , constField "title" "Все статьи"
                                         , defaultContext
                                         ]
            archiveTemp <- archiveTemplate
            defaultTemp <- defaultTemplate
            makeItem "" >>= applyTemplate archiveTemp archiveContext
                        >>= applyTemplate defaultTemp archiveContext
                        >>= relativizeUrls
    return ()
