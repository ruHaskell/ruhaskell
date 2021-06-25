{-# LANGUAGE OverloadedStrings #-}

module IndexPage (
    createIndexPage
) where

import Context              ( postContext )
import Misc                 ( TagsReader )
import Markup.Index         ( indexTemplate )
import Markup.Default       ( defaultTemplate )

import Control.Monad.Reader
import Hakyll

createIndexPage :: TagsReader
createIndexPage = do
    tagsAndAuthors <- ask
    lift $ create ["index.html"] $ do
        route idRoute
        compile $ do
            last5Posts <- fmap (take 5) . recentFirst =<< loadAll "posts/**"
            let indexContext = mconcat [ listField "posts" (postContext tagsAndAuthors) (return last5Posts)
                                       , constField "title" "Русскоязычное сообщество Haskell-разработчиков"
                                       , constField "others" "Прочие"
                                       , defaultContext
                                       ]
            indexTemp   <- indexTemplate
            defaultTemp <- defaultTemplate
            makeItem "" >>= applyTemplate indexTemp indexContext
                        >>= applyTemplate defaultTemp indexContext
                        >>= relativizeUrls
    return ()
