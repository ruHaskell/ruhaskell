{-# LANGUAGE OverloadedStrings #-}

module About (
    createAboutPage
) where

import Misc                 ( TagsReader )
import Markup.About         ( aboutTemplate )
import Markup.Default       ( defaultTemplate )

import Control.Monad.Reader
import Hakyll

createAboutPage :: TagsReader
createAboutPage = do
    lift $ create ["about.html"] $ do
        route idRoute
        compile $ do
            let indexContext = mconcat [ constField "title" "О нас"
                                       , defaultContext
                                       ]

            makeItem "" >>= applyTemplate aboutTemplate indexContext
                        >>= applyTemplate defaultTemplate indexContext
                        >>= relativizeUrls
    return ()
