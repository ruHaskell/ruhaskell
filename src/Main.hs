{-
    Главный модуль.
    https://github.com/ruHaskell/ruhaskell
    Все права принадлежат русскоязычному сообществу Haskell-разработчиков, 2015-2016 г.
-}

{-# LANGUAGE OverloadedStrings #-}

module Main where

import           About                (createAboutPage)
import           Archive              (createPageWithAllPosts)
import           Copiers              (justCompressAndCopy, justCopy,
                                       justCreateAndCopy)
import           IndexPage            (createIndexPage)
import           Misc                 (prepareAllTemplates)
import           Posts                (createPosts)
import           RSSFeed              (setupRSSFeed)
import           Tags                 (buildPostsAuthors, buildPostsCategories,
                                       buildPostsTags, convertAuthorsToLinks,
                                       convertCategoriesToLinks,
                                       convertTagsToLinks,
                                       createPageWithAllAuthors,
                                       createPageWithAllCategories,
                                       createPageWithAllTags)
import           XMLMap               (createXMLMap)

import           Control.Monad.Reader (runReaderT)
import           Hakyll               (hakyll)

main :: IO ()
main = hakyll $ do
    justCopy            "files/**"
    justCopy            "static/images/*"
    justCompressAndCopy "static/css/*"
    justCopy            "README.md"
    justCopy            "CNAME"
    justCreateAndCopy   ".nojekyll"

    prepareAllTemplates

    -- Извлекаем названия тегов, категорий, а также имена авторов из всех публикаций.
    tags        <- buildPostsTags
    categories  <- buildPostsCategories
    authors     <- buildPostsAuthors

    -- Теги и имена авторов нужны всем, поэтому для удобства запускаем читателя.
    (`runReaderT` [tags, categories, authors]) $ do
        createPosts
        createPageWithAllPosts
        createPageWithAllTags
        createPageWithAllCategories
        createPageWithAllAuthors
        convertTagsToLinks
        convertCategoriesToLinks
        convertAuthorsToLinks
        createXMLMap
        setupRSSFeed
        createIndexPage
        createAboutPage
