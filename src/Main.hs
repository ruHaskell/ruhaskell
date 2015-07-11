{-
    Главный модуль.
    https://github.com/ruHaskell/ruhaskell
    Все права принадлежат русскоязычному сообществу Haskell-разработчиков, 2015 г.
-}

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Copiers              (justCopy, justCreateAndCopy, justCompressAndCopy)
import RSSFeed              (setupRSSFeed, setupITunesRSSFeed)
import Posts                (createPosts)
import Tags                 (createPageWithAllTags,
                             createPageWithAllCategories,
                             createPageWithAllAuthors,
                             convertTagsToLinks,
                             convertCategoriesToLinks,
                             convertAuthorsToLinks,
                             buildPostsTags,
                             buildPostsAuthors,
                             buildPostsCategories)
import XMLMap               (createXMLMap)
import Archive              (createPageWithAllPosts)
import Misc                 (prepareAllTemplates)
import IndexPage            (createIndexPage)
import Links                (createPageWithExternalLinks)
import MailingList          (createMailingListFrontend)
import Control.Monad.Reader (runReaderT)
import Hakyll

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
    runReaderT (createPosts
                >> createPageWithAllPosts
                >> createPageWithAllTags
                >> createPageWithAllCategories
                >> createPageWithAllAuthors
                >> convertTagsToLinks
                >> convertCategoriesToLinks
                >> convertAuthorsToLinks
                >> createXMLMap
                >> setupRSSFeed
                >> setupITunesRSSFeed
                >> createIndexPage
                >> createMailingListFrontend
                >> createPageWithExternalLinks) [tags, categories, authors]

