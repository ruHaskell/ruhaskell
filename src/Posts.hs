{-
    Модуль, отвечающий за преобразование статей и в формирование корректных путей к ним.
    https://github.com/ruHaskell/ruhaskell
    Все права принадлежат русскоязычному сообществу Haskell-разработчиков, 2015-2016 г.
-}

{-# LANGUAGE OverloadedStrings #-}

module Posts (
    createPosts
) where

import Context              ( postContext )
import Misc                 ( TagsReader )
import Markup.Post          ( postTemplate )
import Markup.Default       ( defaultTemplate )

import Text.Pandoc.Options  ( WriterOptions(..), HTMLMathMethod(..) )
import System.FilePath      ( splitExtension )
import Data.List.Split      ( splitOn )
import Data.List            ( intersperse )
import Control.Monad.Reader
import Hakyll

-- Дата публикации будет отражена в URL в виде подкаталогов.
directorizeDate :: Routes
directorizeDate = customRoute (\i -> directorize $ toFilePath i)
  where
    directorize path = dirs
      where
        (dirs, _) = splitExtension $ concat $ (intersperse "/" date) ++ ["/"] ++ (intersperse "-" rest)
        minusBetweenDateAndTitle = 3
        (date, rest) = splitAt minusBetweenDateAndTitle $ splitOn "-" path

createPosts :: TagsReader
createPosts = do
    tagsAndAuthors <- ask
    lift $ match "posts/**" $ do
        route $ directorizeDate `composeRoutes` setExtension "html"
        -- Для превращения Markdown в HTML используем pandocCompiler
        compile $ pandocCompilerWith defaultHakyllReaderOptions
                                     defaultHakyllWriterOptions { writerHTMLMathMethod = MathJax "" }
              >>= applyTemplate postTemplate    (postContext tagsAndAuthors)
              >>= applyTemplate defaultTemplate (postContext tagsAndAuthors)
              >>= relativizeUrls
    return ()
