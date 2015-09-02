{-
    Модуль, отвечающий за формирование базового контекста статей.
    https://github.com/ruHaskell/ruhaskell
    Все права принадлежат русскоязычному сообществу Haskell-разработчиков, 2015 г.
-}

{-# LANGUAGE OverloadedStrings #-}

module Context (
    postContext
) where

import Data.List        (intersperse)
import Data.Time        (TimeLocale(..))
import Misc             (aHost,
                         TagsAndAuthors,
                         getNameOfAuthor,
                         getRussianNameOfCategory)
import System.FilePath  (takeBaseName, takeDirectory)

import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A
import           Text.Blaze.Html                (toHtml, toValue, (!))

import Hakyll

-- Код данной функции для формирования простой ссылки взят из исходников Hakyll.
simpleRenderLink :: String
                 -> Maybe FilePath
                 -> Maybe H.Html
simpleRenderLink tag = fmap $ \filePath -> -- Формируем тег <a href...>
    H.a ! A.href (toValue $ toUrl filePath) $ toHtml tag

-- Превращает имя автора в ссылку, ведущую к списку статей данного автора.
authorField :: String -> Tags -> Context a
authorField = tagsFieldWith getNameOfAuthor simpleRenderLink (mconcat . intersperse ", ")

-- Оборачиваем ссылку-тег в программерские кавычки, чтобы было как в Haskell-коде. ;-)
simpleRenderQuottedLink :: String
                        -> Maybe FilePath
                        -> Maybe H.Html
simpleRenderQuottedLink tag = fmap $ \filePath -> -- Формируем тег <a href...>
    let rawHref = H.a ! A.href (toValue $ toUrl filePath) $ toHtml tag
        quote = toHtml ("\"" :: String)
    in quote >> rawHref >> quote

-- Превращает имя ссылки в ссылку, ведущую к списку статей данного автора.
quottedTagField :: String
                -> Tags
                -> Context a
quottedTagField = tagsFieldWith getTags simpleRenderQuottedLink (mconcat . intersperse ", ")

-- Формируем ссылку, конвертируя "родное файловое" имя категории в русскоязычный аналог...
simpleRenderLinkForRussianCategory :: String
                                   -> Maybe FilePath
                                   -> Maybe H.Html
simpleRenderLinkForRussianCategory tag = fmap $ \filePath ->
    H.a ! A.href (toValue $ toUrl filePath) $ toHtml (getRussianNameOfCategory tag)

-- Код данной функции, извлекающей имя категории из файлового пути, взят из исходников Hakyll.
getCategory :: MonadMetadata m => Identifier -> m [String]
getCategory = return . return . takeBaseName . takeDirectory . toFilePath

-- Превращает имя категории в русскоязычную ссылку, ведущую к списку статей, входящих в данную категорию.
categoryFieldInRussian :: String -> Tags -> Context a
categoryFieldInRussian = tagsFieldWith getCategory simpleRenderLinkForRussianCategory (mconcat . intersperse ", ")

-- Локализация в данном случае задаётся только для русских названий месяцев.
-- Остальные поля типа TimeLocale инициализированы пустыми значениями.
ruTimeLocale :: TimeLocale
ruTimeLocale =  TimeLocale { wDays  = []
                           , months = [("января",   "jan"),  ("февраля", "feb"),
                                       ("марта",    "mar"),  ("апреля",  "apr"),
                                       ("мая",      "may"),  ("июня",    "jun"),
                                       ("июля",     "jul"),  ("августа", "aug"),
                                       ("сентября", "sep"),  ("октября", "oct"),
                                       ("ноября",   "nov"),  ("декабря", "dec")]
                           , knownTimeZones = []
                           , amPm = ("", "")
                           , dateTimeFmt = ""
                           , dateFmt = ""
                           , timeFmt = ""
                           , time12Fmt = ""
                           }

-- Основной контекст публикаций.
postContext :: TagsAndAuthors -> Context String
postContext tagsAndAuthors = mconcat [ constField "host" aHost
                                     , dateFieldWith ruTimeLocale "date" "%d %B %Y"
                                     , dateFieldWith ruTimeLocale "haskellDate" "%Y %b %d"
                                     , dateField "issuePubDateInRFC2822" "%a, %_d %b %Y %H:%M:%S +0300"
                                     , quottedTagField "postTags" $ head tagsAndAuthors
                                     , categoryFieldInRussian "postCategory" $ tagsAndAuthors !! 1
                                     , authorField "postAuthor" $ tagsAndAuthors !! 2
                                     , defaultContext
                                     ]
