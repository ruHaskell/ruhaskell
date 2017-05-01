{-
    Модуль, отвечающий за формирование базового контекста статей.
    https://github.com/ruHaskell/ruhaskell
    Все права принадлежат русскоязычному сообществу Haskell-разработчиков, 2015-2016 г.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Context (
    postContext
) where

import           Data.Aeson                  (Value (Object, String))
import qualified Data.HashMap.Strict         as HashMap
import           Data.List                   (intersperse, isPrefixOf)
import qualified Data.Text                   as Text
import           Data.Time                   (TimeLocale (..))
import           GHC.Stack                   (HasCallStack)
import           Misc                        (TagsAndAuthors, aHost,
                                              getNameOfAuthor,
                                              getRussianNameOfCategory)
import           System.FilePath             (takeBaseName, takeDirectory)

import           Text.Blaze.Html             (toHtml, toValue, (!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

import           Hakyll                      (Compiler, Context, Identifier,
                                              Item, MonadMetadata, Tags,
                                              constField, dateField,
                                              dateFieldWith, defaultContext,
                                              field, fromFilePath, getMetadata,
                                              getRoute, getTags, itemIdentifier,
                                              tagsFieldWith, toFilePath, toUrl)

-- | Код данной функции для формирования простой ссылки взят из исходников Hakyll.
simpleRenderLink :: String -> Maybe FilePath -> Maybe H.Html
simpleRenderLink tag = fmap $ \filePath -> -- Формируем тег <a href...>
    H.a ! A.href (toValue $ toUrl filePath) $ toHtml tag

-- | Превращает имя автора в ссылку, ведущую к списку статей данного автора.
authorField :: String -> Tags -> Context a
authorField =
    tagsFieldWith getNameOfAuthor simpleRenderLink (mconcat . intersperse ", ")

-- | Оборачиваем ссылку-тег в программерские кавычки, чтобы было как в Haskell-коде. ;-)
simpleRenderQuottedLink :: String -> Maybe FilePath -> Maybe H.Html
simpleRenderQuottedLink tag = fmap $ \filePath -> -- Формируем тег <a href...>
    let rawHref = H.a ! A.href (toValue $ toUrl filePath) $ toHtml tag
        quote = "\"" :: H.Html
    in quote >> rawHref >> quote

-- | Превращает имя ссылки в ссылку, ведущую к списку статей данного автора.
quottedTagField :: String -> Tags -> Context a
quottedTagField =
    tagsFieldWith getTags simpleRenderQuottedLink (mconcat . intersperse ", ")

-- | Формируем ссылку, конвертируя "родное файловое" имя категории в русскоязычный аналог...
simpleRenderLinkForRussianCategory :: String -> Maybe FilePath -> Maybe H.Html
simpleRenderLinkForRussianCategory tag =
    fmap $ \filePath ->
        H.a ! A.href (toValue $ toUrl filePath) $
            toHtml (getRussianNameOfCategory tag)

-- | Код данной функции, извлекающей имя категории из файлового пути, взят из исходников Hakyll.
getCategory :: MonadMetadata m => Identifier -> m [String]
getCategory = return . return . takeBaseName . takeDirectory . toFilePath

-- | Превращает имя категории в русскоязычную ссылку, ведущую к списку статей,
-- входящих в данную категорию.
categoryFieldInRussian :: String -> Tags -> Context a
categoryFieldInRussian =
    tagsFieldWith
        getCategory
        simpleRenderLinkForRussianCategory
        (mconcat . intersperse ", ")

-- | Локализация в данном случае задаётся только для русских названий месяцев.
-- Остальные поля типа TimeLocale инициализированы пустыми значениями.
ruTimeLocale :: TimeLocale
ruTimeLocale =  TimeLocale
    { wDays = []
    , months =
          [ ("января",  "jan"), ("февраля", "feb"), ("марта",    "mar")
          , ("апреля",  "apr"), ("мая",     "may"), ("июня",     "jun")
          , ("июля",    "jul"), ("августа", "aug"), ("сентября", "sep")
          , ("октября", "oct"), ("ноября",  "nov"), ("декабря",  "dec")
          ]
    , knownTimeZones = []
    , amPm = ("", "")
    , dateTimeFmt = ""
    , dateFmt = ""
    , timeFmt = ""
    , time12Fmt = ""
    }

-- | Основной контекст публикаций.
postContext :: HasCallStack => TagsAndAuthors -> Context String
postContext tagsAndAuthors = mconcat
    [ constField                "host"                  aHost
    , dateFieldWith             ruTimeLocale            "date" "%d %B %Y"
    , dateFieldWith             ruTimeLocale            "haskellDate" "%Y %b %d"
    , dateField                 "issuePubDateInRFC2822" "%a, %_d %b %Y %H:%M:%S +0300"
    , quottedTagField           "postTags"              tags
    , categoryFieldInRussian    "postCategory"          category
    , authorField               "postAuthor"            author
    , field                     "talk.event"            talkEvent
    , defaultContext
    ]
  where
    [tags, category, author] = tagsAndAuthors

talkEvent :: HasCallStack => Item a -> Compiler String
talkEvent (itemIdentifier -> iid@(toFilePath -> file))
        | "posts/talks/" `isPrefixOf` file = do
    metadata <- getMetadata iid
    talkMetadata <-
        case HashMap.lookup "talk" metadata of
            Just (Object talkMetadata) -> pure talkMetadata
            r -> error $ file ++ ": $.talk: expected Object, got " ++ show r
    eventFile <-
        case HashMap.lookup "event" talkMetadata of
            Just (String eventFile) -> pure $ Text.unpack eventFile
            r -> error $ file ++ ": $.talk.event: expected String, got " ++ show r
    mRoute <- getRoute $ fromFilePath $ "posts/events/" ++ eventFile
    route <- case mRoute of
        Just route -> pure route
        Nothing    -> error $ file ++ ": No route for " ++ eventFile
    pure $ toUrl route
talkEvent _ = fail "not a talk"
