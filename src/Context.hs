{-
    Модуль, отвечающий за формирование базового контекста статей.
    https://github.com/ruHaskell/ruhaskell
    Все права принадлежат русскоязычному сообществу Haskell-разработчиков, 2015-2016 г.
-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Context (
    postContext
) where

import           Data.Aeson (FromJSON, Value (Null), parseJSON, withArray)
import           Data.Aeson.Types (parseEither)
import           Data.Foldable (toList)
import qualified Data.HashMap.Strict as HashMap
import           Data.List (intercalate, intersperse, isPrefixOf)
import           Data.Maybe (fromMaybe)
import           Data.Time (TimeLocale (..))
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)

import           Numeric.Natural (Natural)
import           System.FilePath (takeBaseName, takeDirectory)

import           Hakyll (Compiler, Context, Identifier, Item, MonadMetadata,
                         Tags, constField, dateField, dateFieldWith,
                         defaultContext, field, fromFilePath, getMetadata,
                         getRoute, getTags, itemIdentifier, tagsFieldWith,
                         toFilePath, toUrl)
import           Text.Blaze.Html (toHtml, toValue, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import           Misc (TagsAndAuthors, aHost, getNameOfAuthor,
                       getRussianNameOfCategory)

newtype TimePos = TimePos {seconds :: Natural}

instance FromJSON TimePos where
    parseJSON = withArray format $ fromList . toList
      where
        format = "[mm, ss] or [hh, mm, ss]"
        fromList = \case
            [mm, ss]      -> fromHMmSs 0 mm ss
            [hh, mm, ss]  -> do
                h <- parseJSON hh
                fromHMmSs h mm ss
            _             -> fail format
        fromHMmSs h mm ss = do
            m <- parseJSON mm
            s <- parseJSON ss
            pure TimePos{seconds = (h * 60 + m) * 60 + s}

data TalkVideo = TalkVideo
    { youtubeId :: String
    , width :: Maybe Natural
    , start :: Maybe TimePos
    , end :: Maybe TimePos
    }
    deriving (FromJSON, Generic)

data TalkMetadata = TalkMetadata
    { event :: String
    , video :: TalkVideo
    }
    deriving (FromJSON, Generic)

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
    , field                     "talk.video"            talkVideo
    , defaultContext
    ]
  where
    (tags, category, author) = case tagsAndAuthors of
        [t, c, a] -> (t, c, a)
        _         -> error "tags, category and author must be collected"

talkEvent :: HasCallStack => Item a -> Compiler String
talkEvent item = do
    TalkMetadata{event} <- getTalkMetadata iid
    mRoute <- getRoute $ fromFilePath $ "posts/events/" ++ event
    route <- case mRoute of
        Just route -> pure route
        Nothing    -> error $ file ++ ": No route for " ++ event
    pure $ toUrl route
  where
    iid = itemIdentifier item
    file = toFilePath iid

talkVideo :: HasCallStack => Item a -> Compiler String
talkVideo item = do
    TalkMetadata{video} <- getTalkMetadata iid
    pure $ youtubeSnippet video
  where
    iid = itemIdentifier item

getTalkMetadata :: HasCallStack => Identifier -> Compiler TalkMetadata
getTalkMetadata iid@(toFilePath -> file) | "posts/talks/" `isPrefixOf` file = do
    metadata <- getMetadata iid
    either (\e -> error $ file ++ ", metadata.talk: " ++ e) pure $
        parseEither parseJSON $
        fromMaybe Null $
        HashMap.lookup "talk" metadata
getTalkMetadata _ = fail "not a talk"

youtubeSnippet :: TalkVideo -> String
youtubeSnippet TalkVideo{youtubeId, width, start, end} = intercalate "\""
    [ "<iframe width=", showNatural $ fromMaybe defaultWidth width
    , " height=\"400\" src=", url
    , " frameborder=\"0\" allowfullscreen></iframe>"
    ]
  where
    defaultWidth = 712
    url = concat
        [ "https://www.youtube.com/embed/"
        , youtubeId
        , "?"
        , intercalate "&" $ showTimePos "start" start ++ showTimePos "end" end
        ]
    showTimePos name mval =
        [ name ++ "=" ++ showNatural seconds
        | Just TimePos{seconds} <- pure mval
        ]
    showNatural = show :: Natural -> String
