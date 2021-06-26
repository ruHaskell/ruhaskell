{-# LANGUAGE OverloadedStrings #-}

module Misc
    ( aHost
    , prepareAllTemplates
    , getNameOfAuthor
    , TagsAndAuthors
    , TagsReader
    , getRussianNameOfCategory
    ) where

import           Control.Monad.Reader (ReaderT)
import           Data.Aeson (Value (String))
import qualified Data.HashMap.Lazy as HashMap
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as Text
import           Hakyll (Identifier, MonadMetadata, Rules, Tags, compile,
                         getMetadata, match, templateCompiler, trim)

-- | Данный URL останется актуальным до тех пор, пока сайт будет жить на GitHub Pages.
aHost :: String
aHost = "https://ruhaskell.org"

-- | Готовим все шаблоны из каталога templates.
prepareAllTemplates :: Rules ()
prepareAllTemplates = match "templates/*" $ compile templateCompiler

-- | Читательское "облако" с тематическими тегами, категориями и именами авторов статей.
type TagsAndAuthors = [Tags]

type TagsReader = ReaderT TagsAndAuthors Rules ()

-- | Извлекает из статьи значение атрибута `author`.
getNameOfAuthor :: (MonadMetadata m, MonadFail m) => Identifier -> m [String]
getNameOfAuthor identifier = do
    metadata <- getMetadata identifier
        -- Собираем атрибуты статьи в обычный ассоциативный контейнер.
    author <- case HashMap.lookupDefault "Не указан" "author" metadata of
        String author -> pure author
        _             -> fail "expected String"
    return [trim $ Text.unpack author]

-- | Имена категорий извлекаются из файлового пути, поэтому они всегда английские.
-- Это не красиво, поэтому мы формируем словарь русских имён для категорий.
russianNamesOfCategories :: Map String String
russianNamesOfCategories = Map.fromList
    [ ("algorithms" , "Алгоритмы")
    , ("cast"       , "Подкаст")
    , ("dynamic"    , "Динамика")
    , ("elm"        , "Elm")
    , ("events"     , "События")
    , ("extensions" , "Расширения")
    , ("frp"        , "FRP")
    , ("gui"        , "GUI")
    , ("numeric"    , "Численные методы")
    , ("packages"   , "Пакеты")
    , ("projects"   , "Проекты")
    , ("talks"      , "Выступления")
    , ("tasks"      , "Задачи")
    , ("theory"     , "Теория")
    , ("typesystem" , "Система типов")
    , ("utils"      , "Утилиты")
    , ("web"        , "Веб")
    ]

getRussianNameOfCategory :: String -> String
getRussianNameOfCategory englishName =
    Map.findWithDefault englishName
                        englishName
                        russianNamesOfCategories
