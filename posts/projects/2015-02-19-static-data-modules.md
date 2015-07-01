---
title: Генерация модулей со статичными данными
author: Константин Зудов
tags:   данные, AST, метапрограммирование
description: Генерируем модуль, содержаший HTML5-мнемоники
---

# Задача

При работе над парсером [CommonMark](http://commonmark.org) мне было необходимо
парсить именованные [HTML мнемоники](https://ru.wikipedia.org/wiki/Мнемоники_в_HTML).
Если опустить неважные для данного примера детали, спецификация CommonMark требует
следующих правил при разборе мнемоник:

- Именованная мнемоника состоит из символа `&` + любое корректное для HTML5 имя
  мнемоники + символ `;`. Например `&copy;`.
- Строки не находящиеся в списке корректных мнемоник не признаются мнемониками
  при разборе.
- [Следующий JSON файл](https://html.spec.whatwg.org/multipage/entities.json)
  является aвторитетным источником корректных имён и соответствующих им Unicode-кодов
- С целью сделать формат независимы от HTML все корректные мнемоники должны быть 
  конвертированны в Unicode-знаки.
- Некорректные мнемоники должны быть оставлены в тексте неизмененными

Эта статья не о парсинге CommonMark, но при решении задачи описанной выше
возникает следующая проблема: в процессе парсинга нам нужно консультироваться
со списком корректных строк и в случае если разбираемая строка является корректной
мнемоникой заменять её на соответствующие Unicode-знаки.

Как отмечено выше, источник корректных мнемоник является JSON файлом, размером 144K.
И удерживать его в памяти используя подходящую структуру данных (например Map)
не составит никакого труда.

Если бы мы писали на javascript, то можно было бы просто подключить этот файл
и ходить в него как в обычный js-объект. Но к счастливому сожалению мы пишем на
Хаскелле и наш блистательный компилятор категорически откажется компилировать JSON
файл.

# Наивное решение

Один из возможных вариантов решения, хранить где-нибудь наш JSON файл и читать его при
запуске парсера. Или распарсить один раз в нужную структуру, а потом 
сериализировать в бинарный файл из которого загружать. Но мне такой подход не
нравится по определённым причинам:

- необходимость таскать этот файл с собой
- небходимость загружать его при запуске
- Некрасивость решения. В самом деле, нам всего лишь нужно работать со
  статичной структурой данных которая не так уж и часто меняется, зачем тут
  вообще какой-то IO?
- Необходимость в дополнительных зависимостях. В моем парсере вовсе не нужна
  библиотека для чтения JSON, зачем её тащить только ради того чтобы прочитать
  файл со списком мнемоник.

# Как сделать лучше?

Очевидное решение которое избавлено от всех вышеперечисленных минусов пришло
быстро: нужно засунуть все эти данные в обычный хаскелльный модуль, в виде
большого списка и спокойно использовать его из любого модуля моей программы.

Такое решение можно применить во многих других приложениях. К примеру, в 
веб-приложении мы можем захотеть предоставить пользователю выбор города в котором он
живёт в зависимости от выбранной страны и области. Обычно для этого ходят в
базу данных, но разве это необходимо если список городов небольшой и помещается
в память?

Если честно, моим первым порывом было открыть файл в vim и записать хитрый 
макрос который отформатируют данные как мне надо. Подумав и осознав провальность
этой идеи я подумал, что можно воспользоваться неплохим инструментом [jq](http://stedolan.github.io/jq/).
Не менее глупая идея.

Первая достойная идея заключалась в том, что распарсить файл из ghci, а потом
скопировать получившийся список и вставить в мой модуль. Вполне реально, быстро,
но так же некрасиво.

В конце концов я пришёл к выводу, что подходящим решением будет написать небольшой
скрипт на Хаскелле, который распарсит JSON файл, а затем полностью сгенерирует
желаемый модуль.

Почему отдельный скрипт, а не Template Haskell внутри программы? Смотрите
выше -- необходимость в дополнительных зависимостях на template-haskell и aeson.

Так как задача разбора мнемоник вполне общая, я решил сделать из этого библиотеку.

Итак, приступим. Библиотека будет содержать два модуля: `Text.Html5.Entity.Data` --
автоматически генерируемый модуль содержащий только данные, и `Text.Html5.Entity` --
файл содержащий удобные функции для работы с этими данными.

# Пишем код который напишет код

Начнем с автогенерируемого модуля. Вначале некоторые прагмы и несколько импортов.

```haskell
-- file: generation/Generate.hs
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

import           Control.Applicative ((<$>))
import           Data.Aeson
import qualified Data.ByteString.Lazy as BS
import qualified Data.HashMap.Lazy as HM
```

Для начала распарсим JSON файл в список тупл `[(String, [Int])]`, где `String`
описывает мнемонику, а [Int] соответствующий список Unicode-кодов.

```haskell
newtype EntityVal = EntityVal { codepoints :: [Integer] } deriving (Show, Eq)
instance FromJSON EntityVal where
    parseJSON (Object o) = EntityVal <$> o .: "codepoints"
    parseJSON _ = error "Not an object"

type Entities = [(String, [Int])]

main :: IO ()
main = do
    entFile <- BS.readFile "entities.json"
    let Just (entMap :: Entities) = HM.toList . HM.map codepoints <$> decode entFile
```

Если вы знакомы с пакетом [Aeson](https://hackage.haskell.org/package/aeson),
то все должно быть понятно. В противном случае не обращайте внимание это не самая
важная часть статьи, важно то что мы прочитали файл "entities.json" и распарсили
его в нужные нам данные в нужном нам виде.

Стоит отметить, что я намеренно не извлекал из JSON файла поля "characters", так
как имея Unicode-код соответствующий ему знак можно получить с помощью функции
`Data.Char.chr`.

Итак. Данные у нас есть, теперь нужно сгенерировать код который будет их содержать.

Для этого есть немного устаревшая, но прекрасно подходящая библиотека
[`haskell-src`](https://hackage.haskell.org/package/haskell-src/) которая
позволяет парсить и печать код стандарта Haskell98 в и из AST(Abstract Syntax Tree)
соответственно.

Из этой библиотеке нам понадобиться несколько модулей, так что:

```haskell
import Language.Haskell.Syntax -- описание AST
import Language.Haskell.Pretty -- модуль для печати сгенерированного кода
import Language.Haskell.Parser -- модуль для разбора существующего кода
```

## А зачем нам Parser, мы же вроде генерировать собирались?
Дело в том, что запись кода в форме AST достаточна многословна. Оцените сами,
в форме AST вот эти простые объявления импортов,

```haskell
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
```
будут выглядеть вот так:
```haskell
[ HsImportDecl noloc (Module "Data.Map") True (Just $ Module "M")
               (Just (False, [ HsIVar $ HsIdent "fromList"
                             , HsIAbs $ HsIdent "Map"]))
, HsImportDecl noloc (Module "Data.Set") True (Just $ Module "S")
               (Just (False, [ HsIVar $ HsIdent "fromList"
                             , HsIAbs $ HsIdent "Set"]))]
```

Я ленив и поэтому решил сделать шаблон `Template.hs` с содержимым в автоматической 
генерации которого я не нуждаюсь, а потом распарсить этот шаблон и дополнить его 
сгенерированным содержимым.

Вот сам шаблон, содержащий все необходимые импорты и объявления типов:
```haskell
-- file: Template.hs
module Text.Html5.Entity.Data where

import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S

entityMap :: Map String [Int]
entitySet :: Set String
```

Как видите я объявил две функции `entityMap` и `entitySet`. `entityMap` это просто 
`Map` со строками-мнемониками в качестве ключей и списками знаков в качестве значений.
`entitySet` это `Set` содержащий только строки-мнемоники для того чтобы можно 
было быстро проверять корректность мнемоник.

Чуть позже я покажу как распарсить этот шаблон и дополнить его сгенерированными
данными. Но вначале нужно сгенерировать эти данные.

AST хаскелла достаточно объемный, но разобраться с ним не составит труда если
следовать [документации](https://hackage.haskell.org/package/haskell-src-1.0.2.0/docs/Language-Haskell-Syntax.html)
на haddock.

Начнём с определения AST функции `entitySet`. Для начала напишем прототип на нашем
привычном языке:
```haskell
entitySet = S.fromList []
```
В списке который я оставил пустым должны быть все наши 2000+ мнемоник.

Главная часть объявления нашей функции это выражение `S.fromList []`, вот как
оно будет выглядеть в форме AST:

```haskell
HsApp (HsVar $ Qual (Module "S") $ HsIdent "fromList") (HsList [])
```

Если разбить это выражение на части, то оно состоит из применения функции `S.fromList`
к пустому списку. Думаю у вас не составит проблемы соотнести эти части с AST
приведённым выше.

Но нам не нужно выражение возвращающее пустой `Set`, нам нужен `Set` с нашими
мнемониками. Объявим функцию `mkEntitySet :: Entities -> HsExp` которая
принимает список мнемоник, и возвращает выражение с `Set`-ом этих мнемоник:

```haskell
mkEntitySet :: Entities -> HsExp
mkEntitySet ents = HsApp (HsVar $ Qual (Module "S") $ HsIdent "fromList")
                         (HsList $ map (HsLit . HsString . fst) ents)
```

Ничего очень нового, разве что вместо пустого списка мы имеем `map (HsLit . HsString . fst) ents`.
Которое превращает список мнемоник `ents` в список строковых литералов.

Функция mkEntityMap которая делает аналогичное для `entityMap` не намного сложнее:
```haskell
mkEntityMap :: Entities -> HsExp
mkEntityMap ents = HsApp (HsVar $ Qual (Module "M") $ HsIdent "fromList")
                         (HsList $ map mkMapElemTup ents)

mkMapElemTup :: (String, [Integer]) -> HsExp
mkMapElemTup (name, codes) =
    HsTuple [ HsLit $ HsString name
            , HsList $ map (HsLit . HsInt) codes
            ] 
```

Но постойте, ведь `mkEntitySet` и `mkEntityMap` это всего-лишь объявления выражений
мы так и не привязали их к именам функций. Привяжем же:

```haskell
decls :: Entities -> [HsDecl]
decls ents =
    [ HsFunBind [HsMatch noloc (HsIdent "entityMap") []
                               (HsUnGuardedRhs (mkEntityMap ents)) []]
    , HsFunBind [HsMatch noloc (HsIdent "entitySet") []
                               (HsUnGuardedRhs (mkEntitySet ents)) []]
    ]

noloc = SrcLoc "" 0 0
```

За исключением небольшой (оправданной) многословности, все просто. `SrcLoc`
указывает на местоположение объявления в файле, но при генерации кода оно не имеет
значения.

Похоже что мы закончили с генерацией данных. Все что осталось это распарсить
шаблон, дополнить его нашими определениями и записать это все в файл. Здесь все
ещё проще. Дополним нашу `main` функцию:

```haskell
main = do
    entFile <- BS.readFile "generation/entities.json"
    let Just entMap = HM.toList . HM.map codepoints <$> decode entFile
    ParseOk template <- parseModule <$> readFile "generation/Template.hs"
    writeFile "src/Text/Html5/Entity/Data.hs" $ prettyPrint 
                                              $ appendTemplate template entMap

appendTemplate :: HsModule -> Entities -> HsModule
appendTemplate (HsModule srcLoc modName exports imports decls') ents =
    HsModule srcLoc modName exports imports (decls' ++ decls ents)
```

На этом наш код для генерации модуля с данными готов. Вот его полный исходник:
```haskell
{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative ((<$>))
import           Data.Aeson
import qualified Data.ByteString.Lazy as BS
import qualified Data.HashMap.Lazy as HM

import Language.Haskell.Syntax
import Language.Haskell.Pretty
import Language.Haskell.Parser

newtype EntityVal = EntityVal { codepoints :: [Integer] } deriving (Show, Eq)
instance FromJSON EntityVal where
    parseJSON (Object o) = EntityVal <$> o .: "codepoints"
    parseJSON _ = error "Not an object"

type Entity = [(String, [Int])]

main :: IO ()
main = do
    entFile <- BS.readFile "generation/entities.json"
    let Just entMap = HM.toList . HM.map codepoints <$> decode entFile
    ParseOk template <- parseModule <$> readFile "generation/Template.hs"
    writeFile "src/Text/Html5/Entity/Data.hs" $ prettyPrint 
                                              $ appendTemplate template entMap

-- | AST generation
appendTemplate :: HsModule -> Entities -> HsModule
appendTemplate (HsModule srcLoc modName exports imports decls') ents =
    HsModule srcLoc modName exports imports (decls' ++ decls ents)


decls :: Entities -> [HsDecl]
decls ents =
    [ HsFunBind [HsMatch noloc (HsIdent "entityMap") []
                               (HsUnGuardedRhs (mkEntityMap ents)) []]
    , HsFunBind [HsMatch noloc (HsIdent "entitySet") []
                               (HsUnGuardedRhs (mkEntitySet ents)) []]
    ]

mkEntityMap :: Entities -> HsExp
mkEntityMap ents = HsApp (HsVar $ Qual (Module "M") $ HsIdent "fromList")
                         (HsList $ map mkMapElemTup ents)

mkMapElemTup :: (String, [Integer]) -> HsExp
mkMapElemTup (name, codes) =
    HsTuple [ HsLit $ HsString name
            , HsList $ map (HsLit . HsInt) codes
            ] 

mkEntitySet :: Entities -> HsExp
mkEntitySet ents = HsApp (HsVar $ Qual (Module "S") $ HsIdent "fromList")
                         (HsList $ map (HsLit . HsString . fst) ents)

noloc :: SrcLoc
noloc = SrcLoc "" 0 0
```

Попробуем запустить и получим вот такой результат:
```haskell
module Text.Html5.Entity.Data where
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
 
entityMap :: Map String [Int]
 
entitySet :: Set String
entityMap
  = M.fromList
      [("&intlarhk;", [10775]), ("&male;", [9794]), ("&not;", [172]).....]
entitySet
  = S.fromList
      ["&intlarhk;", "&male;", "&not;", "&clubs;", "&cudarrl;".....]
```

# Доступ к данным

Данные у нас есть, дело осталось за малым, написать удобные функции для доступа
к этим данным:

```haskell
module Text.Html5.Entity

import           Control.Applicative ((<$>))
import           Data.Char (chr)
import qualified Data.Map as M
import qualified Data.Set as S

import Text.Html5.Entity.Data

entityCodePoints :: String -> Maybe [Int]
entityCodePoints ent = M.lookup ent entityMap

entityChars :: String -> Maybe String
entityChars ent = map chr <$> M.lookup ent entityMap

isValidEntity :: String -> Bool
isValidEntity ent = S.member ent entitySet


nameToEntity :: String -> String
nameToEntity name = '&' : name ++ ";"

entityNameCodePoints :: String -> Maybe [Int]
entityNameCodePoints = entityCodePoints . nameToEntity

entityNameChars :: String -> Maybe String
entityNameChars = entityChars . nameToEntity

isValidEntityName :: String -> Bool
isValidEntityName name = S.member (nameToEntity name) entitySet
```

# Заключение
Поставленная проблема решена. Конечно для полной законченности нужно поработать
над оптимизацией. В целом идея генерации
таких модулей не нова, к примеру существует инструмент [gperf](https://www.gnu.org/software/gperf/)
который генерирует C(++) код для поиска по хеш-таблицам построенных на основе списков строк.
Тем не менее, я не смог найти примеров использования этой техники в Хаскелле,
хотя наверняка они есть. Было бы интересно написать библиотеку которая упрощала
бы генерацию вот таких модулей, с другой стороны куда уж проще, непосредственно
генерирующий код умещается в 20-30 строк.

Получившуюся библиотеку я разместил на hackage под именем [html5-entity](https://hackage.haskell.org/package/html5-entity).
