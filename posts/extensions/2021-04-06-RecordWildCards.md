---
author: Юрий Сыровецкий
title:  Слово в защиту RecordWildCards
tags:   расширения, стиль, практики, читаемость
description: Способы использования расширения `RecordWildCards` без порчи кода.
---

Многие практикующие хаскелиты <!-- sic! --> ругают расширение [`RecordWildCards`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/exts/record_wildcards.html) за ухудшение читаемости. Действительно, если использовать его где захочется:

```hs
{- здесь и далее использованы случайные слова, не ищите скрытый смысл -}

import Stove.Egg      -- (1)
import Dot.Dig.Trail  -- (2)

shoulder
  Pleasure{..}        -- (3)
  Type{..}            -- (4)
  = do
    Enemy{..} <-      -- (5)
      street *> dance
    allow $ having . lonely
  where
    Train{..} =       -- (6)
      well #. cream
```

то непонятно, откуда взялась та или иная переменная. (Потенциальные места отмечены номерами.)

Однако, `RecordWildCards` в некоторых случаях может улучшить читаемость кода!

## 1. Тривиальная упаковка структуры

Когда все поля заполняются в do-блоке в непосредственной близости.

```hs
parseOptions = do
  inputs <- many parseInput
  output <- optional parseOutput
  health <- optHealth
  roar   <- flagRoar
  pure Options{..}
```

Особенно полезно в комбинации с `ApplicativeDo`.

## 2. Тривиальная распаковка структуры

Когда в теле функции используются только поля одной структуры плюс не очень толстая обвязка общего назначения.

```hs
toJSON Mark{..} =
  object
    [ "id"    .= markId
    , "type"  .= type_
    , "stone" .= stone
    , "frame" .= base64encode frame
    ]
```

### 2a. Чтение поля

Частный случай — ещё более тривиальная распаковка, когда нет вообще ничего, кроме извлечения полей.

Иногда хочется прочитать одно поле, но от автоматических селекторов уже отказались в пользу [`DuplicateRecordFields`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/exts/duplicate_record_fields.html).

```hs
data Path = Path{hole :: Hole}

data Wall = Wall{hole :: Hole}

sortPathsOnHole = sortOn \Path{..} -> hole

sortWallsOnHole = sortOn \Wall{..} -> hole
```

Есть ещё вариант с линзами, но у линз своя цена.

## 3. Преобразование структур с частично совпадающими полями

Например, [миграция в базе данных](/posts/talks/2017/04/06/haskell-as-db.html).

```hs
data AuthorV1 = AuthorV1
  { shorter :: Shorter
  , largest :: Largest
  , center  :: Center
  , event   :: EventV1
  }

data AuthorV2 = AuthorV2
  { shorter :: Shorter
  , largest :: Largest
  , center  :: Center
  , event   :: EventV2
  , worried :: Worried
  }

upgradeAuthorV1ToV2 AuthorV1{..} =
  AuthorV2
    { event   = upgradeEventV1ToV2 event
    , worried = defaultWorried
    , ..
    }
```

### 3a. Обход ворнинга -Wincomplete-record-updates

Очень полезный ворнинг, который я всегда включаю, потому что он находит вот такие ошибки при добавлении новых слагаемых в тип-сумму:

```hs
data Image
  = ColorImage{width, height, colorDepth :: Word}
    -- ^ сначала было только одно слагаемое
  | MonoImage{width, height :: Word}
    -- ^ это добавили позже

setColorDepth1 d image = image{colorDepth = d}
--                       ^^^^^^^^^^^^^^^^^^^^^
--                       Pattern match(es) are non-exhaustive
```

Конечно, лучше сразу избавиться от именованых полей внутри сумм, но допустим, они вам по какой-то причине нужны.

Внезапно при включении этого ворнинга GHC начинает ругаться и на совершенно корректный код:

```hs
setColorDepth2 d = \case
  image@ColorImage{} -> image{colorDepth = d}
                     -- ^^^^^^^^^^^^^^^^^^^^^
                     -- Pattern match(es) are non-exhaustive
  image              -> image
```

И с точки зрения системы типов компилятор совершенно прав. Сопоставление с `ColorImage` не сужает тип переменной. Это не баг. Но как-то улучшить обнаружение проблем в этом месте, наверно, всё-таки можно.

Обойти можно так:

```hs
setColorDepth3 d = \case
  ColorImage{..} -> ColorImage{colorDepth = d, ..}
  image          -> image
```

Ну и линзами, конечно.

## Недостатки

Включение `RecordWildCards` приоткрывает дорогу и плохому коду, если не следить за новичками пристально на ревью. Эту задачу можно было бы переложить на линтеры, но я пока не встречал таких, что могли бы ограничить использование этого расширения только в описанных случаях. Найдёте — сообщите мне.

## Ссылки

1. Менее предвзятый [пост](https://kodimensional.dev/recordwildcards) Дмитрия Кованикова о возможностях `RecordWildCards`.
