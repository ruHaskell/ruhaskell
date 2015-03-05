---
author:         Денис Шевченко
title:          Aeson: продолжаем
tags:           JSON
description:    Продолжим наше изучение пакета aeson.
---

Приветствую, друзья!

Итак, продолжим рассмотрение пакета `aeson`, предназначенного, как вы [помните](http://ruhaskell.org/posts/packages/2015/02/03/aeson-hello-world.html), для работы с JSON.

## Работа с Haskell-типами

В предыдущей заметке мы работали с JSON посредством собственного типа `Host`. Это весьма удобно, но в ряде случаев вводить собственный тип не обязательно.

Пусть в нашем JSON будет обычный массив:

```json
[
    "john@gmail.com",
    "ann@gmail.com",
    "michael@gmail.com"
]
```

В этом случае мы легко обойдёмся списком строк:

```haskell
module Main where

import qualified Data.ByteString as B
import           Data.Aeson

main :: IO ()
main = do
    rawJSON <- B.readFile "/Users/dshevchenko/our.json"
    let result = decodeStrict rawJSON :: Maybe [String]  -- Явно указываем тип...
    putStrLn $ case result of
        Nothing         -> "Invalid JSON!"
        Just emailsList -> show emailsList
```

Вполне ожидаемый результат:

```bash
["john@gmail.com","ann@gmail.com","michael@gmail.com"]
```

Или пусть у нас есть три простые координаты:

```json
{
    "X": 23.347,
    "Y": 455.609,
    "Z": -45.0055
}
```

Читаем их в обыкновенный словарь:

```haskell
module Main where

import qualified Data.ByteString as B
import           Data.Aeson
import qualified Data.Map        as M
import           Data.Maybe (fromJust)

main :: IO ()
main = do
    rawJSON <- B.readFile "/Users/dshevchenko/our.json"
    let result = decodeStrict rawJSON :: Maybe (M.Map String Double)
    putStrLn $ case result of
        Nothing     -> "Invalid JSON!"
        Just coords -> show . fromJust $ M.lookup "Y" coords
```

Вывод:

```bash
455.609
```

Таким образом, мы убедились, что для простых случаев вводить собственные типы вообще не нужно. Ну а что касается обратного превращения в JSON, тут уж совсем всё просто:

```haskell
module Main where

import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy.Char8 as LC
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.Map                   as M
import           Data.Maybe (fromJust)

main :: IO ()
main =
    let coords = M.fromList [ ("X", 12.344)
                            , ("Y", 344.5508)
                            , ("Z", -5.00789)
                            ] :: M.Map String Double
    in LC.putStrLn $ encodePretty coords
```

Только не забываем явно указывать тип значения `coords`, а то компилятору трудно будет разобраться. В итоге:

```bash
{
    "Z": -5.00789,
    "X": 12.344,
    "Y": 344.5508
}
```

## Вытаскиваем значение отдельного поля

Представим, что есть вот такой JSON с краткой информацией обо мне:

```json
{
    "firstName": "Denis",
    "lastName": "Shevchenko",
    "social": {
        "GitHub": {
            "blog": "https://github.com/denisshevchenko/blog",
            "ohaskell": "https://github.com/denisshevchenko/ohaskell"
        },
        "Google": {
            "gmail": "shev.denis@gmail.com",
            "plus": "https://www.google.com/+DenisShevchenko"
        }
    }
}
```

И вот понадобилось мне извлечь значение конкретного поля. Одного-единственного поля, мне не нужно всё остальное. Скажем, URL репозитория моего блога. Таким образом, нам необходимо заглянуть вглубь: `/ -> social -> GitHub -> blog`. Вот как это выглядит:

```haskell
main :: IO ()
main = do
    rawJSON <- B.readFile "/Users/dshevchenko/our.json"
    let result = decodeStrict rawJSON :: Maybe Object
    putStrLn $ case result of
        Nothing   -> "Invalid JSON!"
        Just info -> getBlogRepoURL info
```

Обратите внимание, что тип значения `result` соответствует `Maybe Object`. Затем, после извлечения конкретного значения из `Maybe`, мы передаём его функции `getBlogRepoURL`. Вот её определение:

```haskell
getBlogRepoURL :: Object -> String
getBlogRepoURL info =
    case parseMaybe extractBlogRepoURL info of
        Nothing  -> ""
        Just url -> url
    where
        extractBlogRepoURL = \info       -> info .: "social"
                             >>=
                             \socialInfo -> socialInfo .: "GitHub"
                             >>=
                             \gitHubInfo -> gitHubInfo .: "blog"
```

Самое интересное происходит в функции `extractBlogRepoURL`, ведь именно здесь мы погружаемся в наш JSON. Для этого используется три лямбда-функции, в каждой из которых мы видим уже знакомый нам оператор извлечения значения `.:`. А для того, чтобы сделать их более понятными, я привожу их в полной форме (в сокращённой будет чуть ниже). Но как же происходит погружение в JSON-структуру? Чтобы всё встало на свои места, привожу JSON в "разобранном" виде:

```json
корень ->    Object (fromList [
                 ("lastName",String "Shevchenko"),
                 ("firstName",String "Denis"),
                 ("social",
1-й подобъект ->     Object (fromList [
                         ("GitHub",
2-й подобъект ->             Object (fromList [
                                 ("blog",String "https://github.com/denisshevchenko/blog"),
                                 ("ohaskell",String "https://github.com/denisshevchenko/ohaskell")
                             ])
                         ),
                         ("Google",
                             Object (fromList [
                                 ("plus",String "https://www.google.com/+DenisShevchenko")
                             ])
                         )
                     ])
                 )
             ])
```

Вот теперь назначение лямбда-функций предельно понятно. Первая функция:

```haskell
\info -> info .: "social"
```

погружает нас в 1-й подобъект, вторая функция:

```haskell
\socialInfo -> socialInfo .: "GitHub"
```

ведёт нас в 2-й подобъект, ну а третья:

```haskell
 \gitHubInfo -> gitHubInfo .: "blog"
```

уже извлекает значение URL репозитория блога.

Кстати, как и было обещано, привожу упрощённую форму функции `extractBlogRepoURL`:

```haskell
extractBlogRepoURL = \info -> info .: "social" >>= (.: "GitHub") >>= (.: "blog")
```

Мы убрали лямбда-уточнения и сильно сократили код. Кому-то подобное сокращение не понравится, но пусть каждый сам выбирает удобную для него форму.

Ну вот, на сегодня хватит.

