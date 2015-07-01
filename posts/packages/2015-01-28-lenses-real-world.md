---
author:         Денис Шевченко
title:          Линзы: Real World
tags:           lens
description:    Продолжаем изучать линзы, а именно пакет fclabels.
---

Приветствую, друзья!

Как и было обещано в [предыдущей заметке](/posts/packages/2015/01/23/lenses-hello-world.html), мы продолжаем беседу о линзах. Впрочем, я чуть-чуть нарушил своё обещание: вместо пакета `lens` мы рассмотрим пакет `fclabels`. Дело в том, что я поверил на слово [вот этой рекомендации](http://www.stephendiehl.com/what/#lenses). Пакет `fclabels` значительно легковеснее, нежели `lens`, к тому же, что приятно обрадовало лично меня, у него куда как более понятная документация. Разумеется, между этими двумя пакетами есть общие черты. За сим и начнём.

## Люди и места

Допустим, есть у нас место:

```haskell
data Place = Place { _city
                   , _country
                   , _continent :: String
                   } deriving Show
```

и есть у нас человек, живущий в этом месте:

```haskell
data Person = Person { _name  :: String
                     , _age   :: Int
                     , _place :: Place
                     } deriving Show
```

Всё предельно просто, перед нами два многопольных типа. Обратите внимание, что имена полей начинаются с символа `_`. Это не просто так. Ведь мы собираемся использовать пакет `fclabels` (который вы уже установили, не так ли?), а он требует, чтобы имена полей (к которым мы будем получать доступ через линзы) начинались с подчёркивания. Чуть позже станет понятно, почему.

Теперь весь наш модуль:

```haskell
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Category ((.), id)
import Data.Label
import Prelude hiding ((.), id)

data Place = Place { _city
                   , _country
                   , _continent :: String
                   } deriving Show

data Person = Person { _name   :: String
                     , _age    :: Int
                     , _place  :: Place
                     } deriving Show

mkLabels [''Person, ''Place]

main :: IO ()
main =
    let jan      = Person "Jan" 71 (Place "Utrecht" "The Netherlands" "Europe")
        janName  = get name jan
        janAge   = get age jan
        janCity  = get (city . place) jan
    in putStrLn $ janName ++ ", " ++ (show janAge) ++ ", " ++ janCity
```

Ожидаемый вывод:

```bash
Jan, 71, Utrecht
```

Теперь давайте разбираться. Самая необычная строка, которую мы здесь видим - вот эта:

```haskell
mkLabels [''Person, ''Place]
```

Эта строка есть представитель так называемого Template Haskell. Для тех кто не знает - Template Haskell (сокращённо TH) это специальное расширение языка Haskell, предназначенное для метапрограммирования. Суть его очень проста: специальная конструкция, не являющаяся Haskell-кодом, превращается в некий Haskell-код на этапе компиляции. Часто это избавляет от рутины.

Таким образом, конструкция со словом `mkLabels` вежливо сгенерирует код наших линз за нас. И вот для того, чтобы это прошло гладко, мы и указали символ `_` в именах полей наших типов (таково требование TH). Однако при *использовании* созданных линз мы уже не видим никаких подчёркиваний, и это очень удобно:

```haskell
janName  = get name jan
```

Вот наш старый знакомый, линзовый геттер. Красиво и понятно.

Как вы уже догадались, `mkLabels` создаёт линзы для нескольких типов одновременно. Если же мы работаем с единственным типом, то можно написать проще:

```haskell
mkLabel ''Person
```

## Комбинирование

Обратите внимание вот на эту строчку:

```haskell
janCity  = get (city . place) jan
```

Геттер используется для доступа к имени города, которое, в свою очередь, спрятано в поле `place`. Комбинирование полей при линзовом доступе - мощный и удобный механизм. Вспомним комбинационное прочтение: вместо точки добавляем фразу "будет вызвана после":

```haskell
get (city "будет вызвана после" place) jan
```

Таким образом, `place` даёт нам доступ к полю типа `Place`, а `city` в свою очередь даёт доступ к имени города.

Давайте чуток усложним:

```haskell
data Address = Address { _street :: String
                       , _house  :: Int
                       } deriving Show

data Place = Place { _address   :: Address
                   , _city
                   , _country
                   , _continent :: String
                   } deriving Show
```

Теперь место жительства содержит поле типа `Address`. Далее пишем:

```haskell
mkLabels [''Person, ''Place, ''Address]

main :: IO ()
main =
    let anAddress = Address "Bilthoven" 8
        jan       = Person "Jan" 71 (Place anAddress "Utrecht" "The Netherlands" "Europe")
        janName   = get name jan
        janStreet = get (street . address . place) jan
    in putStrLn $ janName ++ ", " ++ janStreet
```

Вывод:

```bash
Jan, Bilthoven
```

Обратите внимание: мы сгенерировали линзы и для типа `Address`. А теперь комбинируем поля для доступа к названию улицы:

```haskell
janStreet = get (street . address . place) jan
```

Красиво, не правда ли? Таким образом, линзовый доступ позволяет нам заглянуть сколь угодно глубоко.

## Изменяем

Геттеры геттерами, но пришла пора что-нибудь поменять. Изменим возраст:

```haskell
main :: IO ()
main =
    let anAddress  = Address "Bilthoven" 8
        jan        = Person "Jan" 71 (Place anAddress "Utrecht" "The Netherlands" "Europe")
        youngerJan = set age 65 jan
    in putStrLn . show $ get age youngerJan
```

Итак, Jan помолодел на 6 лет, сеттер `set` работает в точности как мы и ожидаем.

Разумеется, мы и здесь можем использовать комбинирование полей, дабы заглянуть вглубь. Представим, что наш Jan решил переехать в другой дом на своей улице:

```haskell
main :: IO ()
main =
    let anAddress   = Address "Bilthoven" 8
        jan         = Person "Jan" 71 (Place anAddress "Utrecht" "The Netherlands" "Europe")
        movedJan    = set (house . address . place) 9 jan
        newJanHouse = get (house . address . place) movedJan
    in putStrLn $ show newJanHouse
```

Теперь Jan живёт в доме номер 9. Элегантно и просто.

## Модифицируем

Пакет `fclabels` предоставляет нам ещё один способ линзового изменения, а именно через функцию `modify`. Если уже известная нам функция `set` изменяет поле прямым значением, то `modify` делает это через функцию.

Помните, как мы изменили возраст? Сделаем же это иначе: теперь наш Jan повзрослеет на один год. Пишем:

```haskell
main :: IO ()
main =
    let anAddress   = Address "Bilthoven" 8
        jan         = Person "Jan" 71 (Place anAddress "Utrecht" "The Netherlands" "Europe")
        olderJan    = modify age (+1) jan
    in putStrLn . show $ get age olderJan
```

И как вы уже догадались, мы и тут можем заглянуть вглубь. Пусть наш Jan опять переедет, но уже в другой конец улицы:

```haskell
main :: IO ()
main =
    let anAddress   = Address "Bilthoven" 8
        jan         = Person "Jan" 71 (Place anAddress "Utrecht" "The Netherlands" "Europe")
        movedJan    = modify (house . address . place) (+10) jan
        newJanHouse = get (house . address . place) movedJan
    in putStrLn $ show newJanHouse
```

Теперь Jan живёт в 18 доме.

## Персона в облаке

В модуле `Data.Label.Monadic` определены вкусности для работы с трансформерами `Reader` и `State`. Далее я предполагаю, что вы знакомы с этими трансформерами. Используется пакет `mtl`.

Поместим нашего Jan в облако, дабы с ним было удобно работать из нескольких функций. Вот наше облако:

```haskell
...
import qualified Control.Monad.Reader as R
import Data.Label.Monadic

...

type PersonInCloud = R.ReaderT Person IO ()
```

Задача такова: вывести на экран информацию о персоне, причём есть несколько функций, каждая из которых вытаскивает лишь часть информации об этой персоне. Вот как это будет выглядеть:

```haskell
main :: IO ()
main = do
    let anAddress = Address "Bilthoven" 8
        jan       = Person ("Jan", "Bakker") 71 (Place anAddress "Utrecht" "The Netherlands" "Europe")
    R.runReaderT (printName
                  >> printAge
                  >> printStreet) jan
    putStrLn "\nIt's done."
```

Мы можем работать с облачным значением двумя способами. Рассмотрим на примере функции `printAge`. Вот первый, канонический способ:

```haskell
printAge :: PersonInCloud
printAge = do
    jan <- R.ask
    let janAge = get age jan
    R.liftIO . putStrLn $ "Age: " ++ show janAge
```

Тут всё как обычно: сначала, используя `Reader`-функцию `ask`, мы вытаскиваем нашу персону, а затем, через уже известную нам линзу, получаем возраст. Но есть путь короче:

```haskell
printAge :: PersonInCloud
printAge = do
    anAge <- asks age
    R.liftIO . putStrLn $ "Age: " ++ show anAge
```

Круто, правда? Мы использовали функцию `asks` из модуля `Data.Label.Monadic`. Она упрощает нам жизнь, сразу же вытаскивая из облачной персоны её возраст.

Таким же образом можно работать и с трансформером `State`: в модуле `Data.Label.Monadic` вы найдёте функции `gets`, `puts` и `modify`.

## Выводы

Ну что ж, на мой взгляд, пакет `fclabels` более чем заслуживает нашего внимания. Просто, удобно и элегантно. Кстати, у этого пакета значиииительно меньше зависимостей, нежели у `lens`.

В будущих заметках мы продолжим рассмотрение линз (из этого пакета или из других), в контексте различных интересных задач.

