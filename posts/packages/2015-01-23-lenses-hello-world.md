---
author:         Денис Шевченко
title:          Линзы: Hello World!
tags:           lens
description:    Введение в линзы. Только для новичков, опытным хаскелистам не читать!
---

Приветствую!

В общем, надоело мне, друзья! Сто раз я уже слышал про эти линзы, а что они такое, понятия не имел. Давайте же наконец разберёмся.

В основу данной заметки лёг мой вольный перевод [прекрасной статьи](http://taylor.fausak.me/2014/08/03/lenses-from-the-ground-up/), с некоторыми изменениями.

## Атлет

Есть у нас атлет, и есть у него имя. Определим следующий тип:

```haskell
-- Main.hs
module Main where

data Athlete = Athlete String
```

И захотелось нам изменять/получать имя атлета. Определяем две функции:


```haskell
getName :: Athlete -> String
getName (Athlete name) = name

setName :: String -> Athlete -> Athlete
setName newName (Athlete _) = Athlete newName
```

Теперь главный код:

```haskell
main :: IO ()
main = 
    let athleteWithoutName = Athlete ""
        realAthlete        = setName "Taylor Fausak" athleteWithoutName
        nameOfRealAthlete  = getName realAthlete
    in putStrLn nameOfRealAthlete
```

Перед нами - классические геттер (`getName`) и сеттер (`setName`). Всё предельно просто. Однако в реальном проекте так делать не рекомендуется. Рано или поздно у атлета вслед за именем появится дата рождения, рост, знак Зодиака, любимое блюдо, девичья фамилия мамы и т.п. И что же, на каждое такое поле определять свою пару геттер/сеттер?? Нет, нас категорически не устраивает такое решение.

## Упрощаем

Автоматизация - наше всё. Пишем:

```haskell
-- Main.hs
module Main where

data Athlete = Athlete { name :: String }

main :: IO ()
main = 
    let athleteWithoutName = Athlete ""
        realAthlete        = athleteWithoutName { name = "Taylor Fausak" }
        nameOfRealAthlete  = name realAthlete
    in putStrLn nameOfRealAthlete
```

Вооот, так уже значительно лучше. Компилятор сделал скучную работу за нас. Теперь у нас есть поле `name`, которое *уже* является и сеттером, и геттером. Ура, победа! Но, как вы видите, заметка на этом не заканчивается, ибо имеется ещё одна проблема.

## Одноимённые поля

Атлеты атлетами, но вскоре у нас появился клуб (возможно, атлетов):

```haskell
-- Main.hs
module Main where

data Athlete = Athlete { name :: String }

data Club = Club { name :: String }

...
```

Увы и ах, компилятор грубо оборвёт наше счастье следующей ошибкой:

```bash
Multiple declarations of ‘name’
```

К сожалению, нельзя использовать одно и то же поле `name` в подобной манере, ибо компилятор путается, что где. Ну что ж, поможем компилятору и разнесём типы `Athlete` и `Club` по разным модулям:

```haskell
-- Club.hs
module Club where

data Club = Club { name :: String }

-- Athlete.hs
module Athlete where

data Athlete = Athlete { name :: String }
```

Теперь пишем:

```haskell
-- Main.hs
module Main where

import Athlete
import Club

main :: IO ()
main = 
    let athleteWithoutName = Athlete ""
        realAthlete        = athleteWithoutName { name = "Taylor Fausak" }
        nameOfRealAthlete  = name realAthlete
    in putStrLn nameOfRealAthlete
```

Но капризный компилятор и этому варианту не будет рад, ибо хотя прежняя ошибка ушла, пришла новая:

```bash
src/Main.hs:9:51:
    Ambiguous occurrence ‘name’
    It could refer to either ‘Athlete.name’,
                             imported from ‘Athlete’ at src/Main.hs:3:1-14
                             (and originally defined at src/Athlete.hs:3:26-29)
                          or ‘Club.name’,
                             imported from ‘Club’ at src/Main.hs:4:1-11
                             (and originally defined at src/Club.hs:3:20-23)
```

Когда мы с вами глядим на эту строку:

```haskell
athleteWithoutName { name = "Taylor Fausak" }
```

нам предельно ясно, какое `name` имеется в виду, но компилятор не столь понятлив. Ладно, удовлетворим его каприз и уточним:

```haskell
-- Main.hs
module Main where

import Athlete as A
import Club    as C

main :: IO ()
main = 
    let athleteWithoutName = Athlete ""
        realAthlete        = athleteWithoutName { A.name = "Taylor Fausak" }
        nameOfRealAthlete  = A.name realAthlete
        clubWithoutName    = Club ""
        realClub           = clubWithoutName { C.name = "Fixed Touring" }
        nameOfRealClub     = C.name realClub
    in putStrLn $ nameOfRealAthlete ++ ", " ++ nameOfRealClub
```

Теперь всё работает. Но такое решение тоже не торт. Рано или поздно количество модулей увеличится, и между alias-именами модулей могут возникнуть коллизии. Конечно, мы можем не использовать alias-имена и указывать имя модуля целиком:

```haskell
Athlete.name realAthlete
```

но каждый раз уточнять "модульную принадлежность" наших полей - это скучновато.

## Отказ от одноимённых полей

В конце концов, раз от них проблемы, так давайте же избавимся от них:

```haskell
-- Club.hs
module Club where

data Club = Club { clubName :: String }

-- Athlete.hs
module Athlete where

data Athlete = Athlete { athleteName :: String }
```

И далее пишем так:

```haskell
-- Main.hs
module Main where

import Athlete
import Club

main :: IO ()
main = 
    let athleteWithoutName = Athlete ""
        realAthlete        = athleteWithoutName { athleteName = "Taylor Fausak" }
        nameOfRealAthlete  = athleteName realAthlete
        clubWithoutName    = Club ""
        realClub           = clubWithoutName { clubName = "Fixed Touring" }
        nameOfRealClub     = clubName realClub
    in putStrLn $ nameOfRealAthlete ++ ", " ++ nameOfRealClub
```

Никаких коллизий, всё прекрасно работает, но это некрасивое решение. Ведь мы постоянно сами себя повторяем:

```haskell
athleteWithoutName { athleteName = "Taylor Fausak" }
```

Читается как "масло масляное". Нет, мы любим Haskell за его красоту, поэтому желаем чего-то лучшего.

## Именной класс типов

Определим класс для работы с типами, имеющими имя:

```haskell
class HasName a where
    getName :: a -> String
    setName :: String -> a -> a
```

Теперь создадим экземпляры этого класса для наших атлетов и клубов:

```haskell
instance HasName Athlete where
    getName athlete = athleteName athlete
    setName newName athlete = athlete { athleteName = newName }

instance HasName Club where
    getName club = clubName club
    setName newName club = club { clubName = newName }
```

И теперь пишем:

```haskell
main :: IO ()
main = 
    let athleteWithoutName = Athlete ""
        realAthlete        = setName "Taylor Fausak" athleteWithoutName
        nameOfRealAthlete  = getName realAthlete
        clubWithoutName    = Club ""
        realClub           = setName "Fixed Touring" clubWithoutName
        nameOfRealClub     = getName realClub
    in putStrLn $ nameOfRealAthlete ++ ", " ++ nameOfRealClub
```

Ну что ж, такое решение значительно красивее, ведь никакого "масла масляного" уже нет. Казалось бы, мы нашли идеальное решение, но... 

## Разные типы

Что произойдёт, если тип поля `name` у атлета станет другим, нежели у клуба? Допустим, мы решили использовать более продвинутый тип `Text`:

```haskell
-- Athlete.hs
module Athlete where

import Data.Text

data Athlete = Athlete { athleteName :: Text }
```

К сожалению, всё тут же сломается:

```bash
src/Main.hs:11:25:
    Couldn't match type ‘Data.Text.Internal.Text’ with ‘[Char]’
    Expected type: String
      Actual type: Data.Text.Internal.Text
    In the expression: athleteName athlete
    In an equation for ‘getName’:
        getName athlete = athleteName athlete
```

Вспомним класс типов `HasName`:

```haskell
class HasName a where
    getName :: a -> String
    setName :: String -> a -> a
```

Методы этого класса дружат с обыкновенной строкой, а им тут подсовывают какой-то `Text`... Но врагу не сдаётся наш гордый Варяг! Сделаем класс более обобщённым:

```haskell
class HasName a b where
    getName :: a -> b
    setName :: b -> a -> a
```

Кстати, чтобы это работало, добавим необходимые Haskell-расширения в начале модуля `Main.hs`:

```haskell
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
```

Теперь изменим экземпляры класса для наших типов:

```haskell
instance HasName Athlete Text where
    getName athlete = athleteName athlete
    setName newName athlete = athlete { athleteName = newName }

instance HasName Club String where
    getName club = clubName club
    setName newName club = club { clubName = newName }
```

Здесь мы учли разность типов имён атлетов и клубов. И теперь пишем:

```haskell
main :: IO ()
main = 
    let athleteWithoutName = Athlete empty
        realAthlete        = setName (pack "Taylor Fausak") athleteWithoutName
        nameOfRealAthlete  = getName realAthlete
        clubWithoutName    = Club ""
        realClub           = setName "Fixed Touring" clubWithoutName
        nameOfRealClub     = getName realClub
    in putStrLn $ unpack nameOfRealAthlete ++ ", " ++ nameOfRealClub
```

Работает:

```bash
Taylor Fausak, Fixed Touring
```

Это весьма хорошее решение, и мы могли бы на этом остановиться, но существует иной путь.

## Линзы

Определим новый тип:

```haskell
data Lens a b = Lens { get :: a -> b
                     , set :: b -> a -> a
                     }
```

Перед нами - главная идея линз. Мы ведь хотим сделать работу с геттерами и сеттерами максимально изящной, не правда ли? Так вот идея в том, чтобы создать линзу (для русскоязычного читателя привычнее будет термин "лупа"), позволяющую нам сфокусироваться на каком-то значении. А фокусироваться нам нужно как раз для того, чтобы изменять (`set`) или получать (`get`).

Но чтобы всё это работало, мы должны определить линзы для каждого из наших типов:

```haskell
athleteNameLens :: Lens Athlete Text
athleteNameLens = Lens { get = \athlete -> athleteName athlete
                       , set = \newName athlete -> athlete { athleteName = newName }
                       }

clubNameLens :: Lens Club String
clubNameLens = Lens { get = \club -> clubName club
                    , set = \newName club -> club { clubName = newName }
                    }
```

Мы определили линзу `athleteNameLens` для фокусировки на имени атлета, а также линзу `clubNameLens` для фокусировки на имени клуба. Фактически, `get` и `set` - это лямбда-обёртки для работы с уже известным нам полем `athleteName`.

Возможно, вы спросите, зачем нужны такие сложности? Но взгляните на новое определение экземпляров класса `HasName`:

```haskell
class HasName a b where
    name :: Lens a b

instance HasName Athlete Text where
    name = athleteNameLens

instance HasName Club String where
    name = clubNameLens
```

Всё сильно упростилось. Класс `HasName` теперь содержит одно-единственное "линзовое" значение `name`. Соответственно, в каждом экземпляре этого класса мы просто заявляем: "Значение `name` теперь связано с конкретной линзой, определённой для данного типа."

И вот теперь наш главный код приобретает следующий вид:

```haskell
main :: IO ()
main = 
    let athleteWithoutName = Athlete empty
        realAthlete        = set name (pack "Taylor Fausak") athleteWithoutName
        nameOfRealAthlete  = get name realAthlete
        clubWithoutName    = Club ""
        realClub           = set name "Fixed Touring" clubWithoutName
        nameOfRealClub     = get name realClub
    in putStrLn $ unpack nameOfRealAthlete ++ ", " ++ nameOfRealClub
```

Элегантно, не правда ли? Читается теперь как поэма:

```haskell
set name "Fixed Touring" clubWithoutName
```

Здесь `set` фокусируется на имени клуба через линзу `name`. Мы как будто говорим: "Измени то, на что смотрит эта линза", или "Получи то, на что смотрит эта линза". Грубо и схематично это можно представить себе так:

```
действие -> линза -> нечто, к чему применяется действие
```

## Правда

А правда, друзья мои, состоит в том, что всё вышеизложенное есть не более чем мааааааленькая демонстрация. Существует мощный пакет [lens](http://hackage.haskell.org/package/lens), в котором всё уже реализовано очень умными людьми. К рассмотрению возможностей этого пакета мы приступим в следующих статьях.

