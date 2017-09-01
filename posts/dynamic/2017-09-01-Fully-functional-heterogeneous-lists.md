---
author: Илья Зуев
title: Полнофункциональные гетерогенные списки
tags: типы, классы типов, гетерогенные списки
description: Использование гетерогенных списков. Фильтрация по классам типов. Вызов соответствующих функций. Реализация концепций instanceOf и asInstanceOf.
---

## Цели и задачи

* Фильтрация элементов (объектов) гетерогенных списков по классам типов.
* Создание «настоящих» гетерогенных списков, обладающих полной функциональностью.

## Описание проблемы

Мы реализуем проект на Хаскелле. И в рамках проекта появилась необходимость в использовании ООП парадигмы.
Обсуждение, хорошо это или плохо, выходит за рамки данной статьи.
Самое главное, что для нашего проекта, для решения конкретно наших задач, подошёл именно ООП подход.

Однако, камнем преткновения для нас стала невозможность хранения объектов в универсальных, так называемых гетерогенных списках.
Хранить-то в принципе можно, но полноценно использовать — нет, даже при использовании экзистенциальных типов.
Проблема, достаточно широко известная в Хаскелле.
На одной из страниц Stack Overflow я нашёл такой комментарий:
«You can use existensials, but then you can't do anything with the data after pattern matching on it», — что достаточно категорично характеризует ситуацию.
Я достаточно долго бился над проблемой и, к сожалению, не нашёл решения данной задачи в интернете.
Я изучил большое количество статей и провёл много экспериментов. См. внизу список использованных мною в процессе подготовки материалов.
Время и усилия были не напрасны в плане погружения в Хаскелл, но больше всего радует то, что поставленная задача решена.
Пусть пока грубо, пусть только для нашего конкретного случая.

## Решение задачи

Хаскелл по своей сути является функциональным языком программирования.
Однако, пусть он будет мультипарадигменным хотя бы в нашем проекте.

Итак, примем за основу следующую схему.
В моём восприятии, это традиционный путь в Хаскелле для имитации ООП.
Для определения и хранения полей объектов будем использовать `data` и/или `newtype`.
Для придания объекту функциональности будем использовать классы типов (`class`), как, в некотором роде, аналоги интерфейсам в традиционных ОО языках.
Типы, классы и экземпляры (`instance`) вместе пусть соответствуют классам в ООП.

Вся суть идеи будет заключена лишь в паре функций и массивов.
Тем не менее, для того, чтобы показать, что всё работает (и как работает), напишем небольшой проект.
Для пущей убедительности в том, что всё работает, сделаем объекты проекта «множественно наследуемыми».

Постараюсь сделать проект максимально простым. Основная цель проекта — донести суть концепции.

Итак, приступим.
Для начала приведу описание структуры дерева объектов:

* `RenderableBase` — абстрактный тип, отвечающий за расположение и рисование;
* `SerializableBase` — абстрактный тип, отвечающий за сериализацию.

Объекты следующих типов будут включены в гетерогенный список:

* `Circle`, наследуется от `RenderableBase` и `SerializableBase`;
* `Rectangle`, наследуется от `RenderableBase`;
* `Triangle`, наследуется от `RenderableBase`.

Представим схему наследования в виде небольшого списка:

* `SerializableBase > Circle`
* `RenderableBase > Circle`
* `RenderableBase > Rectangle`
* `RenderableBase > Triangle`

Иерархия наследования не играет никакой роли в реализации концепции и приведена лишь для того, чтобы придать проекту ощущение объектно-ориентированного подхода.

Теперь дам описание классов типов:

* `ClsShape` предназначен для объединения объектов в гетерогенный список;
* `ClsRenderable` описывает функцию рендеринга;
* `ClsClickable` описывает функцию-реакцию на клик мышки;
* `ClsSerializable` описывает функцию сериализации объекта.

Теперь нечто более важное:

* объекты типов `Circle`, `Rectangle` и `Triangle` могут быть отрисованы, то есть реализуют функцию `render`;
* на объекты типов `Rectangle` и `Triangle` можно кликнуть мышкой (Внимание! На объекты типа `Circle` кликнуть мышкой нельзя, то есть тип `Circle` не реализует класс `ClsClickable`);
* объект типа `Circle` можно сериализовать (класс `ClsSerializable`).

Представим схему функциональности в виде небольшого списка, который показывает, какой класс каким типом реализуется:

* `ClsRenderable > Circle`
* `ClsRenderable > Rectangle`
* `ClsRenderable > Triangle`
* `ClsClickable > Rectangle`
* `ClsClickable > Triangle`
* `ClsSerializable > Circle`

Теперь начнём писать код.
Для начала включим все необходимые нам расширения GHC и импортируем библиотеки:

```haskell
{-# LANGUAGE GADTs, ConstraintKinds, KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleInstances #-}
{-# LANGUAGE DuplicateRecordFields, RecordWildCards #-}
module Main where

import Data.Typeable (Typeable, typeOf)
import GHC.Exts (Constraint)
import Unsafe.Coerce (unsafeCoerce)
```

Да, нам понадобится функция небезопасного приведения типов unsafeCoerce, но об этом чуть позже.
Весь код сосредоточен в одном файле, однако, давайте представим, что всё разбито на модули. Этим мне хотелось бы показать, что проект масштабируем.
Сначала опишем все классы, базовые абстрактные типы и типы данных, для которых мы будем создавать объекты.
А также инстанцируем каждый тип.
Пока всё тривиально. Мы используем «традиционный» подход для имитации ООП в Хаскелле:

```haskell
-- module Base where --
class ClsShape shape

-- module RenderableBase where --
class ClsRenderable a where render::a->String
data RenderableBase = RenderableBase {coords::String} deriving Show
instance ClsRenderable RenderableBase where render a = coords a

-- module ClickableBase where --
class ClsClickable a where click::a->String

-- module SerializableBase where --
class ClsSerializable a where serialize::a->String
data SerializableBase = SerializableBase {serializedData::String} deriving Show
instance ClsSerializable SerializableBase where serialize a = serializedData a

-- module Circle where --
data Circle = Circle {
    name :: String,
    renderableBase :: RenderableBase,
    serializableBase :: SerializableBase
    } deriving Show
instance ClsShape Circle
instance ClsRenderable Circle where
    render Circle{..} = "Circle " ++ name ++ " " ++ render renderableBase
instance ClsSerializable Circle where
    serialize Circle{..} = "Circle " ++ name ++ " " ++ serialize serializableBase

-- module Rectangle where --
data Rectangle = Rectangle {
    name :: String,
    renderableBase :: RenderableBase
    } deriving Show
instance ClsShape Rectangle
instance ClsRenderable Rectangle where
    render Rectangle{..} = "Rectangle " ++ name ++ " " ++ render renderableBase
instance ClsClickable Rectangle where
    click Rectangle{..} = "Rectangle " ++ name ++ " " ++ coords renderableBase ++ " clicked"

-- module Triangle where --
data Triangle = Triangle {
    name :: String,
    renderableBase :: RenderableBase
    } deriving Show
instance ClsShape Triangle
instance ClsRenderable Triangle where
    render Triangle{..} = "Triangle " ++ name ++ " " ++ render renderableBase
instance ClsClickable Triangle where
    click Triangle{..} = "Triangle " ++ name ++ " " ++ coords renderableBase ++ " clicked"
```

Теперь немного интереснее. Создадим тип-обёртку для реализации гетерогенного списка:

```haskell
-- module InferInstanceOf where --
data Wrap (constraint :: * -> Constraint) where
    Wrp :: (Show a, Typeable a, constraint a) => a -> Wrap constraint
instance Show (Wrap a) where show (Wrp a) = show a
```

Инстанцирование класса `Show` не играет никакой роли и служит лишь отладочным целям.
А вот фраза `(constraint :: * -> Constraint)` гораздо интереснее.
Тут мы как бы говорим, что в качестве подтипа для типа `Wrap` мы будем использовать некий класс.
То есть классы в данном случае будут играть роль типов.

Теперь создадим сам гетерогенный список:

```haskell
-- module Main where --
testData :: [Wrap ClsShape]
testData = [
    Wrp$ Circle    "crcl_1"  (RenderableBase "(1, 1)") (SerializableBase "Crcl1"),
    Wrp$ Circle    "crcl_2"  (RenderableBase "(2, 2)") (SerializableBase "Crcl2"),
    Wrp$ Rectangle "rect_1"  (RenderableBase "(3, 3)"),
    Wrp$ Rectangle "rect_2"  (RenderableBase "(4, 4)"),
    Wrp$ Triangle  "trngl_1" (RenderableBase "(5, 5)"),
    Wrp$ Triangle  "trngl_2" (RenderableBase "(6, 6)")
    ]
```

Первое: список можно пополнять динамически в процессе выполнения программы.
Второе: мы тут же теряем всю информацию о функциональности каждого объекта, то есть все словари для классов `ClsRenderable`, `ClsClickable` и `ClsSerializable`.
Остаются лишь ничего не значащиe для нас `ClsShape`, `Typeable` (о нём чуть позже) и `Show` для отладочных целей.

Максимум, что мы можем сейчас сделать, это просто напечатать список объектов:

```haskell
main = do
    putStr "all objects: "
    print$ map (\(Wrp a)->show a) testData

>>all objects: ["Circle {name = \"crcl_1\", renderableBase = RenderableBase {coords = \"(1, 1)\"} ...
```

Теперь переходим к самой интересной части, в которой будет показана вся суть идеи работы с гетерогенными списками.
Определяем списки распределённых по классам типов:

```haskell
renderableTypes:: [Wrap ClsRenderable]
renderableTypes = [
    Wrp (undefined::Circle),
    Wrp (undefined::Rectangle),
    Wrp (undefined::Triangle)
    ]

clickableTypes:: [Wrap ClsClickable]
clickableTypes = [
    Wrp (undefined::Rectangle),
    Wrp (undefined::Triangle)
    ]

serializableTypes:: [Wrap ClsSerializable]
serializableTypes = [
    Wrp (undefined::Circle)
    ]
```

Пока вручную. Позже эту работу возьмёт на себя _TemplateHaskell_.
Мало того, позже попытаемся упаковать эти списки в гетерогенный список типа `HList`, чтобы не использовать неуместные здесь функции.
Итак, каждый элемент этих списков опредлён как `undefined`, то есть данные нас не интересуют.
Нам нужно сохранить информацию о типе и, что более важно, нам важно не потерять информацию о функциональности, то есть нам нужно как-то сохранить словари.

Идём дальше. Первый этап нашей концепции научиться фильтровать наш гетерогенный список по классу.
Для этого напишем следующие функции:

```haskell
-- module InferInstanceOf where --
instanceOf::Typeable a => [Wrap (constraint :: * -> Constraint)] -> a -> Bool
instanceOf list a = not. null $ selectType list a

instanceWrapOf::
    [Wrap (constraint :: * -> Constraint)] ->
    Wrap (constraint2 :: * -> Constraint) -> Bool
instanceWrapOf list (Wrp a) = instanceOf list a

selectType::Typeable a =>
    [Wrap (constraint :: * -> Constraint)] ->
    a ->
    [Wrap (constraint :: * -> Constraint)]
selectType list a = filter inList list
    where inList (Wrp b) = typeOf a == typeOf b
```

Тут мы принимаем некий объект (или обёртку с объектом), список типов, относящихся к определённому классу, и фильтруем этот список по типу.
Вот для этого нам и понадобились `Typeable` и `typeOf` из пакета Data.Typeable.
Тут всё просто, если есть тип нашего объекта в списке, тогда объект относится к соответствующему классу.
(Не забывайте, Хаскелл после компиляции полностью забывает про классы.)

Итак, теперь мы можем немного больше, например, отфильтровать наш список по классу и напечатать выборку:

```haskell
main = do
    -- ...
    putStr "\nclickable objects: "
    print$ map show$ filter (\(Wrp a)->instanceOf clickableTypes a) testData
    putStr "\nserializable objects: "
    print$ map show$ filter (instanceWrapOf serializableTypes) testData
```

То есть если нам понадобятся только объекты класса `ClsClickable` или класса `ClsSerializable`, то есть объекты, обладающие строго определённой функциональностью, мы их получим.
И сможем их только... напечатать. Ничего больше. Но это уже нечто большее, нежели мы ожидали от гетерогенных списков ранее.

Идем дальше.

Нам, тем не менее, всё-таки необходимо как-то с нашими объектами работать.
И для этого напишем следующую функцию. И вот она будет корнем всей концепции, тем, ради чего мы всё и затеяли:

```haskell
-- module Main (later InferInstanceOf) where --
asInstanceOfClickable a =
    if null typeOfClass then
        Nothing
    else
        Just. unwrap. head$ typeOfClass
    where
        typeOfClass = selectType clickableTypes a
        unwrap (Wrp b) = substitute a b
        substitute::forall t1 t2. (Typeable t2, Show t2, ClsClickable t2) =>
            t1 ->
            t2 ->
            Wrap ClsClickable
        substitute x y = Wrp (unsafeCoerce x::t2)
```

Эта функция подлежит генерализации и мы это сделаем ниже. Но сейчас давайте попытаемся понять, что же тут происходит.
Мы принимаем объект. Далее, с помощью фильтрации, анализируем, относится ли этот объект к классу.
Если относится, распаковываем тип (`undefined::некий тип`) из соответствующей обёртки.
Проводим подстановку типов, то есть как бы подтверждаем с помощью функции unsafeCoerce, что наш объект относится к типу, инстанцирующему запрошенный нами класс.
И... перепаковываем наш объект в новую обёртку.
В обёртку с необходимой нам функциональностью.
Другими словами, мы «на лету» подставляем нашему объекту соответствующие словари.
И возвращаем наш объект в новой упаковке внутри типа Maybe.

Теперь мы можем полноценно работать с нашим объектом из гетерогенного списка:

```haskell
main = do
    -- ...
    putStr "\ncall click function: "
    print$ map (\a->case a of Just (Wrp a)->click a; Nothing->"")$
            map (\(Wrp a)->asInstanceOfClickable a ) testData
```

Вуаля.

Ещё один шаг. Мы обобщим нашу функцию, то есть вместо узкоспециальной функции `asInstanceOfClickable` напишем общую функцию `asInstanceOf`:

```haskell
asInstanceOf::Typeable a =>
    [Wrap (constraint :: * -> Constraint)] ->
    a ->
    Maybe (Wrap (constraint :: * -> Constraint))
asInstanceOf list a =
    if null typeOfClass then
        Nothing
    else
        Just. unwrap. head$ typeOfClass
    where
        typeOfClass = selectType list a
        unwrap (Wrp b) = substitute a b
        substitute::forall t1 t2 constraint.
            (Typeable t2, Show t2, (constraint :: * -> Constraint) t2) =>
            t1 ->
            t2 ->
            Wrap (constraint :: * -> Constraint)
        substitute x y = Wrp (unsafeCoerce x::t2)
```

...и проведём ещё пару экспериментов:

```haskell
main = do
    -- ...
    putStr "\ncall render function: "
    print$ map (\a->case a of Just (Wrp a)->render a; Nothing->"")$
        map (\(Wrp a)->asInstanceOf renderableTypes a ) testData
    putStr "\ncall serialize function: "
    print$ map (\a->case a of Just (Wrp a)->serialize a; Nothing->"")$
        map (\(Wrp a)->asInstanceOf serializableTypes a ) testData
```

...а также попробуем объединить действия из разных функциональных интерфейсов:

```haskell
main = do
    -- ...
    putStr "\ncall click and render functions: "
    print$
        map (\(a, b)->
            "click: " ++
            (case a of Just (Wrp a)->click a; Nothing->"-") ++
            "; render: " ++
            (case b of Just (Wrp b)->render b; Nothing->"-") )$
        map (\(Wrp a)->(asInstanceOf clickableTypes a, asInstanceOf renderableTypes a) )
        testData
```

...или применим преобразование последовательно:

```haskell
main = do
    -- ...
    putStr "\ncall click and render functions: "
    print$
        map (\w->"click: " ++
                case w of
                    Just (Wrp a)->
                        click a ++
                        "; render: " ++
                        case asInstanceOf renderableTypes a of
                            Just (Wrp d)->render d
                            Nothing->"-"
                    Nothing->"-"
            )$
        map (\(Wrp a)->asInstanceOf clickableTypes a ) testData
```

Конечно же, код, написанный «с пылу, с жару», не лишён изъянов.
Всё должно быть упаковано в соответствующий модуль, например `Data.InferInstanceOf` или `Data.InstanceOf`.
Такие моменты как создание списков типов, хорошо автоматизируются с помощью _TemplateHaskell_.
Для лучшего восприятия можно воспользоваться `Data.Maybe` или взаимодействовать с `Maybe` в монадическом стиле.
Для проекта, который мы разрабатываем, я, конечно же, проведу эту работу.
Но то, что уже имеется, будет большим подспорьем на нашем проекте.

И самое главное. Я попытался немного подвинуть Хаскелл в сторону мультипарадигменности.
Почему? Да, хотя бы потому, что мне самому это понадобилось и показалось интересным реализовать.

В заключение хочу сказать, что в процессе поиска решения задачи было опробовано много различных подходов.
Например, я пробовал решить задачу с использованием семейств типов, с помощью рефлексии (`Data.Reflection`), с помощью `Dict` из `Data.Constraint`, с помощью `cast`, с помощью `Data.Dynamic` и прочего.
Однако, каждый раз я заходил в тупик. Хаскелл ревностно защищает свою систему типов и жёстко пресекает все попытки её (защиту) обойти.

Буду рад, если эта статья окажется кому-то полезной.

## Список использованных в процессе подготовки материалов

1. [Typeclass example · GitHub](https://gist.github.com/orionll/0f60e891fe7ac306a002)

1. [haskell - Use of &#39;unsafeCoerce&#39; - Stack Overflow](https://stackoverflow.com/questions/22847740/use-of-unsafecoerce)

1. [ghc - Unsafe entailment with Haskell constraints - Stack Overflow](https://stackoverflow.com/questions/32481418/unsafe-entailment-with-haskell-constraints)

1. [unsafe entailment with haskell constraints www.tutel.me](https://www.tutel.me/c/programming/questions/32481418/unsafe+entailment+with+haskell+constraints)

1. [turning a dict into a constraint](https://www.tutel.me/c/programming/questions/29482576/turning+a+dict+into+a+constraint)

1. [Toy instructional example of Haskell GADTs: simple dynamic types. · GitHub](https://gist.github.com/sacundim/5386823)

1. [OOHaskell - 0509027.pdf](https://arxiv.org/pdf/cs/0509027.pdf)

1. [OOP in Haskell: implementing wxHaskell in Haskell](http://www.rubendegooijer.nl/posts/2013-04-06-haskell-oop.html)

1. [Object-Oriented Style Overloading for Haskell - Microsoft Research](https://www.microsoft.com/en-us/research/publication/object-oriented-style-overloading-for-haskell)

1. [encapsulation, mutable state, inheritance, overriding, statically checked implicit and explicit subtyping, and so on](https://arxiv.org/abs/cs/0509027)

1. [nkaretnikov/OOHaskell · GitHub](https://github.com/nkaretnikov/OOHaskell/blob/master/samples/CircBuffer.hs OOHaskell/CircBuffer.hs)

1. [oo-haskell/Store.hs at master · andorp/oo-haskell · GitHub](https://github.com/andorp/oo-haskell/blob/master/src/Store.hs)

1. [newtype Monoid example haskell - Use of &#39;unsafeCoerce&#39; - Stack Overflow](https://stackoverflow.com/questions/22847740/use-of-unsafecoerce)

1. [haskell - How to put constraints on the associated data? - Stack Overflow](https://stackoverflow.com/questions/11258536/how-to-put-constraints-on-the-associated-data)

1. [Haskell for all: Scrap your type classes](http://www.haskellforall.com/2012/05/scrap-your-type-classes.html)

1. [Constraint Kinds for GHC](http://blog.omega-prime.co.uk/2011/09/10/constraint-kinds-for-ghc/)

1. [typeclass - How do I make an heterogeneous list in Haskell? (originally in Java) - Stack Overflow](https://stackoverflow.com/questions/20997745/how-do-i-make-an-heterogeneous-list-in-haskell-originally-in-java)

1. [haskell - ConstraintKinds explained on a super simple example - Stack Overflow](https://stackoverflow.com/questions/31317159/constraintkinds-explained-on-a-super-simple-example)

1. [OOP vs type classes - HaskellWiki](https://wiki.haskell.org/OOP_vs_type_classes)

1. [Heterogenous collections - HaskellWiki](https://wiki.haskell.org/Heterogenous_collections)

1. [How to work on lists - HaskellWiki](https://wiki.haskell.org/How_to_work_on_lists)

1. [Some interesting features of Haskell’s type system | Wolfgang Jeltsch](https://jeltsch.wordpress.com/2013/02/09/some-interesting-features-of-haskells-type-system/)

1. [Typeable and Data in Haskell](http://chrisdone.com/posts/data-typeable)

1. [GHC/Type families - HaskellWiki](https://wiki.haskell.org/GHC/Type_families#Detailed_definition_of_type_synonym_families)

1. [introspection - Get a list of the instances in a type class in Haskell - Stack Overflow](https://stackoverflow.com/questions/5396783/get-a-list-of-the-instances-in-a-type-class-in-haskell)

1. [7.12. The Constraint kind](https://downloads.haskell.org/~ghc/7.8.4/docs/html/users_guide/constraint-kind.html)

1. [Typeclasses: Polymorphism in Haskell - Andrew Gibiansky](http://andrew.gibiansky.com/blog/haskell/haskell-typeclasses/#_converting_between_numeric_types)

1. [Tagging functions in Haskell - Stack Overflow](https://stackoverflow.com/questions/20576596/tagging-functions-in-haskell/20576712#20576712)

1. [haskell - AllowAmbiguousTypes and propositional equality: what&#39;s going on here? - Stack Overflow](https://stackoverflow.com/questions/27008046/allowambiguoustypes-and-propositional-equality-whats-going-on-here)

1. [24 Days of GHC Extensions: Type Families](https://ocharles.org.uk/blog/posts/2014-12-12-type-families.html)

1. [GHC/AdvancedOverlap - HaskellWiki](https://wiki.haskell.org/GHC/AdvancedOverlap)

1. [haskell - How can I read the metadata of a type at runtime? - Stack Overflow](https://stackoverflow.com/questions/28243383/how-can-i-read-the-metadata-of-a-type-at-runtime)

1. [Reflecting values to types and back - School of Haskell | School of Haskell](https://www.schoolofhaskell.com/user/thoughtpolice/using-reflection)

1. [Type Families and Pokemon. - School of Haskell | School of Haskell](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/type-families-and-pokemon)

1. [GHC/Type families - HaskellWiki](https://wiki.haskell.org/GHC/Type_families)


## Исходный код

Ниже представлен весь исходный код целиком.
Его можно скопировать в файл (например, `testInstanceOfClass.hs`) и запустить командой `runhaskell testInstanceOfClass.hs`:

```haskell
{-# LANGUAGE GADTs, ConstraintKinds, KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleInstances #-}
{-# LANGUAGE DuplicateRecordFields, RecordWildCards #-}
module Main where

import Data.Typeable (Typeable, typeOf)
import GHC.Exts (Constraint)
import Unsafe.Coerce (unsafeCoerce)

-- module Base where --
class ClsShape shape

-- module RenderableBase where --
class ClsRenderable a where
    render::a->String
data RenderableBase = RenderableBase {coords::String} deriving Show
instance ClsRenderable RenderableBase where
    render a = coords a

-- module ClickableBase where --
class ClsClickable a where
    click::a->String

-- module SerializableBase where --
class ClsSerializable a where
    serialize::a->String
data SerializableBase = SerializableBase {serializedData::String} deriving Show
instance ClsSerializable SerializableBase where
    serialize a = serializedData a

-- module Circle where --
data Circle = Circle {
    name :: String,
    renderableBase :: RenderableBase,
    serializableBase :: SerializableBase
    } deriving Show
instance ClsShape Circle
instance ClsRenderable Circle where
    render Circle{..} = "Circle " ++ name ++ " " ++ render renderableBase
instance ClsSerializable Circle where
    serialize Circle{..} = "Circle " ++ name ++ " " ++ serialize serializableBase

-- module Rectangle where --
data Rectangle = Rectangle {
    name :: String,
    renderableBase :: RenderableBase
    } deriving Show
instance ClsShape Rectangle
instance ClsRenderable Rectangle where
    render Rectangle{..} = "Rectangle " ++ name ++ " " ++ render renderableBase
instance ClsClickable Rectangle where
    click Rectangle{..} = "Rectangle " ++ name ++ " " ++ coords renderableBase ++ " clicked"

-- module Triangle where --
data Triangle = Triangle {
    name :: String,
    renderableBase :: RenderableBase
    } deriving Show
instance ClsShape Triangle
instance ClsRenderable Triangle where
    render Triangle{..} = "Triangle " ++ name ++ " " ++ render renderableBase
instance ClsClickable Triangle where
    click Triangle{..} = "Triangle " ++ name ++ " " ++ coords renderableBase ++ " clicked"

-- module InferInstanceOf where --
data Wrap (constraint :: * -> Constraint) where
    Wrp :: (Show a, Typeable a, constraint a) => a -> Wrap constraint
instance Show (Wrap a) where
    show (Wrp a) = show a

-- module Main where --
testData :: [Wrap ClsShape]
testData = [
    Wrp$ Circle    "crcl_1"  (RenderableBase "(1, 1)") (SerializableBase "Crcl1"),
    Wrp$ Circle    "crcl_2"  (RenderableBase "(2, 2)") (SerializableBase "Crcl2"),
    Wrp$ Rectangle "rect_1"  (RenderableBase "(3, 3)"),
    Wrp$ Rectangle "rect_2"  (RenderableBase "(4, 4)"),
    Wrp$ Triangle  "trngl_1" (RenderableBase "(5, 5)"),
    Wrp$ Triangle  "trngl_2" (RenderableBase "(6, 6)")
    ]

renderableTypes:: [Wrap ClsRenderable]
renderableTypes = [
    Wrp (undefined::Circle),
    Wrp (undefined::Rectangle),
    Wrp (undefined::Triangle)
    ]

clickableTypes:: [Wrap ClsClickable]
clickableTypes = [
    Wrp (undefined::Rectangle),
    Wrp (undefined::Triangle)
    ]

serializableTypes:: [Wrap ClsSerializable]
serializableTypes = [
    Wrp (undefined::Circle)
    ]

-- module InferInstanceOf where --
instanceOf::Typeable a => [Wrap (constraint :: * -> Constraint)] -> a -> Bool
instanceOf list a = not. null $ selectType list a

instanceWrapOf::[Wrap (constraint :: * -> Constraint)] -> Wrap (constraint2 :: * -> Constraint) -> Bool
instanceWrapOf list (Wrp a) = instanceOf list a

selectType::Typeable a => [Wrap (constraint :: * -> Constraint)] -> a -> [Wrap (constraint :: * -> Constraint)]
selectType list a = filter inList list
    where inList (Wrp b) = typeOf a == typeOf b

-- module InferInstanceOf where --
-- asInstanceOfClickable: only as example
asInstanceOfClickable a =
    if null typeOfClass then
        Nothing
    else
        Just. unwrap. head$ typeOfClass
    where
        typeOfClass = selectType clickableTypes a
        unwrap (Wrp b) = substitute a b
        substitute::forall t1 t2. (Typeable t2, Show t2, ClsClickable t2) => t1 -> t2 -> Wrap ClsClickable
        substitute x y = Wrp (unsafeCoerce x::t2)

asInstanceOf::Typeable a => [Wrap (constraint :: * -> Constraint)] -> a -> Maybe (Wrap (constraint :: * -> Constraint))
asInstanceOf list a =
    if null typeOfClass then
        Nothing
    else
        Just. unwrap. head$ typeOfClass
    where
        typeOfClass = selectType list a
        unwrap (Wrp b) = substitute a b
        substitute::forall t1 t2 constraint. (Typeable t2, Show t2, (constraint :: * -> Constraint) t2) => t1 -> t2 -> Wrap (constraint :: * -> Constraint)
        substitute x y = Wrp (unsafeCoerce x::t2)

-- module Main where --
main = do
    putStr "all objects: "
    print$ map (\(Wrp a)->show a) testData
    putStr "\nclickable objects: "
    print$ map show$ filter (\(Wrp a)->instanceOf clickableTypes a) testData
    putStr "\nserializable objects: "
    print$ map show$ filter (instanceWrapOf serializableTypes) testData
    putStr "\ncall click function: "
    print$ map (\w->case w of Just (Wrp a)->click a; Nothing->"")$
            map (\(Wrp a)->asInstanceOfClickable a ) testData
    putStr "\ncall render function: "
    print$ map (\w->case w of Just (Wrp a)->render a; Nothing->"")$
            map (\(Wrp a)->asInstanceOf renderableTypes a ) testData
    putStr "\ncall serialize function: "
    print$ map (\w->case w of Just (Wrp a)->serialize a; Nothing->"")$
            map (\(Wrp a)->asInstanceOf serializableTypes a ) testData
    putStr "\ncall click and render functions: "
    print$ map (\(w1, w2)-> "click: " ++
            (case w1 of Just (Wrp a)->click a; Nothing->"-") ++
            "; render: " ++
            (case w2 of Just (Wrp b)->render b; Nothing->"-") )$
            map (\(Wrp a)->(asInstanceOf clickableTypes a, asInstanceOf renderableTypes a) ) testData
    putStr "\ncall click and render functions: "
    print$ map (\w->"click: " ++
                    case w of
                        Just (Wrp a)->
                            click a ++
                            "; render: " ++
                            case asInstanceOf renderableTypes a of
                                Just (Wrp d)->render d
                                Nothing->"-"
                        Nothing->"-"
                    )$
            map (\(Wrp a)->asInstanceOf clickableTypes a ) testData
```
