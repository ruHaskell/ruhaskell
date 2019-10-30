---
author:         Юрий Сыровецкий
title:          Effects in Haskell
tags:           effects, purity
description:    Effects implementation in Haskell.
---

In this article we will demonstrate that procedures (functions with effects)
do not require special support from the programming language and that 
regular "pure" functions can be used to handle these procedures with effects. 

As an example we will use Haskell, a pure functions language, which does not have the embedded effects in a way "impure" C, OCaml, JavaScript do.
However, we can build pure, controllable effects in Haskell, which 
serve our goals quite similarly as "impure" effects. 


<!--

Make a footnote:

В [предыдущей статье](../10/effects.html) мы познакомились с основными видами эффектов в математике и программировании.

-->


<!-- Понимаю, знаю как перевести -->

_Замечание о терминах._
Я позволю себе называть параметрические типы с опущенными параметрами просто
_типами_,
хотя строгий хаскелит употребил бы термин _конструктор типов_.
Конструктор типов сам типом не является, но принадлежит языку типов.
В разговорной речи и не очень строгих статьях, как эта,
такая вольность вполне допустима, чтобы речь звучала не так тяжело
(«ехал конструктор через конструктор...»),
благо, ни к каким противоречиям при программировании это не приводит.
Если параметр не указан, то это конструктор типа,
а если по контексту подразумевается всё-таки конкретный тип,
значит, речь идёт о конструкторе с произвольными значениями параметров.

## 0. No effects

We can represent a pure funciton `f :: a -> b` as a black box that takes in a value of type `a` and returns a value of type `b`:

<center>![](../../../../../files/posts/2018-01-18/pure.svg)</center>

## 1. Partiality effect

<center>![](../../../../../files/posts/2018-01-18/partial.svg)</center>

A partial fucntion either returns a result or it doesn't.
A sum data type well captures this behaviour. 

```haskell
data Maybe a = Nothing | Just a
```

Value of type `Maybe a` either contains a value 
of type `a`, or there is no value.

We can describe a partial procedure that optionally 
returns type `b` as a function that always returns `Maybe b` type.

<center>![](../../../../../files/posts/2018-01-18/partial-pure.svg)</center>

```haskell
p :: a -> Maybe b
```

Here is an example.

```haskell
headM :: [a] -> Maybe a
headM []    = Nothing -- cannot take a head of empty list 
headM (x:_) = Just x  -- head of non-mepty list - here it is!
```

Please note that `Maybe` type belongs to `Functor`, `Applicative`,
`Monad` and other interesting and useful typeclasses.

In practice we can also use `Either` and `Except` types. They
implement similar effect as `Maybe`, but 
also add additional information why a computation was not completed. 

In example below we introduce an error data type
`MyError` with a sole value `EmptyList`.
`MyError` will be used in other code examples below. 

```haskell
data Either a b
    = Left a  -- considered an error
    | Right b -- considered success

data MyError = EmptyList -- denotes an custom error name

headE :: [a] -> Either MyError a
headE []    = Left EmptyList
headE (x:_) = Right x
```

<!-- Тут бабах MonadError, когда читатель ни сном ни духом. 
     Может как-то подготовить, о том, что о будет дальше?-->

Partiality can be combined with other effects. Below we use 
`MonadError` from [mtl](https://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-Except.html) package for exception processing:

```haskell
headE
    :: MonadError MyError m -- 'm' type constructor supports "error" effects
    => [a] -> m a
headE (x:_) = pure x
headE []    =
    throwError EmptyList -- we add an error effect, it terminates further computations
```

Partiallity is the only effect that can arise from inside Haskell.
The rest of the effects require some act of the outside world, while 
partiality situation can be constructed by means of Haskell itself
as we show in examples below. 

<!-- 

На самом деле частичность — единственный неуправляемый эффект,
доступный в Хаскеле непосредственно.

EP: доступно непонятно - кому? После "Доступно" читатель 
ждет, что про что-то хорошее сейчас скажут, а тут о 
зависании внутри цикла. 

На самом деле частичность — единственный неуправляемый эффект, 
который может быть возникнуть непосредственно 
из-за использования самих конструкций языка Haskell, 
а не явлений внешнего мира.

Partiallity is the only incontrollable effect that can 
arise from "inside" Haskell by using the contructs of 
the language itself. All other effects are produced
by outside world.    

-->


<!-- 
Любая функция может оказаться частичной,
то есть зависнуть или выбросить исключение,
по ошибке или из-за несовершенства реального мира.


EP: Тут что-то нелогичное с позиций чистых функци - им-то какое
дело до несовершенства реального мира?
-->

Going into an infinte loop is one of partiality effect: 


```haskell
-- a simplistic infinite loop
x = x -- computation of 'x' requires 'x' again and again 

-- a slightly longer example:
-- takeWhile (< 10) will require a value 10 or above
-- to stop, there will be no such value in 
-- [2, 1 ..], so length of infinite list 
-- will never be computed
n = length $ takeWhile (< 10) [2, 1 ..]
```


<!-- Понимаю, знаю как перевести -->

In Turing-complete languages the possibility of an infinite loop
cannot be avoided. There are non-Turing-complete programming 
languages, where the programs will avoid partiality effect
and the program 'totality' is be garanteed, but the drawback
is an increase in langauge complexity. [This link](https://stackoverflow.com/questions/315340/practical-non-turing-complete-languages) provides a discussion of several non-Turing-complete languages 
with respect to program termination guarantee.

Luckily in Haskell we can use controlled partiallity and 
cases of uncontrolled partiallity are rather rare.

## 2. Indeterminism (uncertainty) effect

### 2.1. Instability effect

<center>![](../../../../../files/posts/2018-01-18/unstable.svg)</center>

Если процедура для одного и того же значения аргумента может вернуть от раза
к разу разные результаты,
это значит, что на самом деле результат зависит от чего-то ещё.

Даже генератор случайных чисел (настоящий, аппаратный) — это «чистая» функция, зависящая от состояния источника энтропии.

Чтобы представить этот эффект чистой функцией,
надо всего лишь неявную зависимость сделать явной.

<center>![](../../../../../files/posts/2018-01-18/unstable-pure.svg)</center>

```haskell
p :: a -> r -> b
```

Пример.

<!--
EP: желательно определить, что такое `Config`, откуда он 
берется и что значит запись `Config{dataDir}` - она стандартная 
или через какое-то расширение доступна.

eff.hs:2:12: error:
    Illegal use of punning for field `dataDir'
    Use NamedFieldPuns to permit this
  |
2 | getDataDir Config{dataDir} = dataDir
  |            ^^^^^^^^^^^^^^^

getDataDir выглядит какой-то искуственной - просто 
выдает аттрибут из какой-то структуры данных?

Можно ли обойтись примером без использования расширений?
-->


```haskell
getDataDir :: Config -> FilePath
getDataDir Config{dataDir} = dataDir
```

Для удобства рассуждений об эффектах удобно ввести синоним

EP (дополнение, для редактирования): ...синоним для типа `r -> b`. Мы назовем этот новый тип `Reader`. Этот тип задается с помощью двух параметров типов `r` и `b`. Типу `Reader`, 
согласно нашему опрделению, будут соответстовать функции, аргументом
которых является значение типа `r`, а результатом функции является значение типа `b`. 


```haskell
type Reader r b = r -> b
p :: a -> Reader r b -- EP: это эквивалент a -> r -> b ?
```

```haskell
-- процедура, читающая неявное значение и возвращающая его
-- EP: почему значения являются неявным?
ask :: Reader r r
ask = id

getDataDir :: Reader Config FilePath
getDataDir = do
    Config{dataDir} <- ask -- читаем конфиг, переданный неявно
    pure dataDir
```

Можно комбинировать неявную зависимость с другими эффектами:

```haskell
data MyError = DataDirNotSpecified

getDataDir
    :: ( MonadError MyError m
       , MonadReader Config m
         -- 'm' поддерживает эффект неявной зависимости от 'Config'
       )
    => m FilePath
getDataDir = do
    Config{mDataDir} <- ask -- читаем конфиг, переданный неявно
    case mDataDir of
        Nothing      -> throwError DataDirNotSpecified
        Just dataDir -> pure dataDir
```

Обратите внимание, что тип `Reader r`
(конструктор типа `Reader`, частично применённый к одному аргументу)
принадлежит `Functor`, `Applicative`,
`Monad` и многим другим интересным и полезным классам.

### 2.2. Эффект множественности

<center>![](../../../../../files/posts/2018-01-18/many.svg)</center>

Здесь всё просто и очевидно.
Функция, дающая много ответов сразу — это функция,
имеющая единственный ответ-множество.

В Хаскеле есть тип `Set` для множеств,
но для моделирования эффекта множественности оказывается более удобным список —
`[]`.

<center>![](../../../../../files/posts/2018-01-18/many-pure.svg)</center>

```haskell
p :: a -> [b]
```

Пример.

```haskell
rollADie :: Int -> [Int]
rollADie n = [1..n]

rollTwoDiceAndSum :: Int -> [Int]
rollTwoDiceAndSum n = do
    a <- rollADie n
    b <- rollADie n
    pure $ a + b
```

Обратите внимание, что тип `[]` (конструктор типа списка)
принадлежит `Functor`, `Applicative`,
`Monad` и многим другим интересным и полезным классам.

Посколько множество результатов может быть и пустое,
то множественность можно рассматривать как частный случай случай частичности, —
функция, возвращающая множество ответов,
может для некоторых значений аргумента вернуть пустое множество,
то есть не вернуть ни одного ответа.

Таким образом, тип `[]` реализует и эффект частичности.

## 3. Побочный эффект

<center>![](../../../../../files/posts/2018-01-18/side.svg)</center>

Побочный эффект — это просто неявный результат. Сделаем же неявное явным!

<center>![](../../../../../files/posts/2018-01-18/side-pure.svg)</center>

```haskell
p :: a -> (b, s)
```

Для удобства рассуждений об эффектах удобно ввести обёртку

EP: ... которую мы назовем `Putter`.  Конструктор `Putter (b, s)` 
будет содержать кортеж из значений типа `b` и типа `s`, а обозначаться 
это тип будет `Putter s b`.

```haskell
newtype Putter s b = Putter (b, s)
p :: a -> Putter s b
```

Обратите внимание, что тип `Putter s`
(конструктор типа `Putter`, частично применённый к одному аргументу)
принадлежит `Functor`, `Applicative`,
`Monad` и многим другим интересным и полезным классам.

<!-- Не очень понятно, из чего следует, что он должен к какому-то типу принаделжать,
для него же нет ни инстансов, ни определений фукнций.  -->


На практике чаще применяется тип `Writer`, структурно идентичный,
но с более полезными свойствами: все побочные эффекты собираются в моноид.

<!-- EP: может привести сигнатуру этого типа Writer? -->

<!-- Тут читателю еще и моноид прилетает... может объяснить 
хот ябы поверхностно, чем моноид хорош для этой функции? -->

... , который позволяет "склеивать" побочные эффекты в длинную колбасу,
<!-- переформулирвать ;) --> например, в список сообщений логгирования 
или сообщений об ошибке. 

Пример.

```haskell
-- процедура, откладывающая побочное значение
tell :: w -> Writer w ()
tell w = Writer ((), w)

-- процедура с побочным эффектом журналирования
sumWithLog :: Int -> Int -> Writer String Int
sumWithLog x y = do
    tell $ "sum: x = " ++ show x ++ "\n" -- запишем в лог аргументы процедуры
    tell $ "sum: y = " ++ show y ++ "\n"
    let result = x + y
    tell $ "sum: result = " ++ show result ++ "\n" -- и результат запишем
    pure r -- *** должно быть result ? ***

-- функция склейки процедур, спрятанная в do-синтаксисе
-- тут-то и происходит сборка моноида
(>>) :: Monoid w => Writer w a -> Writer w b -> Writer w b
Writer (_, w1) >> Writer (b, w2) = Writer (b, w1 <> w2)
```

Можно комбинировать!

```haskell
data MyError = DataDirNotSpecified
type AccessCounter = Sum Int

-- процедура с побочным эффектом подсчёта количества вызовов
getDataDir
    :: ( MonadError MyError m
       , MonadReader Config m
       , MonadWriter AccessCounter m
         -- 'm' поддерживает побочный эффект счётчика
       )
    => m FilePath
getDataDir = do
    tell 1 -- добавить 1 ко счётчику обращений
    Config{mDataDir} <- ask
    case mDataDir of
        Nothing      -> throwError DataDirNotSpecified
        Just dataDir -> pure dataDir
```

## 2 + 3. Эффект состояния

<center>![](../../../../../files/posts/2018-01-18/state.svg)</center>

Если соединить результат побочного эффекта и источник нестабильности,
из их комбинации (композиции) получается эффект состояния — процедура,
которая может и зависеть от текущего состояния «переменной»,
и задавать ей новое состояние.

Проведя рассуждения, аналогичные случаям `Reader` и `Putter`, получим

<center>![](../../../../../files/posts/2018-01-18/state-pure.svg)</center>

```haskell
p :: a -> s -> (b, s)
```

Для удобства рассуждений об эффектах удобно ввести обёртку

```haskell
newtype State s b = State (s -> (b, s))
p :: a -> State s b
```

Пример.

```haskell
-- получить значение внутренней переменной
get :: State a a
get = State $ \s -> -- старое значение
    ( s -- результат
    , s -- новое значение переменной совпадает со старым
    )

-- присвоить значение внутренней переменной
put :: a -> State a ()
put s = State $ \_ -> -- старое значение игнорируем
    ( () -- результат
    , s -- новое значение переменной
    )

-- изменить значение внутренней переменной
modify :: (a -> a) -> State a ()
modify f = State $ \s -> -- старое значение
    ( () -- результат
    , f s -- новое значение переменной
    )

-- процедура-генератор псевдослучайных чисел по простейшей формуле
prng
    :: State
        Int -- тип внутренней переменной
        Int -- тип результата
prng = do
    modify $ \s -> s * 23 + 97
    get
```

Комбинируем с предыдущими эффектами.

```haskell
type Storage = Map FilePath Value -- имитация файловой системы для демонстрации

-- процедура с побочным эффектом подсчёта количества вызовов
putData
    :: ( MonadError MyError m
       , MonadReader Config m
       , MonadWriter AccessCounter m
       , MonadState Storage m -- 'm' поддерживает эффект изменения 'Storage'
       )
    => m FilePath
putData key value = do
    dataDir <- getDataDir
    modify $ Map.insert (dataDir </> key) value -- вносим изменения в Storage
```

Не будет сюрпризом, что тип `State s`
(конструктор типа `State`, частично применённый к одному аргументу)
принадлежит `Functor`, `Applicative`,
`Monad` и многим другим интересным и полезным классам.

## 0. Отсутствие эффектов (продолжение)

Рассмотрим тип

```haskell
newtype Identity a = Identity a
```

Тип `Identity a` полностью аналогичен типу `a`.
То есть это своего рода функция `id`, только на уровне типов.

Тип `Identity` не может выражать никаких эффектов.
С другой стороны, можно сказать, что он выражает отсутствие эффектов.

Конечно же, конструктор типа `Identity` принадлежит `Functor`, `Applicative`,
`Monad` и многим другим интересным и полезным классам.

## Эффекты! Эффекты повсюду!

В Хаскеле есть специальный тип `IO`, реализующий сразу все возможные эффекты.
В нём можно прерывать программу, обмениваться данными с ресурсами,
не указанными явно в аргументах и возвращаемом значении.
В `IO` нет ограничений, доступен на чтение и запись весь мир,
в том числе ядерные ракеты.
Как если бы у нас был в программе объект типа `RealWorld` и мы могли бы изменять
его, как переменную под `State`.

Реализация `IO` не определена в спецификации языка.
Если вы заглянете в исходники стандартной библиотеки, скорее всего,
действительно увидите что-то подобное `State RealWorld`,
но эта реализация нужна только для внутренних нужд компилятора и пропадает при
компиляции,
так что верить ей не стоит.

Вы уже догадались, что конструктор типа `IO` принадлежит `Functor`,
`Applicative`, `Monad` и многим другим интересным и полезным классам.

Выше я утверждал, что в Хаскеле никаких побочных эффектов нет,
а сейчас рассказываю, что в `IO` всё можно — только руку протяни.
Этот парадокс разрешается просто: запускает ракеты не сам язык, а тип `IO`,
который в некотором смысле отделён от языка и подчиняется ему.
Хаскель управляет `IO`, а не наоборот.
Нет возможности запустить `IO` в произвольном месте программы
(ну ладно, есть пара грязных хаков, но честных способов нет),
только в специально отведённых.

Иными словами, `IO` даёт доступ к неуправляемым эффектам,
но доступ к самому `IO` управляемый.

## Процедуры и чистые функции

Вернёмся к нашей дихотомии.

С одной стороны, функция — это просто процедура без эффектов.

С другой стороны, нам удалось все процедуры выразить в типах вида

```haskell
p :: a -> f b
```

где _b_ — тип результата, он может быть любым, _f_ — тип, реализующий эффект.

Иными словами, процедура — это чистая функция, вычисляющая эффект.

Обратите внимание, что все упомянутые типы эффектов принадлежат `Functor`,
`Applicative`, `Monad` и многим другим интересным и полезным классам
(разве что `Writer` с некоторыми ограничениями).
Эти классы предоставляют общий механизм для работы с эффектами.
Подробнее можно почитать в
[www.staff.city.ac.uk/~ross/papers/Applicative](http://www.staff.city.ac.uk/~ross/papers/Applicative).

Существуют и другие типы, реализующие эти эффекты иными,
более сложными и полезными способами.
Они всегда являются аппликативными функторами и почти всегда монадами.

## Управляемые эффекты

Что даёт возможность управлять эффектами?
Переводя процедуры с эффектами в пространство чистых функций, мы,
не теряя выразительности, получаем возможности

- рассуждать о процедурах и их эффектах как о сущностях внутри программы,
  - в частности, буквально читать в типе функции эффекты, возможные в ней,
- проверять их корректность системой типов компилятора,
- в каждой функции ограничивать пространство эффектов только необходимыми для
  решения задачи,
- создавать столь же управляемые комбинации эффектов
  (это уже тема для отдельной статьи или даже книги).

## Заключение

Соберём в таблицу аспекты чистоты, эффекты, нарушающие эти аспекты, и типы,
моделирующие эти эффекты.

| Свойство чистой функции   | Эффект               | Тип                       |
|---------------------------|----------------------|---------------------------|
| Все                       | Нет                  | `Identity`                |
| Тотальность               | Частичность          | `Maybe`, `Either e`, `[]` |
| Детерминированность       | Нестабильность       | `Reader r`                |
| Детерминированность       | Множественность      | `[]`                      |
| Нет побочных эффектов     | Побочный             | `Putter s`, `Writer w`    |
| Детерминированность и нет побочных | Состояние   | `State s`                 |
| Любое                     | Любой                | `IO`                      |

All above mentioned types are embedded into the language 
and can be found in easily in standard packages like `base` and `mtl`, except `Putter`. I created `Putter` as an illustration, but one can envisage it as `State`, contrained to operation `put` only.

To run the examples in this arctile I attach their code in [effects.hs](effects.hs).

We can make a follow-up on effects implementation through types 
in our further publications, based on reader interest.