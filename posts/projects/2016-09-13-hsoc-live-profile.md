---
author:         Anton Gushcha
title:          Сервер для онлайн профилирования приложений
tags:           HSOC, eventlog, profiling
description:    Результаты Haskell Summer of Code 2016 проекта по улучшению eventlog.
---

Лето закончилось,
и вместе с ним завершается студенческая программа
[Haskell Summer of Code 2016](https://summer.haskell.org/),
которая в этом году заменяет Google Summer of Code для хаскеллистов.
Мой проект приняли по счастливой случайности:
дополнительный спонсор появился к концу июня.
Итого, у меня было всего два месяца для осуществления проекта,
и я думаю, что они не пропали даром.

# О чём проект

На текущий момент существует несколько инструментов
для профилирования хаскелльных программ:

1. [Сборка с `-prof`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/profiling.html),
  которая позволяет замерять статистику
  по «cost centers» и использованию heap'а.
  Для использования этого вида профайлера
  необходимо перекомпилировать приложение и все его зависимости,
  что в больших проектах занимает ощутимое время.
  Но что более важно, это значительный вносимый overhead в работу приложения.

2. [Eventlog](https://ghc.haskell.org/trac/ghc/wiki/EventLog),
  который позволяет записывать события GC, планировшика потоков,
  пользовательские сообщения и прочее.
  В лог также попадает также статистика использования памяти,
  но не так детально, как в «настоящем» профилировании.
  Главной особенностью данного инструмента является то,
  что он не требует длительной пересборки и даёт малый overhead.

3. [Ticky-ticky](https://ghc.haskell.org/trac/ghc/wiki/Debugging/TickyTicky)
  профайлер --- довольно специфическая разновидность,
  которая добавляет счетчики к каждому объекту на уровне
  [STG](https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/GeneratedCode).
  Этот вид профайлера нужен, в основном, разработчикам GHC.

Для проблем с многопоточностью, проблем, которые проявляются только под нагрузкой, или просто для вероятностных багов целесообразно записывать eventlog и потом анализировать происходящее через специальные инструменты, например, [ThreadScope](https://wiki.haskell.org/ThreadScope) и [ghc-events-analyze](http://www.well-typed.com/blog/86/).

Однако существующая реализация eventlog имеет важный недостаток,
которые не позволяет реализовать онлайн профилирование приложений.
RTS создаёт файл для лога и пишет туда все события,
гипотетическим инструментам для онлайн-профилирования
приходилось бы решать проблему чтения из этого файла,
пока туда ведется запись.
Также при значительном потоке сообщений
размер лога может прибывать по 100 МБ в минуту,
рано или поздно лог займет всё свободное пространство на диске.

# Цели проекта

Мой HSOC-проект направлен на доработку eventlog
и реализации proof-of-concept для онлайн-профиловщика приложений:

1. Доработка RTS для возможности перенаправления событий в произвольный файловый дескриптор или передачу событий через память на уровень Haskell. Сюда также входит возможность динамически изменять размер промежуточных буферов лога.

2. Разработка приложения-монитора, которое бы перехватывало поток сообщений из профилируемой программы и отдавала его удаленному инструменту для профилирования.

3. Разработка удалённого инструмента для отображения eventlog
  на основе веб-приложения.

# Результаты

В целом, все три задачи были решены.
Изначально было запланировано гораздо больше,
но по тем или иным причинам от фич пришлось отказываться,
в основном, из-за дефицита времени.

## RTS

Изменения RTS оформлены в отдельный
[тикет](https://ghc.haskell.org/trac/ghc/ticket/12582)
и залиты на [Phabricator](https://phabricator.haskell.org/D2522),
где их ждёт жёсткое ревью и доработка.

## Monitor

Полный data flow событий от их создания и до прихода в инструмент профиловщика выглядит так:

![](/files/posts/2016-09-13/live-profile-monitor-data-flow-ru.png#center)

[Монитор](https://github.com/NCrashed/live-profile-monitor) позволяет подключаться к приложениям, к которым добавлена маленькая [библиотека-пиявка](https://github.com/NCrashed/live-profile-monitor/tree/master/live-profile-leech):

```
hs-live-profile --RTS myapp +RTS -lm
```

и потом передавать сообщения через TCP соединение, где на другом конце профилировщик вешает свои [callback'и](https://github.com/NCrashed/live-profile-monitor/tree/master/live-profile-client):

``` haskell
data ClientBehavior = ClientBehavior {
  clientOnHeader :: !(Header -> IO ())  -- ^ Вызывается, когда пришел полный заголовок лога
, clientOnEvent :: !(Event -> IO ()) -- ^ Вызывается, когда пришло новое событие
, clientOnService :: !(ServiceMsg -> IO ()) -- ^ Вызывается, когда пришло служебное сообщение
, clientOnState :: !(EventlogState -> IO ()) -- ^ Вызывается, когда пришел новый снимок состояния лога
, clientOnExit :: !(Maybe SomeException -> IO ()) -- ^ Вызывается при выходе из потока клиента
}
```

## Сервер-профилировщик

Самая сырая часть проекта,
так как на неё пришлось самое малое количество времени.
Итого, сервер может успешно пародировать
[ghc-events-analyze](http://www.well-typed.com/blog/86/),
подключаться к монитору, управлять логами через админку:

![](/files/posts/2016-09-13/screen-fib-bined.png#center)

[Сервер](https://github.com/NCrashed/live-profile-server)
можно как развернуть у себя локально,
так и попробовать
[развернутый вариант онлайн](http://liveprofile.teaspotstudio.ru/).
Сейчас есть проблемы с производительностью построения диаграмм,
для логов размеров около 50 МБ они могут строиться по 5--7 минут.

Особенностью реализации сервера является то, что он полностью написан на Haskell, в том числе фронтенд, который реализован на GHCJS + [reflex](https://github.com/reflex-frp/reflex-platform) + [diagrams](http://projects.haskell.org/diagrams/#), а серверная часть вовсю использует [servant](http://haskell-servant.readthedocs.io/en/stable/) и type-level магию.

## Сопутствующие библиотеки

В процессе написания HSOC проекта я старался выносить все полезные куски кода в отдельные библиотеки:

* [aeson-injector](http://hackage.haskell.org/package/aeson-injector) ---
  типы-обёртки для инъекции полей в генерируемые JSON'ы.

* [servant-auth-token-api](http://hackage.haskell.org/package/servant-auth-token-api)
  и [servant-auth-token](http://hackage.haskell.org/package/servant-auth-token) ---
  библиотека для token-авторизации для servant.
  Эта библиотека эксплуатирует модульность servant'а
  и позволяет с минимальными усилиями
  встроить авторизацию с пользователями и группами пользователей
  внутрь своего приложения.

### servant-rest-derive

В ходе разработки сервера была получена экспериментальная библиотека, которая может только из описания [vinyl](http://hackage.haskell.org/package/vinyl) структур вывести RESTful сервер:

Описание API:

``` haskell
-- | Connection to remote application
type Connection = FieldRec '[
    '("name", Text)
  , '("host", Text)
  , '("port", Word)
  , '("lastUsed", Maybe UTCTime)
  ]

instance Named Connection where
  getName _ = "Connection"

-- | Correspoinding patch record
declareVinylPatch ''Connection

-- | API about connections to remote Haskell applications that we profile
type ConnectionAPI = "connection" :> RESTFull Connection "connection"
```

Вывод сервера:
``` haskell
connectionServer :: ServerT ConnectionAPI App
connectionServer = restServer (Proxy :: Proxy '[ 'GET, 'POST, 'PUT, 'PATCH, 'DELETE])
  (Proxy :: Proxy Connection) (Proxy :: Proxy "connection")
  (Proxy :: Proxy App)
```

Данная библиотека заслуживает отдельного поста и будет загружена на Hackage в скором будущем.

# Будущее проекта

Я не собираюсь бросать проект,
так как он ещё далеко от стадии «полезный для реальных применений».
Далее перед мной стоят следующие задачи:

* Пройти код-ревью и смержить изменения RTS в GHC.
  На момент написания поста в Phabricator уже выразили
  пару замечаний по документации и реализации,
  но принципиальных проблем для попадания новых возможностей в GHC нет.

* Добиться real-time отрисовки диаграмм,
  чтобы графики строились для логов инкрементально,
  а сервер имел опцию держать в базе только определённое временно́е окно событий.

* Добавить другие виды графиков и диаграмм. Потенциально сервер может быть полным аналогом ThreadScope и ghc-events-analyze и даже вычленять статистику по используемой памяти.

* Многопользовательский режим использования сервера. Возможность заливать приватные логи на сервер.

# Посты на английском

Более подробно о проекте и моих изысканиях по реализации проекта можно прочесть
в блоге на английском языке:

* [Оригинальный proposal проекта](http://ncrashed.github.io/blog/posts/2016-06-12-hsoc-acceptance.html)

* [Реализация изменений в RTS](http://ncrashed.github.io/blog/posts/2016-06-22-hsoc-rts.html)

* [Сказ о мониторе](http://ncrashed.github.io/blog/posts/2016-07-20-hsoc-monitoring-library.html)

* [Отчёт о результатах HSOC](http://ncrashed.github.io/blog/posts/2016-09-11-hsoc-results.html)
