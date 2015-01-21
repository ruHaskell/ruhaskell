---
author:      Александр Бондаренко
title:       Минимальный Yesod сайт для публикации на keter
tags:        Yesod, Keter
description: Keter это штука для one-click deploy сайтов на Yesod. Если в двух словах, то на production висит демон keter и мониторит свой каталог incoming, куда загружаются пакеты. Из полученых тарболов он достаёт статику, настройки и бинари, проверяет сайт, запускает и переключает на него новые запросы.
---

Keter это штука для one-click deploy сайтов на Yesod. Если в двух словах, то на production висит демон keter и мониторит свой каталог incoming, куда загружаются пакеты. Из полученых тарболов он достаёт статику, настройки и бинари, проверяет сайт, запускает и переключает на него новые запросы.

При желании, код включать в пакет не обязательно и можно просто раздавать статику с него же - он вполне себя хорошо чувствует даже не прикрываясь nginx.

Ну а для кода у yesod есть одноимённая команда (keter, если кто не догадался), которая соберёт пакет со всем барахлом и, опционально, загрузит его по scp.

По работе мне все эти шекспировски страсти из штатной поставки не нужны, а вот автодеплой пригодится. В принципе, итоговый результат по минимальности может потягаться даже со scotty, что определённо хороший знак.

---------------------------------------------------

Первым делом надо сделать каталог проекта:

```
$ mkdir mini
$ cd mini
$ cabal sandbox init
Writing a default package environment file to
/home/wiz/src/mini/cabal.sandbox.config
Creating a new sandbox at /home/wiz/src/mini/.cabal-sandbox
```

Для использования cabal надо создать файл пакета, это тоже обычная процедура.

```
$ cabal init
...
What does the package build:
   1) Library
   2) Executable
Your choice? 1
...
```

Создадим само приложение:

```
module Application where

import Yesod.Core
import Data.Text (Text)

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

instance Yesod App

getHomeR :: Handler Text
getHomeR = return "Lolsies"
```

Вот и всё, никаких супер-ужасов и развесистых модулей с сотнями автогенерённого кода.
Теперь надо добавить экспорт приложения и зависимости в пакет. Нас интересует дефолтный блок library в конце файла:

```
...

executable mini
  -- exposed-modules:
  -- other-modules:
  -- other-extensions:
  build-depends:       base >=4.6 && <5
  -- hs-source-dirs:
  default-language:    Haskell2010
```

Добавляем наше приложение в exposed-modules, фичи языка в default-extensions и ставим yesod-core и text в зависимости:

```
...

library
  -- hs-source-dirs:
  default-language: Haskell2010
  default-extensions:
    OverloadedStrings
    TemplateHaskell
    QuasiQuotes
    TypeFamilies

  exposed-modules:
    Application

  build-depends:
      base >=4.6 && <4.7
    , yesod-core
    , text
```

Запускаем загрузку барахла в наш sandbox (и идём пить чай):

```
$ cabal install -j --only-dependencies
...
Installed yesod-core-1.2.6.2
Пробуем собрать пакет:
$ yesod keter
yesod: InvalidYaml (Just (YamlException "Yaml file not found: config/keter.yaml"))
```

Упс, не получилось. Но зато сразу понятно что надо делать (читать мануал по keter).

Создаём необходимые каталоги и их содержимое:

```
$ mkdir config static
$ nano config/keter.yaml
Прописываем туда настройки keter - что, где деплоить:
exec: ../dist/build/mini/mini
args:
    - production
host: mini.example.org

# copy-to: keter@keter.example.org:/opt/keter/incoming
```

Последняя строчка для автодеплоя: после успешной сборки оно само загрузит результат через scp, где его подхватит сервер.
Теперь на всё это хозяйство вешаем загрузчик:

```
module Main where

import Yesod.Core.Dispatch (warpEnv)
import "mini" Application (App(..))

main :: IO ()
main = warpEnv App
```

PackageImports используются чтобы оно брало всё из библиотеки приложения и нам не пришлось дублировать все завимости ещё и для бинаря.

Прописываем загрузчик в cabal:

```
executable mini
  main-is: Main.hs
  ghc-options: -O2 -threaded
  default-language: Haskell2010
  default-extensions:
    PackageImports
  build-depends:
      base >=4.6 && <4.7
    , mini
    , yesod-core
```

Вот теперь всё готово, заряжаем:

```
$ yesod keter
cleaning...
Resolving dependencies...
Configuring mini-0.1.0.0...
Warning: The 'license-file' field refers to the file 'LICENSE' which does not
exist.
Building mini-0.1.0.0...
Preprocessing library mini-0.1.0.0...
[1 of 1] Compiling Application      ( Application.hs, dist/build/Application.o )
...
... загрузка модулей для TH ...
...
Preprocessing executable 'mini' for mini-0.1.0.0...
[1 of 1] Compiling Main             ( Main.hs, dist/build/mini/mini-tmp/Main.o )
Linking dist/build/mini/mini ...
mini.keter                                         100% 4295KB   1.4MB/s   00:03   
```

---------------------------------------------------

Вот и всё. Теперь добавляем всю фигню в приложение, компилим, делаем yesod keter. У себя на локальном сервере можно даже в inotifywait-скрипт завернуть и будет не хуже yesod devel (который, кстати, не работает - но это уже тема для отдельного поста).

Завернув это в репозиторий, получим скелет для быстрого старта.
