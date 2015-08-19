---
author: Денис Шевченко
title:  stack и Yesod: дружба навек
tags:   stack, Yesod
description: Stack и Yesod: как их подружить.
---

Приветствую, друзья!

Бодро [сообщив всему миру](https://twitter.com/dshevchenko_biz/status/631431767338143748) о том, что отныне (и вовек?) я использую Stack, приступил я к переводу моих проектов на этот самый Stack.

Начиналось всё прекрасно: я снёс Haskell Platform во избежание путаницы между "системным" ghc и установленным(и) с помощью `stack setup`. Но затем я столкнулся с одной небольшой проблемкой, решением которой хочу поделиться с вами. Думаю, кому-то пригодится.

## Yesod старый

Один из моих Haskell-проектов базируется на Yesod. И очень уж я привык к команде вида:

```bash
$ yesod devel -p 3010
```

которая запускает проект на локальном сервере, да ещё и автоматически пересобирает его при изменениях в коде. Вот тут-то и ждала меня трудность.

Дело в том, что команда `yesod devel` вызывает утилиту `yesod`, устанавливаемую с пакетом [`yesod-bin`](http://hackage.haskell.org/package/yesod-bin). И вот запускаю я её в моём проекте - и получаю ерунду:

```bash
$ yesod devel -p 3010
...
Resolving dependencies...
cabal: At least the following dependencies are missing:
aeson -any,
blaze-html -any,
conduit -any,
data-default -any,
...
```

Стоп, а почему `cabal`? А потому: работать эта утилита умеет исключительно с `cabal` и знать ничего не знает про `stack`.

## Yesod новый

Как вы понимаете, о вышеупомянутой проблеме прекрасно знал великий наш Снойман, а посему 29 июня он опубликовал статью [stack support for yesod devel](http://www.yesodweb.com/blog/2015/06/stack-support-yesod-devel). В ней предложено решение: установить пакет `yesod-bin` версии `1.4.11` (от того же 29 июня), поддерживающий `stack`, после чего запускать сие через `stack exec`.

Сказано - сделано. Утилита `yesod-bin` у меня уже имелась, причём той самой версии `1.4.11` (из `stack`-снимка `2.18`). Запускаю в проекте:

```bash
$ stack exec yesod devel
```

и получаю ошибку:

```bash
ERROR: Yesod has been compiled with a different GHC version, please reinstall yesod-bin
yesod: ExitFailure 1
```

Так, с наскока не вышло. Я ведь проект-то перевёл уже на `ghc 7.10.2`, а `yesod-bin` был собран ещё старой версией `7.8.4`. Переустанавливаю:

```bash
$ stack build yesod-bin-1.4.13.3
```

Как вы поняли, версия `1.4.13.3` взята из `lts`-снимка `3.1`, который я начал использовать после перехода на `ghc 7.10.2`. Установка прошла без проблем.

## Новая трудность

Итак, продолжим:

```bash
$ stack exec -- yesod devel -p 3010
```

Да-да, запускать необходимо именно так, через два минуса. В статье Снойман об этом не упомянул, но я нашёл это [в этом коммите](https://github.com/yesodweb/yesod/commit/a7cccf2a7c5df8b26da9ea4fdcb6bac5ab3a3b75).

Ура, наконец-то начинается пересборка проекта - и вдруг:

```bash
Rebuilding application... (using cabal)
ghc: ghc no longer supports single-file style package databases (dist/package.conf.inplace) use 'ghc-pkg init' to create the database with the correct format.
Build failure, pausing...
```

Не совсем понятно, о чём речь, но гугление подсказало, что дело в версии `cabal`. Вот что сказал об этом один из хаскелистов:

```
That error only happens with versions prior to Cabal/cabal-install-1.22 (and GHC 7.10 requires version 1.22 or later),
```

Что ж, всё понятно, обновляем `cabal`. Для этого в файле `~/.stack/global/stack.yaml` меняем `resolver` с `2.18` на `3.1`, после чего:

```bash
$ stack install cabal-install-1.22.6.0
```

После установки проверяем:

```bash
$ cabal --version
cabal-install version 1.22.6.0
using version 1.22.4.0 of the Cabal library
```

То что надо. Вернувшись в каталог проекта, выполняем:

```bash
$ stack exec -- yesod devel -p 3010
Yesod devel server. Type 'quit' to quit
...
Starting development server...
Starting devel application
Devel application launched: http://localhost:3010
```

Победа! Кстати, обратите внимание на маленькое изменение: раньше для остановки сервера нужно было просто нажать `Enter`, теперь же необходимо напечатать `quit`.

Итак, теперь `stack` и `yesod` прекрасно уживаются друг с другом.
