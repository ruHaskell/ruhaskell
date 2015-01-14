{-
    Модуль копировщиков.
    https://github.com/denisshevchenko/ruhaskell
    Все права принадлежат русскоязычному сообществу Haskell-разработчиков, 2015 г.
-}

{-# LANGUAGE OverloadedStrings #-}

module Copiers (
    justCopy,
    justCreateAndCopy,
    justCompressAndCopy
) where

import Hakyll

-- Берём нечто готовое и просто копируем в итоговый сайт.
justCopy :: Pattern -> Rules ()
justCopy something = match something $ do
    route   idRoute
    compile copyFileCompiler

-- Создаём нечто пустое и просто копируем в итоговый сайт.
justCreateAndCopy :: Identifier -> Rules ()
justCreateAndCopy something = create [something] $ do
    route   idRoute
    compile copyFileCompiler

-- Сжимаем нечто готовое и просто копируем в итоговый сайт.
justCompressAndCopy :: Pattern -> Rules ()
justCompressAndCopy something = match something $ do
    route   idRoute
    compile compressCssCompiler

