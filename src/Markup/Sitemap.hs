{-# LANGUAGE QuasiQuotes #-}

module Markup.Sitemap (
    sitemapTemplate
) where

import           Control.Monad ((<=<))
import           Data.String.QQ
import           Hakyll (Compiler, makeItem)
import           Hakyll.Web.Template

sitemapTemplate :: Compiler Template
sitemapTemplate = compileTemplateItem <=< makeItem $ raw

raw :: String
raw = [s|<?xml version="1.0" encoding="UTF-8"?>
    <urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">
    $for(entries)$
        <url>
            <loc>$host$$url$</loc>
        </url>
    $endfor$
    </urlset>
    |]
