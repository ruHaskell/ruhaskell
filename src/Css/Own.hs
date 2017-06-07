{-# LANGUAGE OverloadedStrings #-}

module Css.Own (
    ownCss
) where

import           Clay
import           Data.Text                      ( Text )
import qualified Data.Text.Lazy                 as Lazy

-- | Наш собственный CSS, встраивается в <head>-секцию всех страниц.
-- Внимание: используйте 'renderWith compact' осторожно, она глючная.
ownCss :: Text
ownCss = Lazy.toStrict . render $ do
    let centerAlign   = textAlign . alignSide $ sideCenter
        leftAlign     = textAlign . alignSide $ sideLeft
        rightAlign    = textAlign . alignSide $ sideRight

        paddingTopPx    = paddingTop . px
        paddingBottomPx = paddingBottom . px
        paddingLeftPx   = paddingLeft . px
        paddingRightPx  = paddingRight . px

        marginTopPx     = marginTop . px
        marginBottomPx  = marginBottom . px
        marginRightPx   = marginRight . px
        marginAuto      = margin (px 0) auto (px 0) auto

        fontSizePx      = fontSize . px
        fontSizePct     = fontSize . pct
        fontWeight600   = fontWeight $ weight 600

        simpleLinks     = do
            a # hover   ? do
                "color" -: "black !important"
            a # link    ? do
                "color" -: "black !important"
            a # visited ? do
                "color" -: "black !important"
            a # active  ? do
                "color" -: "black !important"
    
    ---------------------------------------------

    importUrl "https://fonts.googleapis.com/css?family=Roboto+Condensed:400,400i,700|Roboto+Mono:400,400i,700&subset=cyrillic"

    body ? do
        fontFamily      ["Roboto Condensed"] [sansSerif]
        fontSizePx      18
        backgroundColor "#f6f6f6"
        backgroundImage $ url "/static/images/satinweave.png"

    simpleLinks

    ".footer" ? do
        marginAuto
        maxWidth        $ pct 86
        paddingTopPx    70
        paddingBottomPx 50
        color           "#777"
        fontSizePct     94

    ".left"   ? leftAlign
    ".right"  ? rightAlign
    ".center" ? centerAlign

    h1 ? do
        centerAlign
        fontSizePct     200
        paddingTopPx    30
        paddingBottomPx 30
    
    h2 ? do
        fontSizePct     160
        paddingTopPx    30
        paddingBottomPx 25
    
    h3 ? do
        fontSizePct     140
        paddingTopPx    25
        paddingBottomPx 20

    a ? do
        color           "#2d3644"
        textDecoration  none
        borderBottom    dotted (px 1) "#333333"

    a # hover ? do
        color           "#5493ff"
        textDecoration  none
        borderBottom    solid (px 1) "#999999"

    a # visited ? do
        color           "#2d3644"
        textDecoration  none
        borderBottom    dotted (px 1) "#333333"

    a # active ? do
        color           "#2d3644"
        textDecoration  none
        borderBottom    dotted (px 1) "#333333"
    
    ".href-to-original" ? do
        rightAlign
        fontSizePct     90
        paddingTopPx    10

    "#authors-link" ? do
        textDecoration  none
        borderBottom    solid (px 0) "#ffffff"
    
    "#about-link" ? do
        textDecoration  none
        borderBottom    solid (px 0) "#ffffff"

    "#tags-link" ? do
        textDecoration  none
        borderBottom    solid (px 0) "#ffffff"

    "#categories-link" ? do
        textDecoration  none
        borderBottom    solid (px 0) "#ffffff"
   
    "#links-link" ? do
        textDecoration  none
        borderBottom    solid (px 0) "#ffffff"

    "#sl-1" ? do
        textDecoration  none
        borderBottom    solid (px 0) "#ffffff"

    "#sl-2" ? do
        textDecoration  none
        borderBottom    solid (px 0) "#ffffff"
    
    "#sl-3" ? do
        textDecoration  none
        borderBottom    solid (px 0) "#ffffff"
    
    "#sl-4" ? do
        textDecoration  none
        borderBottom    solid (px 0) "#ffffff"

    "#sl-5" ? do
        textDecoration  none
        borderBottom    solid (px 0) "#ffffff"
    
    "#go-home" ? do
        textDecoration  none
        borderBottom    solid (px 0) "#ffffff"

    "#hakyll-link" ? do
        textDecoration  none
        borderBottom    solid (px 0) "#ffffff"

    "#mit-link" ? do
        textDecoration  none
        borderBottom    solid (px 0) "#ffffff"
    
    ".fpconf-link" ? do
        fontSizePct     120
    
    ".fby-link" ? do
        fontSizePct     120
    
    ".friends-separator" ? do
        paddingLeftPx   30

    ".navigation" ? do
        maxWidth        $ pct 86
        marginAuto
        paddingTopPx    40
        paddingBottomPx 40
        fontSizePct     120
    
    ".links" ? do
        marginTopPx     17
        fontSizePct     90
        paddingBottomPx 10

    ".logo-area" ? do
        centerAlign
    
    ".social-links" ? do
        rightAlign
        marginTopPx     10
        fontSizePct     140

    ".social-links-separator" ? do
        paddingRightPx  20

    ".links-separator" ? do
        paddingRightPx  20
    
    ".reddit-color" ? do
        color           "#FF4500"

    ".twitter-color" ? do
        color           "#1DA1F2"

    ".gitter-color" ? do
        color           "#E10454"

    ".github-color" ? do
        color           "#000"

    ".rss-color" ? do
        color           "#FF924A"

    ".name-of-category" ? do
        fontSizePct     85
        border          solid (px 1) "#ffbcb1"
        borderRadius    (px 3) (px 3) (px 3) (px 3)
        backgroundColor "#fec1b7"
        paddingTopPx    3
        paddingBottomPx 3
        paddingLeftPx   5
        paddingRightPx  5
        marginRightPx   22

    ".post-info" ? do
        color           "#777"
        fontSizePct     90
        fontFamily      ["Roboto Mono"] [monospace]
        leftAlign
        paddingTopPx    10
        paddingBottomPx 60

    ".post-list" ? do
        padding         (px 0) (px 0) (px 0) (px 0)
        margin          (px 0) (px 0) (px 0) (px 0)
        listStyleType   none
        fontSizePct     110

    ".post-list" ? li ? do
        marginTopPx     21
        paddingBottomPx 21

    ".post-list" ? a ? do
        textDecoration  none

    ".post-list" ? a # hover ? do
        textDecoration  none

    ".post-comments" ? do
        paddingLeftPx   15
        paddingRightPx  5
        fontSizePct     90
        color           "#777"

    ".comments-link" ? do
        fontSizePct     90
        color           "#777"

    ".post-date" ? do
        rightAlign
        color           "#888888"

    ".archive-button" ? do
        paddingTopPx    26
    
    ".undecorated" ? do
        textDecoration  none
        borderBottom    solid (px 0) "#ffffff"

    "img[src*='#center']" ? do
        marginAuto
        display         block
        maxWidth        $ pct 100

    ".social-buttons-separator" ? do
        paddingTopPx    40

    ".heart-color" ? do
        color           "#FF3A1D"

    ".tags-cloud" ? do
        centerAlign
        paddingTopPx    30
        paddingBottomPx 30
        marginAuto
        maxWidth        $ pct 60

    ".tag-default" ? do
        backgroundColor "#fec1b7"
        border          solid (px 1) "#ffbcb1"
        color           inherit

    ".sourceCode" ? code ? do
        fontFamily      ["Roboto Mono"] [monospace]

    "pre.sourceCode" ? do
        fontFamily      ["Roboto Mono"] [monospace]
        fontSizePct     94
        border          solid (px 1) "#fbe7d8"
        borderRadius    (px 3) (px 3) (px 3) (px 3)
        backgroundColor "#fdf6e3"
        color           "#073642"
        paddingTopPx    10
        paddingBottomPx 10
        paddingLeftPx   15
        paddingRightPx  15
        marginTopPx     20
        marginBottomPx  20

    "#content" ? p |> code ? do
        fontFamily      ["Roboto Mono"] [monospace]
        fontSizePct     94
        borderRadius    (px 3) (px 3) (px 3) (px 3)
        border          solid (px 1) "#fbe7d8"
        backgroundColor "#fdf6e3"
        color           "#000"
        paddingTopPx    1
        paddingBottomPx 1
        paddingLeftPx   4
        paddingRightPx  4

    "pre.sourceCode" ? "span.kw" ? do color "#007020"; fontWeight600
    "pre.sourceCode" ? "span.dt" ? do color "#902000"
    "pre.sourceCode" ? "span.dv" ? do color "#40a070"
    "pre.sourceCode" ? "span.bn" ? do color "#40a070"
    "pre.sourceCode" ? "span.fl" ? do color "#40a070"
    "pre.sourceCode" ? "span.ch" ? do color "#4070a0"
    "pre.sourceCode" ? "span.co" ? do color "#60a0b0"
    "pre.sourceCode" ? "span.ot" ? do color "#007020"
    "pre.sourceCode" ? "span.al" ? do color "#fa0202"; fontWeight600
    "pre.sourceCode" ? "span.fu" ? do color "#06287e"
    "pre.sourceCode" ? "span.er" ? do color "#fa0202"; fontWeight600
