{-# LANGUAGE QuasiQuotes #-}

module Markup.PostList (
    postList
) where

import           Data.String.QQ (s)
import           Text.Blaze.Html5 (Html, preEscapedToHtml)

import           Markup.Post (disqusCount)

postList :: Html
postList = preEscapedToHtml raw

raw :: String
raw = [s|
    <ul class="post-list">
        $for(posts)$
            <li>
                <div class="row">
                    <div class="col-lg-9 col-md-9 col-sm-12 col-xs-12">
                        <div class="post-title">
                            <span class="name-of-category">$postCategory$</span><a href="$url$">$title$</a>
                            <span class="fa fa-comments post-comments"></span><a href="$url$#disqus_thread" class="comments-link">К</a>
                        </div>
                    </div>

                    <div class="col-lg-3 col-md-3 col-sm-12 col-xs-12">
                        <div class="post-date">
                            $date$
                        </div>
                    </div>
                </div>
            </li>
        $endfor$
    </ul>
    |]
    ++ disqusCount
