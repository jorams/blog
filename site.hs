--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           Text.Pandoc.Options

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "fonts/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "style.css" $ do
        route   idRoute
        compile compressCssCompiler

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompilerWith defaultHakyllReaderOptions withToc
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx `mappend` bodyField "description"
            posts <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots "posts/*" "content"
            renderAtom feedConfiguration feedCtx posts

    match "templates/*" $ compile templateCompiler
  where
    withToc = defaultHakyllWriterOptions
        { writerTableOfContents = True
        , writerTemplate =
            "$if(toc)$<div id=\"TOC\"><h2>Table of Contents</h2>$toc$</div>\n$endif$$body$"
        , writerStandalone = True
        , writerReferenceLinks = False }



feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "Reinventing the future"
    , feedDescription = "Lisp, things, and more"
    , feedAuthorName  = "Joram Schrijver"
    , feedAuthorEmail = "hi@joram.io"
    , feedRoot        = "http://joram.io/"
    }

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%Y-%m-%d" `mappend`
    defaultContext
