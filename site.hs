--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid (mappend)
import Control.Applicative
import System.FilePath
import Text.Pandoc.Options
import Hakyll

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
  match "static/**" $ do
    route $ customRoute stripTopDir
    compile copyFileCompiler

  -- Watch and compile LESS files
  match "assets/**.less" $ compile getResourceBody

  -- Tell Hakyll that style.css depends on all LESS files
  d <- makePatternDependency "assets/**.less"
  rulesExtraDependencies [d] $ create ["style.css"] $ do
    route idRoute
    compile $ loadBody "assets/style.less"
      >>= makeItem
      >>= withItemBody (unixFilter "lessc" [ "--include-path=assets"
                                           , "-x"
                                           , "-" ])

  match "posts/*" $ do
    route $ postRoute `composeRoutes` cleanRoute
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/post.html"    postCtx
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "templates/default.html" postCtx
      >>= relativizeUrls

  match ("pages/**" .||. "index.md") $ do
    route $ customRoute stripTopDir `composeRoutes` cleanRoute
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/page.html"    postCtx
      >>= loadAndApplyTemplate "templates/default.html" postCtx
      >>= relativizeUrls

  match "blog.html" $ do
    route $ customRoute (\x -> toFilePath "blog/index.html")
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
      posts <- fmap (take 10) . recentFirst =<<
               loadAllSnapshots "posts/*" "content"
      renderAtom feedConfiguration feedCtx posts

  match "templates/*" $ compile templateCompiler

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
  dateField "date" "%Y-%m-%d" `mappend`
  field "url" stripIndexLink `mappend`
  defaultContext

stripIndexLink :: (Item a -> Compiler String)
stripIndexLink =
  (fmap (maybe empty (dropFileName . toUrl)) . getRoute . itemIdentifier)

postRoute :: Routes
postRoute = customRoute $ combine "blog" . drop 17 . toFilePath

feedCtx :: Context String
feedCtx =
  postCtx `mappend` bodyField "description"

stripTopDir :: Identifier -> FilePath
stripTopDir = joinPath . tail . splitPath . toFilePath

cleanRoute :: Routes
cleanRoute = customRoute fileToDirectory

fileToDirectory :: Identifier -> FilePath
fileToDirectory = flip combine "index.html" . dropExtension . toFilePath

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
  { feedTitle       = "Reinventing the Future"
  , feedDescription = "Lisp, things, and more"
  , feedAuthorName  = "Joram Schrijver"
  , feedAuthorEmail = "hi@joram.io"
  , feedRoot        = "http://joram.io/" }
