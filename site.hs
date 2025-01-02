--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
import           Data.Monoid (mappend)
import           Data.List (intercalate)
import           Data.List.Split (splitOn)
import           Hakyll


--------------------------------------------------------------------------------
config :: Configuration
config = defaultConfiguration
    { destinationDirectory = "docs"
    }

main :: IO ()
main = hakyllWith config $ do
    match ("images/*" .||. "css/et-book/**" .||. "favicon.ico" .||. "index.html") $ do
        route   idRoute
        compile copyFileCompiler

    match ("training.html" .||. "training-goal.html") $ do
        route   idRoute
        compile copyFileCompiler

    match "css/default.css" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["pt/contact.markdown", "pt/about.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "pt/templates/default.html" defaultContext
            >>= relativizeUrls

    match (fromList ["en/contact.markdown", "en/about.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "en/templates/default.html" defaultContext
            >>= relativizeUrls

    match "en/posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "en/templates/post.html"    postCtx
            >>= loadAndApplyTemplate "en/templates/default.html" postCtx
            >>= relativizeUrls

    match "pt/posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "pt/templates/post.html"    postCtx
            >>= loadAndApplyTemplate "pt/templates/default.html" postCtx
            >>= relativizeUrls

    create ["en/archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "en/posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "en/templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "en/templates/default.html" archiveCtx
                >>= relativizeUrls

    create ["pt/archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "pt/posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Arquivos"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "pt/templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "pt/templates/default.html" archiveCtx
                >>= relativizeUrls


    match "en/index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "en/posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "My Minimal Presence" `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "en/templates/default.html" indexCtx
                >>= relativizeUrls

    match "pt/index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "pt/posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Minha presença mínima" `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "pt/templates/default.html" indexCtx
                >>= relativizeUrls

    match "en/templates/*" $ compile templateCompiler
    match "pt/templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    field "enUrl" insertEnUrl `mappend`
    field "ptUrl" insertPtUrl `mappend`
    defaultContext

insertEnUrl :: Item String -> Compiler String
insertEnUrl item = do
  unContext defaultContext "url" [] item >>= \case
      StringField value -> do
        let splitted = splitOn "/" value
            modified = "/en" : drop 2 splitted
        return (intercalate "/" modified)
      _ -> return ""

insertPtUrl :: Item String -> Compiler String
insertPtUrl item = do
  unContext defaultContext "url" [] item >>= \case
      StringField value -> do
        let splitted = splitOn "/" value
            modified = "/pt" : drop 2 splitted
        return (intercalate "/" modified)
      _ -> return ""
