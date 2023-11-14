{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Data.Aeson (FromJSON, ToJSON, object, (.=), decode, encode)
import Data.Time.Clock ( getCurrentTime )
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (Maybe)

-- owned
import Repository
import HtmlGen ( patchIndexWith )
import Structure ( Post(..))

instance ToJSON Post
instance FromJSON Post

main :: IO ()
main = do
  repository <- initializeRepository

  scotty 3000 $
    get "/" helloHaskellHandler >>
    get "/site-logo.png" (fileHandler "site-logo.png") >>
    get "/pylon.css" (fileHandler "pylon.css") >>
    get "/:id" (postIdParamHandler repository) >>
    post "/newpost" (newPostHandler repository) >>
    get "/all" (getAllPostsHandler repository) >>
    post "/regenerate" (regenerateHandler repository) >>
    delete "/post" (deletePostHandler repository)

  closeRepository repository


-- Route Handlers
helloHaskellHandler :: ActionM ()
helloHaskellHandler = file "index-u.html"

fileHandler :: FilePath -> ActionM ()
fileHandler = file

postIdParamHandler :: Repository -> ActionM ()
postIdParamHandler repo = do
  postId <- captureParam "id"
  post <- liftIO $ getPostById repo postId
  json post

newPostHandler :: Repository -> ActionM ()
newPostHandler repo = do
  newPost <- jsonData :: ActionM Post
  timestamp <- liftIO getCurrentTime
  let postWithTimestamp = newPost { timestamp = Just timestamp }
  postId <- liftIO $ insertPost repo postWithTimestamp
  json $ object ["id" .= postId]

getAllPostsHandler :: Repository -> ActionM ()
getAllPostsHandler repo = do
  posts <- liftIO $ getAllPosts repo
  json posts

regenerateHandler :: Repository -> ActionM ()
regenerateHandler repo = do
  posts <- liftIO $ getAllPublicPosts repo
  liftIO $ patchIndexWith posts
  file "index-u.html"

deletePostHandler :: Repository -> ActionM ()
deletePostHandler repo = do
  id <- queryParam "id"
  liftIO $ setToPrivate repo id
