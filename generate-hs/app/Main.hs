{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Web.Scotty
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.Aeson (FromJSON, ToJSON, object, (.=), decode, encode)
import Data.Time.Clock
import GHC.Generics

import Control.Monad.IO.Class (liftIO)
import Data.Maybe (Maybe)

-- Define a Post data type for database operations
data Post = Post { 
    id :: Int, 
    title :: String, 
    content :: String, 
    timestamp :: Maybe UTCTime 
  } deriving (Show, Generic)

instance FromRow Post
instance ToJSON Post
instance FromJSON Post

main :: IO ()
main = do
  -- Open a connection to the SQLite database
  conn <- open "posts.db"

  -- Create the 'posts' table if it doesn't exist
  execute_ conn "CREATE TABLE IF NOT EXISTS posts (id INTEGER PRIMARY KEY, title TEXT, content TEXT, timestamp TIMESTAMP)"

  -- Start the Scotty web server
  scotty 3000 $ 
    get "/" helloHaskellHandler >> 
    get "/:id" (postIdParamHandler conn) >> 
    post "/newpost"  (newPostHandler conn) >> 
    get "/all"  (getAllPostsHandler conn) 

  -- Close the database connection
  close conn

-- Route Handlers

helloHaskellHandler :: ActionM ()
helloHaskellHandler = text "Hello Haskell"

postIdParamHandler :: Connection -> ActionM ()
postIdParamHandler conn = do
  postId <- param "id"
  post <- liftIO $ getPostById conn postId
  json post

newPostHandler :: Connection -> ActionM ()
newPostHandler conn = do
  newPost <- jsonData :: ActionM Post
  timestamp <- liftIO getCurrentTime
  let postWithTimestamp = newPost { timestamp = Just timestamp }
  postId <- liftIO $ insertPost conn postWithTimestamp
  json $ object ["id" .= postId]

getAllPostsHandler :: Connection -> ActionM ()
getAllPostsHandler conn = do
  posts <- liftIO $ getAllPosts conn
  json posts

-- Database Operations

getPostById :: Connection -> Int -> IO (Maybe Post)
getPostById conn postId = do
  result <- query conn "SELECT * FROM posts WHERE id = ?" (Only postId) :: IO [Post]
  return $ case result of
    [post] -> Just post
    _      -> Nothing

insertPost :: Connection -> Post -> IO Int
insertPost conn post = do
  execute conn "INSERT INTO posts (title, content, timestamp) VALUES (?, ?, ?)" (title post, content post, timestamp post)
  fromIntegral <$> lastInsertRowId conn

getAllPosts :: Connection -> IO [Post]
getAllPosts conn = query_ conn "SELECT * FROM posts"
