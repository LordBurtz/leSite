{-# LANGUAGE OverloadedStrings #-}

module Repository (
  Repository,
  initializeRepository,
  closeRepository,
  getPostById,
  insertPost,
  getAllPosts,
  getAllPublicPosts,
  setToPrivate
) where

import Structure (Post(..))
import Database.SQLite.Simple
import Data.Time.Clock
import qualified Data.Text as T
import GHC.Generics
import Data.Aeson.Key (toString)
import Data.Text (Text)

instance FromRow Post where
  fromRow = Post <$> field <*> field <*> field <*> field <*> field

instance ToRow Post where
  toRow (Post id title content timestamp link) = toRow (title, content, timestamp, link, 0 :: Int)

-- Define a Repository data type to encapsulate database operations
newtype Repository = Repository Connection

buildQuery :: Text -> Text -> Query
buildQuery = (Query .) . (<>)

selectPosts :: Text -> Query
selectPosts = buildQuery "SELECT id, title, content, timestamp, link FROM posts "

setPostPrivate :: Text -> Query
setPostPrivate = buildQuery "UPDATE posts SET private"

initializeRepository :: IO Repository
initializeRepository = do
  conn <- open "posts.db"
  execute_ conn "CREATE TABLE IF NOT EXISTS posts (id INTEGER PRIMARY KEY, title TEXT, content TEXT, timestamp TIMESTAMP, link TEXT, private INTEGER)"
  return $ Repository conn

closeRepository :: Repository -> IO ()
closeRepository (Repository conn) = close conn

getPostById :: Repository -> Int -> IO (Maybe Post)
getPostById (Repository conn) postId = do
  result <- query conn (selectPosts "WHERE id = ?") (Only postId)
  return $ case result of
    [post] -> Just post
    _      -> Nothing

insertPost :: Repository -> Post -> IO Int
insertPost (Repository conn) post = do
  execute conn "INSERT INTO posts (title, content, timestamp, link, private) VALUES (?, ?, ?, ?, ?)" post
  fromIntegral <$> lastInsertRowId conn

getAllPosts :: Repository -> IO [Post]
getAllPosts (Repository conn) = query_ conn $ selectPosts ""

getAllPublicPosts :: Repository -> IO [Post]
getAllPublicPosts (Repository conn) = query_ conn $ selectPosts "WHERE private = 0" 

setToPrivate :: Repository -> Int -> IO ()
setToPrivate (Repository conn) id = execute conn (setPostPrivate " = 1 WHERE id = ?") (Only id)

setToPublic :: Repository -> Int -> IO ()
setToPublic (Repository conn) id = execute conn (setPostPrivate "= 0 WHERE id = ?") (Only id)
