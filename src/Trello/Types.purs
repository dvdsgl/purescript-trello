module Trello.Types where

import Prelude

import Data.Maybe
import Data.List
import Data.Monoid
import Data.Foldable (any, find)

import Data.Foreign
import Data.Foreign.Null (runNull)
import Data.Foreign.NullOrUndefined (runNullOrUndefined)
import Data.Foreign.Class

type Id = String
type Url = String
type Name = String
type Credentials = { key :: String, token :: String }

newtype Markdown = Markdown String

newtype BoardInfo = BoardInfo
  { id :: Id
  , name :: String
  , desc :: String
  }

newtype Organization = Organization
  { id :: Id
  , name :: String
  , desc :: String
  , boards :: List BoardInfo
  }

newtype Preview = Preview
  { width :: Int
  , height :: Int
  , url :: String
  }

newtype Attachment = Attachment
  { id :: Id
  , name :: String
  , isUpload :: Boolean
  , url :: String
  , previews :: List Preview
  }

newtype Label = Label
  { name :: String
  }

newtype CardList = CardList
  { id :: Id
  , name :: String
  , closed :: Boolean
  }

newtype Card = Card
  { id :: Id
  , name :: String
  , desc :: Markdown
  , closed :: Boolean
  , url :: String
  , idList :: String
  , labels :: List Label
  , cover :: Maybe Attachment
  , attachments :: List Attachment
  }

newtype Board = Board
  { id :: Id
  , name :: String
  , desc :: String
  , cards :: List Card
  , lists :: List CardList
  , raw :: Foreign
  , url :: String
  }

boardInfo :: Board -> BoardInfo
boardInfo (Board b) = BoardInfo { id: b.id, name: b.name, desc: b.desc }

foreign import data OrganizationOptions :: *
foreign import data BoardOptions :: *

instance showBoard :: Show Board where
  show board = "Board " <> Util.stringify board

readNullOrUndefinedArray :: forall a. (IsForeign a) => String -> Foreign -> F (Array a)
readNullOrUndefinedArray name object = fromMaybe [] <$> runNullOrUndefined <$> readProp name object

instance boardInfoIsForeign :: IsForeign BoardInfo where
  read object = do
    id <- readProp "id" object
    name <- readProp "name" object
    desc <- readProp "desc" object
    return $ BoardInfo
      { id: id
      , name: name
      , desc: desc
      }

instance organizationIsForeign :: IsForeign Organization where
  read object = do
    id <- readProp "id" object
    name <- readProp "name" object
    desc <- readProp "desc" object
    boards <- readProp "boards" object
    return $ Organization
      { id: id
      , name: name
      , desc: desc
      , boards: toList (boards `asTypeOf` [])
      }

instance boardIsForeign :: IsForeign Board where
  read object = do
    id <- readProp "id" object
    name <- readProp "name" object
    desc <- readProp "desc" object
    url <- readProp "url" object
    cards <- readNullOrUndefinedArray "cards" object
    lists <- readNullOrUndefinedArray "lists" object
    return $ Board
      { id: id
      , name: name
      , desc: desc
      , cards: toList cards
      , lists: toList lists
      , raw: object
      , url: url
      }

instance cardListIsForeign :: IsForeign CardList where
  read object = do
    id <- readProp "id" object
    name <- readProp "name" object
    closed <- readProp "closed" object
    return $ CardList
      { id: id
      , name: name
      , closed: closed
      }

instance previewIsForeign :: IsForeign Preview where
  read object = do
    width <- readProp "width" object
    height <- readProp "height" object
    url <- readProp "url" object
    return $ Preview
      { width: width
      , height: height
      , url: url
      }

instance attachmentIsForeign :: IsForeign Attachment where
  read object = do
    id <- readProp "id" object
    name <- readProp "name" object
    isUpload <- readProp "isUpload" object
    url <- readProp "url" object
    previews <- readProp "previews" object
    return $ Attachment
      { id: id
      , name: name
      , isUpload: isUpload
      , url: url
      , previews: toList (previews `asTypeOf` [])
      }

instance labelIsForeign :: IsForeign Label where
  read object = do
    name <- readProp "name" object
    return $ Label
      { name: name
      }

instance cardIsForeign :: IsForeign Card where
  read object = do
    id <- readProp "id" object
    name <- readProp "name" object
    desc <- readProp "desc" object
    closed <- readProp "closed" object
    shortUrl <- readProp "shortUrl" object
    idList <- readProp "idList" object
    labels <- readProp "labels" object
    attachments <- readNullOrUndefinedArray "attachments" object

    idAttachmentCover <- runNull <$> readProp "idAttachmentCover" object

    return $ Card
      { id: id
      , name: name
      , desc: Markdown desc
      , closed: closed
      , url: shortUrl
      , idList: idList
      , labels: toList (labels `asTypeOf` [])
      , cover: find (\(Attachment a) -> Just a.id == idAttachmentCover) attachments
      , attachments: toList (attachments `asTypeOf` [])
      }

instance mardownEq :: Eq Markdown where
  eq (Markdown x) (Markdown y) = x == y

instance mardownSemigroup :: Semigroup Markdown where
  append (Markdown x) (Markdown y) = Markdown (x <> "\n" <> y)

instance mardownMonoid :: Monoid Markdown where
  mempty = Markdown mempty
