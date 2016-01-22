module Trello.Options where

import Prelude

import Data.Foreign (Foreign())
import Data.Maybe (Maybe(..))
import Data.Options (Option(), IsOption, optionFn, options, opt, (:=))

import Trello.Types

data State a = All | Closed | None | Open | Visible

instance showState :: Show (State a) where
  show All = "all"
  show Closed = "closed"
  show None = "none"
  show Open = "open"
  show Visible = "visible"

instance optionState :: IsOption (State a) where
  assoc k state = (optionFn k) := show state

cards :: Option BoardOptions (State Card)
cards = opt "cards"

lists :: Option BoardOptions (State CardList)
lists = opt "lists"

cardAttachments :: Option BoardOptions Boolean
cardAttachments = opt "card_attachments"

boards :: Option OrganizationOptions String
boards = opt "boards"
