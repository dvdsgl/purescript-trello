module Trello.Query where

import Prelude
import Data.List
import Data.Maybe (Maybe(..))
import Data.Foldable (any, find)
import Control.MonadPlus (guard)

import Trello.Types

cardLabeled :: String -> Card -> Boolean
cardLabeled label (Card c) = any (\(Label l) -> l.name == label) c.labels

cardNamed :: String -> Card -> Boolean
cardNamed name (Card c) = c.name == name

listForCard :: Board -> Card -> Maybe CardList
listForCard (Board b) (Card c) = find (\(CardList l) -> l.id == c.idList) b.lists

cardsWithList :: String -> Board -> List Card
cardsWithList name (Board board) = do
  CardList list <- board.lists
  guard $ list.name == name

  Card card <- board.cards
  guard $ card.idList == list.id

  return $ Card card
