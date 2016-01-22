module Trello
  ( Trello()
  , Client()
  , runTrello
  , getBoard
  , getOrganization
  ) where

import Prelude
import Data.Maybe
import Data.Either
import Data.Function
import Data.Options (Options(), options)

import Control.Monad.Eff
import Control.Monad.Trans (lift)
import Control.Monad.Eff.Exception (Error(), error)
import Control.Monad.Error.Class (throwError)

import Control.Monad.Reader
import Control.Monad.Reader.Class
import Control.Monad.Reader.Trans

import Control.Monad.Aff

import Trello.Types

import Data.Foreign
import Data.Foreign.Class

-- TODO make Trello newtype so we don't have to be so general
type Trello a = forall e. ReaderT Client (Aff e) a

foreign import data Client :: *

foreign import _client :: Credentials -> Client
foreign import _get :: forall e a. Client -> String -> Foreign -> (Error -> Eff e Unit) -> (a -> Eff e Unit) -> Eff e Unit

getAff :: Client -> String -> Foreign -> PureAff Foreign
getAff client path opts = makeAff (_get client path opts)

get :: forall a r. (IsForeign a) => String -> Foreign -> Trello a
get path opts = do
  client <- ask
  object <- lift $ getAff client path opts
  readM object

-- Read foreign values in the Trello context, failing if the read fails
readM :: forall a. (IsForeign a) => Foreign -> Trello a
readM object = lift do
  case readWith (error <<< show) object of
    Left msg -> throwError msg
    Right x -> return x

getBoard :: Id -> Options BoardOptions -> Trello Board
getBoard id' opts = get ("/1/board/" ++ id') (options opts)

getOrganization :: String -> Options OrganizationOptions -> Trello Organization
getOrganization name opts = get ("/1/organizations/" <> name) (options opts)

runTrelloT :: forall e a. Trello a -> Credentials -> Aff e a
runTrelloT trello credentials = runReaderT trello $ _client credentials

runTrello' :: forall e a. Trello a -> Credentials -> (Error -> Eff e Unit) -> (a -> Eff e Unit) -> Eff e Unit
runTrello' trello credentials err success = runAff err success $ runTrelloT trello credentials

runTrello :: forall e a. Trello a -> Credentials -> (Either Error a -> Eff e Unit) -> Eff e Unit
runTrello trello credentials handle = runTrello' trello credentials (handle <<< Left) (handle <<< Right)
