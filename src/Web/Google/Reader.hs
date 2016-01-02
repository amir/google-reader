{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Google.Reader
  ( -- Functions
    ping
  , edit
  , token
  , userInfo
  , subscribe
  , bazQuxRoot
  , unsubscribe
  , clientLogin
  , subscriptionList
  , defaultEditSubscriptionOptions
  , streamId
  , authToken
  , addTo
  , removeFrom
  , streamTitle
    -- Types
  , StreamId
  , Tokens                  (..)
  , UserInfo                (..)
  , ReaderError             (..)
  , Subscriptions           (..)
  , ReaderOptions           (..)
  , EditSubscriptionOptions (..)
  ) where

import Data.List
import Data.Aeson
import Control.Lens 
import GHC.Generics
import Control.Exception
import Control.Applicative
import Data.Attoparsec.ByteString.Char8 as A hiding (try)

import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Network.URI as URI
import qualified Network.Wreq as Wreq
import qualified Data.ByteString.Char8 as C
import qualified Network.HTTP.Client as HTTP
import qualified Data.ByteString.Lazy as LBS (toStrict, ByteString, unpack)

type Token = String

data TokenType = SID
               | LSID
               | Auth
               deriving (Show, Eq, Ord)

data Tokens = Tokens {
      sid  :: String
    , lsid :: String
    , auth :: String
  } deriving (Show)

data ReaderOptions = ReaderOptions {
      root     :: URI.URI
    , email    :: String
    , password :: String
  } deriving (Eq, Show)

data UserInfo = UserInfo {
      userId              :: T.Text
    , userName            :: T.Text
    , userEmail           :: T.Text
    , userProfileId       :: T.Text
    , isBloggerUser       :: Bool
    , signupTimeSec       :: Int
    , isMultiLoginEnabled :: Bool
  } deriving (Generic, Show)

data Category = Category {
      categoryId :: T.Text
    , label      :: T.Text
  } deriving (Generic,Show)

data Subscription = Subscription {
      subscriptionId :: T.Text
    , title          :: T.Text
    , sortid         :: T.Text
    , htmlUrl        :: T.Text
    , categories     :: [Category]
    , firstitemmsec  :: T.Text
  } deriving (Generic, Show)

data Subscriptions = Subscriptions {
    subscriptions :: [Subscription]
  } deriving (Generic, Show)

data Endpoint = PingEndpoint
              | TokenEndpoint
              | UserInfoEndpoint
              | ClientLoginEndpoint
              | SubscriptionEditEndpoint
              | SubscriptionListEndpoint

type StreamId = T.Text

data EditSubscriptionOptions = EditSubscriptionOptions {
      _action      :: String
    , _streamId    :: StreamId
    , _authToken   :: T.Text
    , _addTo       :: Maybe [ StreamId ]
    , _removeFrom  :: Maybe [ StreamId ]
    , _streamTitle :: Maybe T.Text
  } deriving (Show, Eq)

makeLenses ''EditSubscriptionOptions

defaultEditSubscriptionOptions = EditSubscriptionOptions {
    _addTo       = Nothing
  , _action      = ""
  , _streamId    = ""
  , _authToken   = ""
  , _removeFrom  = Nothing
  , _streamTitle = Nothing
}

instance FromJSON Subscription where
  parseJSON (Object v) = Subscription         <$>
                         v .: "id"            <*>
                         v .: "title"         <*>
                         v .: "sortid"        <*>
                         v .: "htmlUrl"       <*>
                         v .: "categories"    <*>
                         v .: "firstitemmsec"
  parseJSON _          = empty

instance FromJSON Category where
  parseJSON (Object v) = Category     <$>
                         v .: "id"    <*>
                         v .: "label"
  parseJSON _          = empty

instance FromJSON UserInfo
instance FromJSON Subscriptions

bazQuxRoot = URI.URI {
    URI.uriScheme = "https:"
  , URI.uriAuthority = Just (URI.URIAuth {
      URI.uriUserInfo = ""
    , URI.uriRegName = "www.bazqux.com"
    , URI.uriPort = ":443"
    })
  , URI.uriPath = "/"
  , URI.uriQuery = ""
  , URI.uriFragment = ""
}

data BazQuxError = FreeTrialExpired
                 | YearSubscriptionExpired
                 deriving (Show)

data ReaderError = BadAuthentication
                 | Unauthorized
                 | ReaderHttpError HTTP.HttpException
                 | ReaderParseError String
                 | ReaderBazQuxError BazQuxError
                 deriving (Show)

getUri :: ReaderOptions -> Endpoint -> String
getUri readerOptions endpoint =
  URI.uriToString id ((root readerOptions) { URI.uriPath = path }) ""
  where
    path = case endpoint of
      PingEndpoint             -> "/reader/ping"
      TokenEndpoint            -> "/reader/api/0/token"
      UserInfoEndpoint         -> "/reader/api/0/user-info"
      ClientLoginEndpoint      -> "/accounts/ClientLogin"
      SubscriptionListEndpoint -> "/reader/api/0/subscription/list"
      SubscriptionEditEndpoint -> "/reader/api/0/subscription/edit"

getCause :: HTTP.HttpException -> ReaderError
getCause e = case e of
  (HTTP.StatusCodeException _ hs _) -> case bodyStart of
    Just bs -> case parse (A.takeWhile isAlpha_ascii <* char '=' *> errorParser) bs of
      Fail s _ _ -> ReaderParseError $ "Unable to parse " ++ C.unpack s
      Done _ i   -> i
    Nothing -> ReaderHttpError e
    where bodyStart = snd <$> find ((=="X-Response-Body-Start").fst) hs

  _                                 -> ReaderHttpError e

tryWreq :: IO (Wreq.Response LBS.ByteString) -> IO (Either ReaderError LBS.ByteString)
tryWreq a = do 
  res <- try a
  case res of
    Left  e -> return $ Left $ getCause e
    Right r -> return $ Right $ r ^. Wreq.responseBody

tokenTypeParser :: Parser TokenType
tokenTypeParser =
     (string "SID"  >> return SID)
 <|> (string "LSID" >> return LSID)
 <|> (string "Auth" >> return Auth)

errorParser :: Parser ReaderError
errorParser =
  (string "BadAuthentication" >> return BadAuthentication)

alphaNumericDotDash :: Char -> Bool
alphaNumericDotDash = inClass "-a-zA-Z0-9."

parseLoginResponseLine :: Parser (TokenType, String)
parseLoginResponseLine = (,) <$> (tokenTypeParser <* char '=') <*> (C.unpack <$> A.takeWhile alphaNumericDotDash)

parseLoginResponse :: Parser [(TokenType, String)]
parseLoginResponse = many $ parseLoginResponseLine <* endOfLine

toRecord :: [(TokenType, String)] -> Maybe Tokens
toRecord a = Tokens <$> Map.lookup SID m <*> Map.lookup LSID m <*> Map.lookup Auth m
          where m = Map.fromList a

addAuthHeader :: Wreq.Options -> String -> Wreq.Options
addAuthHeader o a = o & Wreq.header "Authorization" .~ ["GoogleLogin auth=" `C.append` C.pack (a)]

ping :: ReaderOptions -> Tokens -> IO (Either ReaderError LBS.ByteString)
ping readerOptions tokens =
  tryWreq(Wreq.getWith options uri)
  where
    options = addAuthHeader Wreq.defaults (auth tokens)
    uri = getUri readerOptions PingEndpoint

token :: ReaderOptions -> Tokens -> IO (Either ReaderError LBS.ByteString)
token readerOptions tokens =
  tryWreq(Wreq.getWith options uri)
  where
    options = addAuthHeader Wreq.defaults (auth tokens)
    uri = getUri readerOptions TokenEndpoint

userInfo :: ReaderOptions -> Tokens -> IO (Either ReaderError UserInfo)
userInfo readerOptions tokens = do
  q <- tryWreq(Wreq.getWith options uri)
  case q of
    Left  e -> return $ Left e
    Right r -> do
      case decode r of
        Just u  -> return $ Right u
        Nothing -> return $ Left $ ReaderParseError $ C.unpack $ LBS.toStrict r
  where
    options = addAuthHeader Wreq.defaults (auth tokens)
    uri = getUri readerOptions UserInfoEndpoint

clientLogin :: ReaderOptions -> IO (Either ReaderError Tokens)
clientLogin readerOptions = do
  res <- try(Wreq.postWith Wreq.defaults
                            (getUri readerOptions ClientLoginEndpoint)
                            ["Email" Wreq.:= (email readerOptions), "Passwd" Wreq.:= (password readerOptions)])
  case res of
    Left  e -> return $ Left $ getCause e
    Right r ->
      case toRecord $ r ^. (Wreq.responseBody . strict) . Wreq.atto_ parseLoginResponse of
        Just i  -> return $ Right i
        Nothing -> return $ Left BadAuthentication

subscriptionList :: ReaderOptions -> Tokens -> IO (Either ReaderError Subscriptions)
subscriptionList readerOptions tokens = do
  res <- tryWreq(Wreq.getWith options uri)
  case res of
    Left  e -> return $ Left e
    Right r -> do
      case decode r of
        Just u -> return $ Right u
        Nothing -> return $ Left $ ReaderParseError $ C.unpack $ LBS.toStrict r
  where
    options = (addAuthHeader Wreq.defaults (auth tokens)) & Wreq.param "output" .~ ["json"]
    uri = getUri readerOptions SubscriptionListEndpoint

subscriptionEdit :: ReaderOptions -> Tokens -> EditSubscriptionOptions -> IO (Either ReaderError String)
subscriptionEdit readerOptions tokens editSubscriptionOptions = do
  res <- tryWreq(Wreq.postWith options uri formParams)
  case res of
    Left  e -> return $ Left e
    Right r -> return $ Right $ C.unpack $ LBS.toStrict r
  where
    options = (addAuthHeader Wreq.defaults (auth tokens)) & Wreq.param "T" .~ [editSubscriptionOptions ^. authToken]
    formParams = [
                    "ac" Wreq.:= editSubscriptionOptions ^. action
                  , "s"  Wreq.:= editSubscriptionOptions ^. streamId
                 ]
    uri = getUri readerOptions SubscriptionEditEndpoint

subscribe readerOptions tokens editSubscriptionOptions =
  subscriptionEdit readerOptions tokens options
  where options = action .~ "subscribe" $ editSubscriptionOptions

unsubscribe readerOptions tokens editSubscriptionOptions =
  subscriptionEdit readerOptions tokens options
  where options = action .~ "unsubscribe" $ editSubscriptionOptions

edit readerOptions tokens editSubscriptionOptions =
  subscriptionEdit readerOptions tokens options
  where options = action .~ "edit" $ editSubscriptionOptions
