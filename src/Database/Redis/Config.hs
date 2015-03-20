module Database.Redis.Config
       ( RedisConfig(..)
       , connectRedis
       ) where

import Control.Applicative
import Data.Aeson
import Data.Scientific
import Data.Time
import Database.Redis

import qualified Data.Aeson.Types as A
import qualified Data.Text.Encoding as T


{- | Wrapper around 'ConnectInfo' to parse connection settings from
JSON-like values. All fields are optional, defaults are get from
'defaultConnectInfo'

Here is YAML example of config:

@
host: localhost                 # host name or address
port: 6379                      # you can specify either port
\# socket: \/run\/redis.socket     # or unix socket path
\# service: redis                # or service name
password: "pass"                # if not specified then no password used
database: 0                     # database number to connect to
max-connections: 5              # max 5 connections in pool
max-idle-time: 30               # keep connection open for 30 seconds
@

-}

newtype RedisConfig =
    RedisConfig
    { getConnectInfo :: ConnectInfo
    }


parsePort :: Object -> A.Parser (Maybe PortID)
parsePort o =
    optional
    $   (fmap (\a -> PortNumber $ floor (a :: Scientific)) (o .: "port"))
    <|> (fmap UnixSocket (o .: "socket"))
    <|> (fmap Service (o .: "service"))


instance FromJSON RedisConfig where
    parseJSON v = RedisConfig <$> withObject "RedisConfig" go v
      where
        go o =
            ConnInfo
            <$> (o .:? "host" .!= connectHost defaultConnectInfo)
            <*> (parsePort o .!= connectPort defaultConnectInfo)
            <*> (fmap (fmap T.encodeUtf8) (o .:? "password"))
            <*> (o .:? "database" .!= (connectDatabase defaultConnectInfo))
            <*> (o .:? "max-connections" .!= (connectMaxConnections defaultConnectInfo))
            <*> (fmap (fmap (realToFrac :: Scientific -> NominalDiffTime)) (o .:? "max-idle-time") .!= (connectMaxIdleTime defaultConnectInfo))


-- | Open redis connection
connectRedis :: RedisConfig -> IO Connection
connectRedis = connect . getConnectInfo
