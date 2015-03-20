module Database.Redis.Config where

import Control.Applicative
import Data.Aeson
import Data.Scientific
import Data.Time
import Database.Redis

import qualified Data.Aeson.Types as A
import qualified Data.Text as T
import qualified Data.Text.Encoding as T


-- | Wrapper around 'ConnectInfo' to parse connection settings from
-- JSON-like values.
newtype RedisConfig =
    RedisConfig
    { getConnectInfo :: ConnectInfo
    }

newtype RedisPort =
    RedisPort
    { getPortID :: PortID
    }

instance FromJSON RedisPort where
    parseJSON (A.String t) =
        return $ RedisPort $ UnixSocket $ T.unpack t
    parseJSON (A.Number n) =
        return $ RedisPort $ PortNumber $ floor n
    parseJSON _ = fail "RedisPort must be either string or number"

instance FromJSON RedisConfig where
    parseJSON v = RedisConfig <$> withObject "RedisConfig" go v
      where
        go o =
            ConnInfo
            <$> (o .:? "host" .!= (connectHost defaultConnectInfo ))
            <*> (fmap (fmap getPortID) (o .:? "port") .!= (connectPort defaultConnectInfo ))
            <*> (fmap (fmap T.encodeUtf8) (o .:? "password"))
            <*> (o .:? "database" .!= (connectDatabase defaultConnectInfo))
            <*> (o .:? "max-connections" .!= (connectMaxConnections defaultConnectInfo))
            <*> (fmap (fmap (realToFrac :: Scientific -> NominalDiffTime)) (o .:? "max-idle-time") .!= (connectMaxIdleTime defaultConnectInfo))
