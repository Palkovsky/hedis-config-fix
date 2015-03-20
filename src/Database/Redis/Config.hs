module Database.Redis.Config where

import Control.Applicative
import Data.Aeson
import Database.Redis


newtype RedisConfig =
    RedisConfig
    { rcConnectInfo :: ConnectInfo
    }

instance FromJSON RedisConfig where
    parseJSON v = RedisConfig <$> withObject "RedisConfig" go v
      where
        go o =
            ConnInfo
            <$> (o .:? "host" .!= (connectHost defaultConnectInfo ))
            <*> (o .:? "port" .!= (connectPort defaultConnectInfo ))
            <*> (o .:? "password" .!= (connectAuth defaultConnectInfo))
            <*> (o .:? "database" .!= (connectDatabase defaultConnectInfo))
            <*> (o .:? "max-connections" .!= (connectMaxConnections defaultConnectInfo))
            <*> (o .:? "max-idle-time" .!= (connectMaxIdleTime defaultConnectInfo))
