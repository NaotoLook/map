{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.Aeson
import qualified Data.ByteString.Lazy as B
import           Data.Text            (Text)
import           GHC.Generics
import           Network.HTTP.Conduit (simpleHttp)


data  Altitude = Altitude { elevation      :: Double
                                    , hsrc :: !Text
                                      } deriving (Show, Generic)

instance FromJSON  Altitude
instance ToJSON Altitude

getAltitude :: Double -> Double -> IO (Maybe Double)
getAltitude longitude latitude =
  fmap (fmap elevation . decode)  (simpleHttp  $ "http://cyberjapandata2.gsi.go.jp/general/dem/scripts/getelevation.php?lon=" ++ show longitude ++ "&lat=" ++ show latitude ++ "&outtype=JSON")

main :: IO ()
main = do
  let longitude = 140.08531 :: Double
  let latitude = 36.103543 :: Double
  ets <- getAltitude longitude latitude
  case ets of
    Nothing -> putStrLn "There was an error reading the JSON data."
    Just  r -> print r
