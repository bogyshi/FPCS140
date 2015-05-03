{-# LANGUAGE OverloadedStrings #-}

module WeatherStuff where

import Control.Monad
import Data.Aeson
import Data.Functor
import Data.Maybe
import Data.String
import Network.Curl
import Data.Text.Internal
import Data.Functor.Identity
import Data.Monoid
import Control.Applicative
import Data.String


data Weather = Weather
             { dt :: Integer
             , coord :: Coord
             , ws :: [W]
             , wmain :: Main
	     , wind :: Wind
	     , c :: Clouds
             } deriving (Show)

data Clouds = Clouds
    { coverage :: Int
    } 

data Wind = Wind
     {
       speed::Double
     } 

data Coord = Coord
           { lon :: Int
           , lat :: Int
           } deriving (Show)

data W = W
       { w_id :: Int
       , w_main :: String
       , w_description :: String
       } deriving (Show)

data Main = Main
    { temp :: Double
    , humidity :: Int
    --, tempMn :: Int
    --, tempMx :: Int
    } deriving (Show)


instance FromJSON Clouds where
    parseJSON (Object v) = Clouds
                           <$> v .: "all"

instance FromJSON Weather where
    parseJSON (Object v) = Weather
                           <$> v .: "dt"
                           <*> v .: "coord"
                           <*> v .: "weather"
                           <*> v .: "main"
			   <*> v .: "wind"
			   <*> v .: "clouds"

instance FromJSON Coord where
    parseJSON (Object v) = Coord
                           <$> v .: "lon"
                           <*> v .: "lat"

instance FromJSON Wind where
    parseJSON (Object v) = Wind
    	      	      	   <$> v .: "speed"

instance FromJSON W where
    parseJSON (Object v) = W
                           <$> v .: "id"
                           <*> v .: "main"
                           <*> v .: "description"

instance ToJSON W where
    toJSON (W i m d) = object ["id" .= i, "main" .= m, "description" .= d]

--to JSON is only if you need to according to the weather api, so not really necessary.

instance FromJSON Main where
    parseJSON (Object v) = Main
                           <$> v .: "temp"
			   <*> v .: "humidity"
			   -- <*> v .: "temp_min"
			   -- <*> v .: "temp_max"

instance Show Clouds where 
	 show (Clouds c) = (show (c::Int))

instance Show Wind where
	 show (Wind w) = (show (w::Double))

getCover :: Clouds -> Int
getCover (Clouds a) = a

compute :: Int -> String
compute 0 = "notta problemo"
compute a | a >= 80  = "tis shady"
	  | otherwise = "nvm"
--get rushils get and post request webstuff

main = do
  x <- getLine
  response <- snd <$> curlGetString ("http://api.openweathermap.org/data/2.5/weather?zip="++x++",us") []
  let w = decode (fromString response)
  print w
  let coverage =  (c (fromJust (w :: Maybe Weather)))
  print coverage
  mapM_ print . ws . fromJust $ (w :: Maybe Weather)
  print $ compute (getCover coverage)
  print $ fromJust $ (w :: Maybe Weather)


--decod is how you take a json to a data type 
--decode:: (fromJSON a) => String -> a
-- if you have fromJSON erson where
-- decode of bytestring into a datatype. 
--just go to aason to figure this stuff out.

-- there is ap roof language like idris, and its wrtiten in haskell, its agda, and its works similar, jsut for theorems and proofs. The most popular one is made by the the french, its call coq.

--etherpad for technical interviews