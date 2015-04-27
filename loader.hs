{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Types
import Web.Scotty
import Data.Monoid
import Data.Aeson (object, (.=))
import Data.Aeson
import Control.Monad.IO.Class
import Data.Text.Lazy as L
import Data.Text as S
import Lucid
import Lucid.Base
import Data.Text.Internal
import Data.Functor
import Data.Functor.Identity
import Data.Monoid
import Control.Applicative
import Data.String
import Control.Monad
import Data.Maybe
import Network.Curl
import qualified WeatherStuff as W
import Data.ByteString.Lazy
import System.IO.Unsafe

main = scotty 3000 $ do
    get "/style.css" $ file "style.css"
    get "/" $ file "./index.html" 
   
    
    post ":/post" $ do
    	 zipcode <- param "zip"

  	 response' <- liftIO (snd <$> curlGetString ("http://api.openweathermap.org/data/2.5/weather?zip="++zipcode++",us") [])
	 --response <- response'
	 let l = response'
	 --let l = further $ calculate zipcode
  	 let w = decode $ (fromString l)
  	 let coverage =  show (W.c (fromJust (w :: Maybe W.Weather)))

	 html . renderText $ 
                            html_ $ 
                                  body_ $ do
				  	h1_ $ fromString coverage

calculate :: String -> (IO String)
calculate s = do response <- (snd <$> curlGetString ("http://api.openweathermap.org/data/2.5/weather?zip="++s++",us") [])
	      	 return response

further :: (IO String) -> String
further a = unsafePerformIO a
	      	 



   -- get "/:word" $ do
--	text <- liftIO $ readFile("test.html")
      --  html $ L.pack text
--the purpose of this code is to see if I can load an html file

