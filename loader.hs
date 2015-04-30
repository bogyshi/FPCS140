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
    get "/pictures/weather.jpg" $ file "pictures/weather.jpg"
    get "/" $ file "./index.html" 
   
    
    post ":/post" $ do
    	 zipcode <- param "zip"

  	 response <- liftIO (snd <$> curlGetString ("http://api.openweathermap.org/data/2.5/weather?zip="++zipcode++",us") [])
	 let l = response
	 --liftIO $ print l
  	 let a = decode $ (fromString l)
	 liftIO $ print a

	 let result = (isJust (a))
	 liftIO $ print result

	 case result of
	    True -> html.renderText $ successPage (show (W.c (fromJust (a :: (Maybe W.Weather)))))
	    False -> html.renderText $ errorPage
	    	 --let coverage = show (W.c (fromJust (a :: (Maybe W.Weather))))
	      	 --html . renderText $ 
                 --           html_ $ 
                 --                 body_ $ do
		 --		  	h1_ $ fromString coverage
	    --else liftIO $ html.get "/" $ file "./error.html"

	      	 


--successPage :: String -> Html
successPage s = html_ $
	      	    body_ $ do
		    	  h1_ $ fromString s

errorPage = html_ $
	    	  body_ $ do
		  	h1_ $ "whoops! we have an error"
			br_ []
			p_ $ do
			    "you most likely typed " 
			    a_ [href_ "http://localhost:3000/"] "here"
			    " in the wrong zipcode"
			

-- make the pages take in a string with the file path for an image