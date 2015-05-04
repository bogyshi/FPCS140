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
import qualified Calculations as C
import Data.ByteString.Lazy
import System.IO.Unsafe

main = scotty 3000 $ do
    get "/style.css" $ file "style.css"
    get "/error.css" $ file "error.css"
    get "/pictures/weather.jpg" $ file "pictures/weather.jpg"
    get "/pictures/sunny.jpg" $ file "pictures/sunny.jpg"
    get "/" $ file "./index.html" 
   
    
    post ":/post" $ do
    	 zipcode <- param "zip"

  	 response <- liftIO (snd <$> curlGetString ("http://api.openweathermap.org/data/2.5/weather?zip="++zipcode++",us") [])
	 let l = response
	 --liftIO $ print l
  	 let a = decode $ (fromString l)
	 
	 liftIO $ print a

	 let result = (isJust (a))
	 let request = fromJust (a :: (Maybe W.Weather))
	 let coverage = (W.coverage (W.c (request)))
	 let windspeed = (W.speed (W.wind (request))) -- in m/s
	 let temp = ((W.temp $ W.wmain (request))*(9/5) -459.67) --Farenheit
	 let h = (W.humidity $ W.wmain (request))
	 let s = snd(C.forecast coverage windspeed temp h)
	 liftIO $ print result
	 --liftIO $ print request
	 --liftIO $ print coverage
	 --liftIO $ print windspeed 
	 --liftIO $ print temp
	 --liftIO $ print h

	 case result of
	    True -> html.renderText $ successPage coverage windspeed temp h s
	    False -> html.renderText $ errorPage
	    	 --let coverage = show (W.c (fromJust (a :: (Maybe W.Weather))))
	      	 --html . renderText $ 
                 --           html_ $ 
                 --                 body_ $ do
		 --		  	h1_ $ fromString coverage
	    --else liftIO $ html.get "/" $ file "./error.html"

	      	 


--successPage :: String -> Html
successPage c w t h s= html_ $
	      	         with body_ [style_ (fromString $ "background-image: url(\"" ++ (s::String) ++ "\");")] $ do
		    	   h1_ $ fromString(fst(C.forecast c w t h))
			   br_ []
			   p_ $ fromString (C.showTemp t)
			   br_ []
			   p_ $ fromString (C.showWind w)
			   br_ []
			   p_ $ fromString (C.showCover c)
			   br_ []
			   p_ $ fromString (C.showHumid h)

errorPage = html_ $ do
	    	 head_ $ do
		    --meta_ [charset_ "utf-8"]
	 	    link_ [href_ "error.css", rel_ "stylesheet", type_ "text/css"]
	    	 body_ $ do
		   -- [style_ "background-image = url("pictures/weather.jpg");"]
		     h1_ $ "whoops! we have an error"
		     br_ []
		     p_ $ do
		     	   "you most likely typed in the wrong zip code click " 
			   a_ [href_ "http://localhost:3000/"] "here"
			   " to try again"
			

-- make the pages take in a string with the file path for an image