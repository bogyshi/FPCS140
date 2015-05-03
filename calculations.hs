{-# LANGUAGE OverloadedStrings #-}

module Calculations where

forecast :: Int -> Double -> Double -> Int -> String
forecast coverage wind temp humid | hot (temp) = "It is currently " ++ (show (truncate temp)) ++ " degrees farenheight. Grab a fan and cool down."
	 	       	    	  | cold (temp) = "It is currently" ++ (show temp) ++ " degrees farenheight. Consider some heavy clothes."
				  | windy (wind) = "It is pretty windy outside at " ++ (show wind) ++ " m/s. Try a wind breaker"
				  | cloudy (coverage) = "The sky is " ++ (show (quot coverage 10)) ++ " clouds, itll be cooler than youd expect."
				  | otherwise = "pretty calm day"

hot :: Double -> Bool
hot t = if t >= 60 then True else False

cold :: Double -> Bool
cold t = if t <= 32 then True else False

windy :: Double -> Bool
windy w = if w >= 6.5 then True else False

cloudy :: Int -> Bool
cloudy c = if c >= 10 then True else False

showTemp :: Double -> String
showTemp t = "Current temperature: " ++ show (truncate t) ++ " degrees Farenheit"

showWind :: Double -> String
showWind w = "Current wind speed: " ++ show w ++ " meters per second"

showCover :: Int -> String 
showCover c = "Current cloud coverage: " ++ show (quot c 10) ++ "/10s of the sky are clouds"

showHumid :: Int -> String
showHumid h = "Current relative humidity: " ++ show h ++ "%"