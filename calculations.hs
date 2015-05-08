{-# LANGUAGE OverloadedStrings #-}

module Calculations where

forecast :: Int -> Double -> Double -> Int -> String -> (String,String)
forecast coverage wind temp humid descrip | rain (descrip) = ("It is currenly raining; grab a rain jacket, umbrella, or stay inside!","pictures/rain.jpg")
	 	       	    	  	  | hot (temp) = ("It is currently " ++ (show (truncate temp)) ++ " degrees farenheight. Grab a fan and cool down.", "pictures/sunny.jpg")
	 	       	    	  	  | cold (temp) = ("It is currently " ++ (show(truncate temp)) ++ " degrees farenheight. Consider some heavy clothes.","pictures/cold.jpg")
				  	  | windy (wind) = ("It is pretty windy outside at " ++ (show wind) ++ " m/s. Try a wind breaker","pictures/windy.jpg")
				  	  | cloudy (coverage) = ("The sky is " ++ (show (coverage)) ++ "% clouds, itll be cooler than youd expect.","pictures/cloudy.jpg")
				  	  | otherwise = ("pretty calm day, a good day to lay in the grass and contemplate life","pictures/calm.jpg")

hot :: Double -> Bool
hot t = if t >= 75 then True else False

rain :: String -> Bool
rain d = if d == "Rain" then True else False

cold :: Double -> Bool
cold t = if t <= 32 then True else False

windy :: Double -> Bool
windy w = if w >= 6 then True else False

cloudy :: Int -> Bool
cloudy c = if c >= 40 then True else False

showTemp :: Double -> String
showTemp t = "Current temperature: " ++ show (truncate t) ++ " degrees Farenheit"

showWind :: Double -> String
showWind w = "Current wind speed: " ++ show w ++ " meters per second"

showCover :: Int -> String 
showCover c = "Current cloud coverage: " ++ show (quot c 10) ++ "/10ths of the sky are clouds"

showHumid :: Int -> String
showHumid h = "Current relative humidity: " ++ show h ++ "%"

colorText :: Double -> String -> String
colorText t d | ((cold t) || (rain d)) = "background-color:rgba(192,192,192,0.4)"
	      | otherwise = ""

supaHot :: Double -> String -> Int -> String
supaHot t d c | ((rain d) || (not (hot(t))) || ((cloudy c))) = ""
	      | otherwise = "WARNING: Its a hot day! stay hydrated, and put some Sunscreen on!"