import Data.List
import Data.Char
import Data.Maybe

stringFitsFormat :: String -> Bool
stringFitsFormat = isJust . go
  where go :: String -> Maybe String
  	go (x:xs) = do
  		xnum <- readMaybe [x] :: Maybe Int
  		res <- stripPrefix (replicate xnum 'a') xs
  		go res
  			 
  	{- str... >>= \_ -> go -}
  	--go "0" = Just ""
  	go "" = Just ""
  	go _ = Nothing