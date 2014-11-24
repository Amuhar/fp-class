import Control.Monad.Reader
import System.Environment


multiplier :: (MonadReader b m, Num b) => b -> m b
multiplier a = do
    n <- ask
    return $ a * n
    
summand :: (MonadReader b m, Num b) => b -> m b
summand a = do
    n <- ask
    return $ a+n
	
divisor :: (MonadReader b m, Fractional b) => b -> m b    
divisor a = do
    n <- ask
    return $ n/a
	
loadConfig :: (Read r, Fractional r) => r -> String -> r
loadConfig n content =  foldl (\  acc  x -> runReader (parseInfo. words $ x) acc ) n (lines  content)

parseInfo :: (MonadReader b m, Read b, Fractional b) => [String] -> m b
parseInfo act = case head act of
					"multiplier" -> multiplier ( read . last $ act)
					"summand" -> summand ( read . last $ act)
					"divisor" -> divisor ( read . last $ act)
					

main =  getArgs >>= \ [x,y] -> readFile x >>= \c -> (map (\x -> read x  ) . lines) `fmap` readFile y >>=  mapM_ (\n -> print ( loadConfig n c))