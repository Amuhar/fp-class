{-
  Для тестирования программ, работающих с файловой системой, часто необходимо
  достаточно большое дерево каталогов с файлами. Реализуйте случайный генератор
  такого дерева, управляемый набором параметров (минимальная и максимальная ширина
  и глубина дерева, количество и размеры файлов, что-нибудь ещё). В качестве идеи
  для архитектуры приложения используйте пример с подсчётом количества файлов в
  дереве (count.hs). Этот же пример можно использовать для тестирования
  разработанного приложения.
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import System.Environment
import System.Directory
import System.FilePath
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State
import Control.Applicative
import System.Random


data AppConfig = AppConfig {
      cfgMaxDepth :: Int,
      cfgMaxWidth :: Int,
      cfgMinWidth :: Int,
      cfgMaxFiles :: Int,
      cfgMaxFileSize :: Int
    } deriving (Show)

data AppState = AppState {
      stCurDepth :: Int,
      stCurPath :: FilePath
    } deriving (Show)



newtype MyApp a = MyA {
      runA :: ReaderT AppConfig (StateT AppState  IO) a
    } deriving (Functor, Applicative, Monad,
                MonadIO,
                MonadReader AppConfig,
                MonadState AppState)
    


genRandTree ::StdGen ->  MyApp ()
genRandTree gen = do
  config <- ask
  let [maxDepth,minWidth,maxWidth,maxNFiles, maxFSize] = [cfgMaxDepth config, cfgMinWidth config, cfgMaxWidth config, cfgMaxFiles config,cfgMaxFileSize config]
  st <- get
  let (randSF, gen') = randomR (1,maxFSize) gen :: (Int, StdGen) -- размер файла 
  c <- liftIO $ readFile "dandelion_wine.txt" 
  let content = take randSF c
  let (randNF, newGen) = randomR (1,maxNFiles) gen' :: (Int, StdGen) -- число файлов 
  let curDepth = stCurDepth st
  let path = stCurPath st
  forM_ [1..randNF] $ \ c -> liftIO$ do
    writeFile (path ++ "\\" ++ show c ++ ".txt") content
  when (curDepth < maxDepth) $ do
    let (randNumber, newGen') = randomR (minWidth,maxWidth) newGen :: (Int, StdGen) -- число каталогов 
    forM_ [1..randNumber] $ \name -> do
     let newPath = path </> ("C" ++ (show curDepth)++ (show name ))
     liftIO $ createDirectory newPath
     let newDepth = curDepth + 1
     put $ st {stCurDepth = newDepth, stCurPath = newPath}
     genRandTree newGen'

     
runMyApp :: MyApp a ->Int ->  Int -> Int -> Int -> Int -> FilePath -> IO a
runMyApp app maxDepth minWidth maxWidth maxNumbFiles maxFSize path =
    let config = AppConfig maxDepth maxWidth minWidth maxNumbFiles maxFSize
        state = AppState 1 path
      
    in (evalStateT (runReaderT (runA app ) config) state)        



main = do
    [minWidth,maxWidth, depth,numbFiles, fileSize] <- getArgs 
    -- создание первоначального каталога 
    let path = "C:\\Users\\Asus\\Documents\\GitHub\\fp-class\\class-12\\Start"
    createDirectory path
    gen <- newStdGen  
    runMyApp (genRandTree gen) (read depth) (read minWidth) (read maxWidth) (read numbFiles) (read fileSize) path
   
