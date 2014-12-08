import Parser
import SimpleParsers
import Control.Applicative hiding (many, optional)
import Control.Monad

{-
   Определите тип данных, представляющий адрес URL следующего вида:

     <схема>://<логин>:<пароль>@<хост>:<порт>/<URL-путь>?<параметры>#<якорь>

   Реализуйте соответствующий парсер, считая, что обязательными компонентами
   URL являются только схема с символами "://" и имя хоста, все остальные
   компоненты могут отсутствовать.
-}


data Scheme = FTP | HTTP | HTTPS | Unk String
              deriving Show
type Server = String
type Path = String
data URL = URL Scheme Server Path
           deriving Show

        
type Domain = String           
type Login = String
type Password = String
type Port = String
type QueryString = String
type FragmentIdent = String     
data Adress = Adress Scheme Login Password  Domain Port Path QueryString FragmentIdent 
                                     deriving Show
                                                                    
scheme = (string "https" >> return HTTPS) <|>
         (string "http" >> return HTTP) <|>
         (string "ftp" >> return FTP) <|>
         Unk `liftM` lowers

url = URL <$>
      scheme <*>
      (string "://" >> many1 (sat (/='/'))) <*>
      many (sat $ const True)

login = do
   s <- many1 (sat (/= ':'))
   char ':'
   return s
      
password = do
   s <- many1 (sat (/= '@'))
   char '@'
   return s
domain = do
   s <- many1 (sat (/=':'))
   return s   
  
port = do
   s' <- string ":"
   s <- many1 (sat (/= '/'))
   char '/'
   return s
 
path = do
    s <- many1 (sat (/= '?'))
    char '?'
    return s
    
queryString = do
    s <- many1 (sat (/= '#' )) 
    char '#'
    return s
    
      
      
adress = Adress <$> scheme <*> (string "://" >>  optional "" login )<*> optional ""  password  <*> domain <*> optional "" port 
         <*> optional "" path <*> optional "" queryString <*> many (sat $ const True)
   