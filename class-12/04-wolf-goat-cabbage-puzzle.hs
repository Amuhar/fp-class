{-
  Напишите программу, решающую следующую задачу методом полного перебора:

     «Крестьянину нужно перевезти через реку волка, козу и капусту. Но лодка такова,
     что в ней может поместиться только крестьянин,  а с ним или один волк, или одна
     коза, или одна капуста.  Но если оставить волка с козой,  то волк съест козу, а
     если оставить  козу с капустой,  то коза  съест капусту.  Как перевёз свой груз
     крестьянин?»

  В качестве идеи для реализации используйте решение задачи о калотанской семье
  (kalotans-puzzle.hs). 
-}
import Control.Monad
import Control.Monad.State
import Data.Maybe


type Var   = String
type Value = String
data Predicate = Is    Var Value           
               | Equal Var Var              
           | And   Predicate Predicate  
           | Or    Predicate Predicate  
           | Not   Predicate            
  deriving (Eq, Show)
 
type Variables = [(Var,Value)]

data ProblemState = PS {vars::Variables,  constraints::[Predicate]}

type NDS a = StateT ProblemState [ ] a


type Plan = [String]

isNot :: Var -> Value -> Predicate
isNot var value = Not (Is var value)
 
implies :: Predicate -> Predicate -> Predicate
implies a b = Not (a `And` (Not b))
 
orElse :: Predicate -> Predicate -> Predicate
orElse a b = (a `And` (Not b)) `Or` ((Not a) `And` b)

check :: Predicate -> Variables -> Maybe Bool
check (Is var value) vars = liftM (==value) (lookup var vars)
check (Equal v1 "farmer") vars = liftM2 (==) (lookup v1 vars) (Just $ head . words . fromJust . lookup "farmer" $ vars)
check (Equal v1 v2) vars = liftM2 (==) (lookup v1 vars) (lookup v2  vars)
check (And p1 p2) vars = liftM2 (&&) (check p1 vars) (check p2 vars)
check (Or  p1 p2) vars = liftM2 (||) (check p1 vars) (check p2 vars)
check (Not p) vars = liftM not (check p vars)


setVar :: Var -> Value -> NDS ()
setVar v x = do 
  st <- get
  vs' <- return $ filter ((v/=).fst) (vars st)
  put $ st {vars=(v,x):vs'}

getVar :: Var -> NDS (Maybe Value)
getVar v = do 
  vs <- gets vars
  return $ lookup v vs

  
isConsistent :: Bool -> NDS Bool
isConsistent partial = do 
  cs <- gets constraints
  vs <- gets vars
  let results = map (\p -> check p vs) cs
  return $ and (map (maybe partial id) results)  
  
  
getSolution :: NDS a -> ProblemState -> Maybe a
getSolution c i = listToMaybe (evalStateT c i)  
  


getFinalVars :: NDS Variables
getFinalVars = do 
  c <- isConsistent False
  guard c
  gets vars

isSolution = do 
        a <- getVar "goat"
        b <- getVar "cabbage"
        c <- getVar "farmer"
        d <- getVar "wolf"
        return (all (== Just "Bank2") [a,b,c,d])

onRiverSide :: Value -> NDS(Var,Int)
onRiverSide bank = do 
  st <- get
  vs <- return $ filter (\(x,y) -> y == bank) (vars st)
  vs' <- return $ filter (\(x,y) -> x /= "goat") (vs)
  let res = if null vs' then "goat" else (fst.head $ vs')  
  return  (res,length $ vs)
        
    
        
tryAllValues::Var -> NDS ()
tryAllValues var = do
    farmer <- getVar "farmer"
    let s = head. words. fromJust $ farmer
    let str = if s == "Bank1" then "Bank2" else "Bank1" 
    setVar var str
    (cabbageOrWolfElseGoat, count) <- onRiverSide s 
    c <- isConsistent True
    case c of 
        t| t == False && s == "Bank1" && count < 3 -> setVar cabbageOrWolfElseGoat str 
         | t == False -> setVar "goat" str `mplus` setVar "wolf" str `mplus` setVar "cabbage" str
         | t == True && str == "Bank2" -> setVar cabbageOrWolfElseGoat str
         |otherwise -> return ()

result :: Monad m => Bool -> [Predicate] -> Variables -> [Variables] -> m [Variables]               
result True _ variables plan =  return plan
result isSolution' constraints variables plan = do
                        vs <-return  $ (`getSolution` (PS variables constraints)) $ do 
                            tryAllValues "farmer"
                            getFinalVars
                        let s =  runStateT isSolution (PS (fromJust vs) constraints )                           
                        result ( fst .head $ s) constraints (fromJust vs) (plan ++ [(fromJust vs)])
 

 
main :: IO ()        
main = do
      let variables = [("cabbage","Bank1"),("wolf","Bank1"),("goat","Bank1"),("farmer","Bank1")]
          constraints = [(Or (Equal "goat" "farmer") (And (Not (Equal "wolf" "goat") )  (Not (Equal "goat" "cabbage"))))]
          problem = PS variables constraints
      plan <- result False  constraints variables [variables]
      mapM_ print plan
      