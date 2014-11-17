import Control.Monad
import Data.List
import System.Environment
import Data.Maybe

{-
   Дан текстовый файл (inventory.txt)  с перечислением всей имеющейся на складе
   лёгкой брони. Сформируйте список имеющихся полных комплектов брони одного
   вида (kind). Указание: в решении рекомендуется пользоваться монадическими
   операциями всюду, где только возможно.
-}

data ArmorType = Shield | Helmet | Gauntlets | Boots | Cuirass
   deriving (Show, Read, Eq, Ord, Enum, Bounded)
data ArmorKind = Chitin | Hide | Leather | Elven | Scaled | Glass | ImperialLight
   deriving (Show, Read, Eq, Ord, Enum, Bounded)
data ArmorItem = ArmorItem ArmorKind ArmorType 
   deriving (Show, Eq)
data ArmorKit = ArmorKit ArmorKind [ArmorType]
   deriving (Show, Eq)

armKind = [ Chitin,Hide, Leather , Elven , Scaled , Glass , ImperialLight]

  
armorItem :: [String] -> ArmorItem
armorItem [kind ,type'] = ArmorItem (read kind) (read type')   
   
   
loadInventory :: FilePath -> IO [ArmorItem]
loadInventory fname = readFile fname >>= return . map (armorItem. words) . lines 

buildArmorKit :: ArmorKind -> [ArmorItem] -> Maybe ArmorKit
buildArmorKit armorKind armorItems  = if (== 5). length $ ls then Just (ArmorKit armorKind ls) else Nothing
            where  ls = foldl (\acc (ArmorItem kind type') -> if kind == armorKind then type':acc else acc ) [] armorItems

buildKits :: [ArmorItem] -> Maybe [ArmorKit]
buildKits ls  = if not. null $ ls' then Just ls' else Nothing
    where ls' =  foldl (\acc kind ->let l =  buildArmorKit kind ls in if isNothing l then acc else (fromJust l): acc) [] armKind

main = (head `liftM` getArgs) >>= loadInventory >>= return . buildKits >>= print
