import AbstractGraph as AG
import qualified AdjList as AL
import Data.List
import System.Environment

createGraph ::FilePath -> IO(Int,Bool,[(Int,Int,Int)])
createGraph  fname =  do
    content <- readFile fname
    let data' =  map words $lines content
    let [numbVert',orientation'] = head data'
    let (numbVert,orientation) = (read numbVert'::Int,read orientation'::Bool)
    let edgeList = foldr (\ls acc ->if null ls then acc else (let [b,e,w] = ls in  (read b, read e, read w ):acc))  [] $ tail data'
    return (numbVert,orientation,edgeList)
	
	
main = do
	[fname] <- getArgs 
	data' <- createGraph fname
	let (numbVert,orientation,edgeList) = data'
	print $( mkGraph orientation (0,numbVert-1) edgeList ::AL.Graph Int Int)

