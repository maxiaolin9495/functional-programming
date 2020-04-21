module Exercise_5 where
import Test.QuickCheck
import Data.List

{- H5.3 -}
{-WETT-}
type Vertex = Int
type Edge = (Vertex, Vertex)
type Graph = ([Vertex], [Edge])

findStart :: Graph -> Vertex
findStart ([], e) = -1
findStart (v:vs, e) = case filter ((==v).snd) e of [] -> v
                                                   _  -> findStart (vs, e)

buildDAG :: [Vertex] -> Vertex -> [(Vertex, Int)]
buildDAG xs s = [ if x /= s then (x,-1) else (x,0) | x <- xs]

-- remove used vertex
removeV :: [Vertex] -> Vertex -> [Vertex]
removeV vs start = [ v | v <- vs, v /= start]

-- remove used edges
removeE :: [Edge] -> Vertex -> [Edge]
removeE e vertex = [(v1,v2)|(v1,v2) <- e, v1 /= vertex]

getLength :: [(Vertex, Int)] -> Vertex -> Int
getLength [] final = 0
getLength ((v,i):dag) vertex | v == vertex = i
                            | otherwise = getLength dag vertex


changeLength::[(Vertex, Int)] -> [(Vertex, Int)] -> Vertex -> [(Vertex, Int)]
changeLength [] vToChangeList start= []
changeLength dag [] start = dag
changeLength ((v,i):dag) ((v',i'):vToChangeList) start | v == v' = (v, maximum [i, i']) : changeLength dag vToChangeList start
                                                       | v == start = changeLength dag ((v',i'):vToChangeList) start
                                                       | otherwise =  (v,i) : changeLength dag ((v',i'):vToChangeList) start
-- store the new value
writeIn :: [(Vertex, Int)] -> Vertex -> [Edge] -> [(Vertex, Int)]
writeIn dag start es = let len = getLength dag start in
                    let vToChangeList = sortOn fst [(e, 1 + len) |(s, e)<- es, s == start] in
                    changeLength dag vToChangeList start


longestPath :: Graph -> Vertex -> Int
longestPath (v,e) final = let start = findStart (v,e) in
                          let dag = buildDAG v start in
                          let topologicalSort vs es dag final = let s = findStart (vs, es) in if s == final || s == -1 then getLength dag final
                                                              else topologicalSort (removeV vs s) (removeE es s) (writeIn dag s es) final
                          in topologicalSort (sort v) (sortOn fst e) dag final




{-TTEW-}

-- generates a DAG with u vertices and only one node without incoming edges
-- you can use this function to test your implementation using QuickCheck
genDag :: Int -> Gen Graph
genDag n = let v = [1..n] in
  do b <- mapM (\i -> choose (1,n-i)) [1..n-1]
     t <- mapM (\(c,i) -> vectorOf c (choose (i+1, n))) (zip b [1..n])
     let e = nub $ ([(1, i) | i<-[2..n]] ++ edges t 1 [])
     return $ (v,e)
  where
    edges [] _ acc = acc
    edges (ts:xs) i acc = edges xs (i+1) (acc ++ [(i,t) | t<-ts])
