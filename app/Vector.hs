module Vector (Vector, newVector, size, capacity, resize, set, get) where

newtype Vector a = Vector (Int, [(Int, a)])

newVector :: Int -> Vector a
newVector n = Vector (n, [])

size :: Vector a -> Int
size (Vector (n, l)) = n

capacity :: Vector a -> Int
capacity (Vector (n, l)) = length l

resize :: Vector a -> Int -> Vector a
resize (Vector (n, l)) newSize | newSize <= 0 = Vector (0, [])
                               | newSize >= n = Vector (newSize, l)
                               | otherwise = let l' = [(indic, value)| (indic, value) <- l, indic < newSize - 1 ] in
                                           Vector (newSize, l')

set :: Vector a -> a -> Int-> Maybe (Vector a)
set (Vector (n,l)) value indic | indic < 0 || indic > n - 1 = Nothing
                               | otherwise = let updateInList [] value indic = [(indic, value)]
                                                 updateInList ((i, v):l) value indic = if indic == i then (i, value) : l
                                                                                                     else (i,v) : updateInList l value indic
                                             in Just (Vector(n, updateInList l value indic))

get :: Vector a -> Int -> Maybe a
get (Vector (n,l)) indic | indic < 0 || indic > n - 1 = Nothing
                         | otherwise = let  getNth [] indic = Nothing
                                            getNth ((i,v):l) indic = if i == indic then Just v
                                                                                   else getNth l indic
                                       in getNth l indic
