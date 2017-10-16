pascal :: Int -> [Int]
pascal 0 = [1]
pascal n = zipWith (+) ([0] ++ pascal (n-1)) (pascal(n-1) ++ [0])

findInPascal :: Int -> Int -> Int
findInPascal i j = last (take j (pascal i))
main = do 
    let x = findInPascal 7 3  -- linha por coluna comecando em i1 j1
    print x
