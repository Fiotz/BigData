somaDiag :: [[Int]] -> Int
somaDiag x = pegaDiag x ((length x)-1) 0
    where
        pegaDiag x cont res
            | cont >= 0 = pegaDiag x (cont-1) (res+ (x!!cont)!!(( (length x)-1) - cont ))
            | otherwise = res

main = do
    let ind = somaDiag [[3,0,30],[0,2,0],[10,0,7]] 
    print ind
