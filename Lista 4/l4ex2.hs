somaDiag :: [[Int]] -> Int
somaDiag x = pegaDiag x ((length x)-1) 0
    where
        pegaDiag x cont res
            | cont >= 0 = pegaDiag x (cont-1) (res+ (x!!cont)!!cont)
            | otherwise = res

main = do
    let ind = somaDiag [[3,0,0],[0,2,0],[0,0,7]] 
    print ind
