criaLinha :: Int -> Int -> [Int]
criaLinha ind tam = cria ind tam 0 []
    where
        cria i t cont l
            |t==cont = l
            |i== cont = cria i t (cont+1) (l++[1])
            |otherwise = cria i t (cont+1) (l++[0])

criaMatriz :: Int -> [[Int]]
criaMatriz tam = [criaLinha x tam | x <- [0..tam-1]]
main = do
    let ind = criaMatriz 5
    print ind
