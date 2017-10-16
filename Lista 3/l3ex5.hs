produtoEscalar :: Num a => [a] -> [a] ->a
produtoEscalar x y = sum (zipWith (*) x y)

main = do
    let x = produtoEscalar [3,3,3] [3,3,3]
    print x