binom :: Integer -> Integer -> Integer
binom n m = product [(1+n-m)..n] `div` product[1..m]
main = do 
    let x = binom 10 2
    print x