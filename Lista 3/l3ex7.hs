collatz :: Int -> Int
collatz x
    | x `mod`2 ==0 = x`div`2
    | otherwise = ((3*x)+1)

tamanho :: Int -> [Int] -> [Int]
tamanho x y
    |x ==1 = x:y
    |otherwise = tamanho (collatz x) (x:y)

collatzLen :: Int -> Int
collatzLen y = length ([x | x <- (tamanho (collatz y) [] )])

main = do
    let x = collatzLen 8
    let y = collatzLen 3
    print x
    print y