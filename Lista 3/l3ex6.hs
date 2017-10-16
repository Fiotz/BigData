collatz :: Int -> Int
collatz x
    | x `mod`2 ==0 = x`div`2
    | otherwise = ((3*x)+1)

main = do
    let x = collatz 8
    let y = collatz 3
    print x
    print y