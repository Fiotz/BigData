fib = 1 : 2 : prox fib
  where
    prox (x : t@(y:_)) = (x+y) : prox t

main = do
    let x = foldl (+) 0 [n | n <- takeWhile (<4000000) fib, n`mod`2 ==0 ]
    print x
