ehPrimo :: Int -> Bool
ehPrimo y = tamanho [x | x <- [2 .. (y - 1)], y `mod` x ==0]
	where
		tamanho z
			| (length z) > 0 = False
			| otherwise = True 

main = do
	let x = ehPrimo 101
	print x
