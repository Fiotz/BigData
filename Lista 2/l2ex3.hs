multEti :: Int -> Int -> Int 
multEti x y = multi x y 0
	where
		multi n m z
			| n ==1 = (m + z)
			| (n `mod` 2 ) == 1 = multi (n `div` 2) (m*2) (m+z)
			| otherwise = multi (n `div` 2) (m *2 ) (z) 

main = do
	let x = multEti 17 17
	print x
