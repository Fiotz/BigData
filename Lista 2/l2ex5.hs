calcDig :: Int -> Int
calcDig y = divDig y 0
	where
		divDig num rest
			| num >= 10 = divDig (num `div` 10) (rest + ( num `mod` 10))
			| otherwise = (num+rest)

main = do
	let x = calcDig 1029383913
	print x
