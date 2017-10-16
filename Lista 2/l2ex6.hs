calcDig :: Integer -> Integer
calcDig y = divDig y 0
	where
		divDig num rest
			| num >= 10 = divDig (num `div` 10) (rest + ( num `mod` 10))
			| otherwise = (num+rest)

persisAdd :: Integer -> Integer
persisAdd x = conta x 0
	where
	conta num cont
		| num >= 10 = conta (calcDig num) (cont+1) 
		| otherwise = cont

main = do
	let x = persisAdd 19999999999999999999999
	let y = persisAdd 199
	let z = persisAdd 19
	print x
	print y
	print z
	
