
main = do
	let lista = [ x | x <- [1..2017], (x `rem` 400 == 0) || ((x `rem` 4 == 0) && (x `rem` 100 /= 0))]
	let x = take 10 lista	
	print x
