
main = do
	let lista = [ x | x <- [1..2017], (x `rem` 400 == 0) || ((x `rem` 4 == 0) && (x `rem` 100 /= 0))]
	let x = (length lista) `quot` 2
	let u = splitAt x lista
	print u
