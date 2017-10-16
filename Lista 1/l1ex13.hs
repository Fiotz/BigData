strinToDig :: String -> [Int]
strinToDig y = map (\x -> read[x]::Int) y

main = do
	let x = strinToDig "0123456789"
	print x
