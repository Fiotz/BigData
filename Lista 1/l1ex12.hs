conc :: String -> String -> String
conc x y = x ++ (' ' : y)

main = do
	let x = conc "Ola" "Mundo"
	print x
