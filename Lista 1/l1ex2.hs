mult3 :: Int ->Bool
mult3 x = (mod x 3)==0

main = do
	print (mult3 5)
