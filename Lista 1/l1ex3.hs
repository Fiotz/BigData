mult5 :: Int ->Bool
mult5 x = (mod x 5)==0

main = do
	print (mult5 7)
