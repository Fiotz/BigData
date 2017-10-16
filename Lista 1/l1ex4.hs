mult35 :: Int ->Bool
mult35 x = (mod x 5)==0 && (mod x 3)==0

main = do
	print (mult35 27)
