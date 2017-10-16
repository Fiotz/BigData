mult35 :: Int ->Bool
mult35 x = ( x < -1) || ( ( x > 1 ) && (mod x 2)==0)

main = do
	print (mult35 (4))
