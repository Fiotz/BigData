sen2d :: Float -> (Float,Float)
funcao :: Float -> Float
funcao y = sqrt ( ( 1 - cos(y) ) / 2 )
sen2d x = ( ( 0 + funcao x) , ( 0 - funcao x))
main = do
	let x = sen2d 30
	print x
