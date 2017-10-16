ehTri :: Int -> Int -> Int -> Bool
ehTri x y z = ( x + y > z ) && ( x + z > y ) && ( z + y > x )

tipoTri :: Int -> Int -> Int -> String
tipoTri x y z 
	| not( ehTri x y z ) = "Nao eh um triangulo"
	| x == y && y ==z = "Triaqulo equilatero"
	| (x == y) || (x == z) || (y == z) = "Triangulo isosceles"
	| otherwise = "Triangulo escaleno" 

main = do
	let x = tipoTri 10 10 0
	print x 
