ehTri :: Int -> Int -> Int -> Bool
ehTri x y z = ( x + y > z ) && ( x + z > y ) && ( z + y > x )

main = do
	let x = ehTri 1 10 100
	print x
