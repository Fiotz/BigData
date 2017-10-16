divisivel20 :: Int -> Bool
divisivel20 x = tamanho [n | n <- [1..20],x`mod`n /= 0 ]
    where
        tamanho lista
            |length lista >0 = False
            |otherwise = True
main = do
    let x = divisivel20 (1*2*3*4*5*6*7*8*9*10*11*12*13*14*15*16*17*18*19*20)
    let y = divisivel20 30
    print x
    print y