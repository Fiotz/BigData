module Main where
 
import Control.Applicative ((<$>), (<*>))
import Control.Monad (foldM, forM_)
import Data.List ((\\))
 
-- Tipos
data Casa = Casa   
    { cor :: Cor     
    , pessoa   :: Pessoa
    , animal   :: Animal
    , bebida :: Bebida
    , fuma :: Fuma
    }
    deriving (Eq, Show)
 
data Cor = Vermelho | Verde | Azul | Amarelo | Branco
    deriving (Eq, Show, Enum, Bounded)
 
data Pessoa = Ingles | Sueco | Dinarmaques | Noruegues | Alemao
    deriving (Eq, Show, Enum, Bounded)
 
data Animal = Cachorros | Passaros | Gatos | Cavalo | Zebra
    deriving (Eq, Show, Enum, Bounded)
 
data Bebida = Cafe | Cha | Leite | Cerveja | Agua
    deriving (Eq, Show, Enum, Bounded)
 
data Fuma = PallMall | Dunhill | Blend | BlueMaster | Prince
    deriving (Eq, Show, Enum, Bounded)
 
type Solution = [Casa]
 
main :: IO ()
main = do
  forM_ solutions $ \sol -> mapM_ print sol
                            >> putStrLn "----"
  putStrLn "Nao tem mais solucoes"
 
 
solutions :: [Solution]
solutions = filter finalCheck . map reverse $ foldM next [] [1..5]
    where
      -- NOTA: lista de casas é gerada em ordem inversa
      next :: Solution -> Int -> [Solution]
      next sol pos = [h:sol | h <- novasCasas sol, consistent h pos]
 
 
novasCasas :: Solution -> Solution
novasCasas sol =    -- Todas as combinacoes ainda nao usadas
    Casa <$> new cor <*> new pessoa <*> new animal <*> new bebida <*> new fuma
    where
      new trait = [minBound ..] \\ map trait sol  -- :: [<Trait>]
 
 
consistent :: Casa -> Int -> Bool
consistent house pos = and                  -- consistente com as regras
    [ pessoa   `eh` Ingles     <=>   cor `eh` Vermelho              --  2
    , pessoa   `eh` Sueco     <=>   animal   `eh` Cachorros              --  3
    , pessoa   `eh` Dinarmaques     <=>   bebida `eh` Cha              --  4
    , cor `eh` Verde   <=>   bebida `eh` Cafe           --  6
    , animal   `eh` Passaros   <=>   fuma `eh` PallMall         --  7
    , cor `eh` Amarelo  <=>   fuma `eh` Dunhill          --  8
    , const (pos == 3)   <=>   bebida `eh` Leite             --  9
    , const (pos == 1)   <=>   pessoa   `eh` Noruegues              -- 10
    , bebida `eh` Cerveja    <=>   fuma `eh` BlueMaster       -- 13
    , pessoa   `eh` Alemao     <=>   fuma `eh` Prince           -- 14
    ]
    where
      infix 4 <=>
      p <=> q  =  p house == q house   -- Ambos verdadeiros ou ambos Falso
 
 
eh :: Eq a => (Casa -> a) -> a -> Casa -> Bool
(trait `eh` value) house  =  trait house == value
 
 
finalCheck :: [Casa] -> Bool
finalCheck solution = and                    -- Aplica as regras
    [ (cor `eh` Verde)   `leftOf` (cor `eh` Branco)  --  5
    , (fuma `eh` Blend  ) `nextTo` (animal   `eh` Gatos )  -- 11
    , (fuma `eh` Dunhill) `nextTo` (animal   `eh` Cavalo)  -- 12
    , (cor `eh` Azul   ) `nextTo` (pessoa   `eh` Noruegues  )  -- 15
    , (fuma `eh` Blend  ) `nextTo` (bebida `eh` Agua)  -- 16
    ]
    where
      nextTo :: (Casa -> Bool) -> (Casa -> Bool) -> Bool
      nextTo p q = leftOf p q || leftOf q p
      leftOf p q 
          | (_:h:_) <- dropWhile (not . p) solution = q h
          | otherwise                               = False