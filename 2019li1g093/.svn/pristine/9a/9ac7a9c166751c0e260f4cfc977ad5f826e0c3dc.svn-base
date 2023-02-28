-- | Este mÃ³dulo define funÃ§Ãµes genÃ©ricas sobre vetores e matrizes, que serÃ£o Ãºteis na resoluÃ§Ã£o do trabalho prÃ¡tico.
module Tarefa0_2019li1g093 where

-- * FunÃ§Ãµes nÃ£o-recursivas.

-- | Um ponto a duas dimensÃµes dado num referencial cartesiado (distÃ¢ncias aos eixos vertical e horizontal)
--
-- <<http://li1.lsd.di.uminho.pt/images/cartesiano.png cartesisano>>
-- , ou num referencial polar (distÃ¢ncia Ã  origem e Ã¢ngulo do respectivo vector com o eixo horizontal).
--
-- <<http://li1.lsd.di.uminho.pt/images/polar.png polar>>
data Ponto = Cartesiano Double Double | Polar Double Angulo
    deriving (Show)
-- | Um Ã¢ngulo em graus.
type Angulo = Double

-- ** FunÃ§Ãµes sobre vetores

-- | Um 'Vetor' na representaÃ§Ã£o escalar Ã© um 'Ponto' em relaÃ§Ã£o Ã  origem.
type Vetor = Ponto
-- ^ <<http://li1.lsd.di.uminho.pt/images/vetor.png vetor>>

-- *** FunÃ§Ãµes gerais sobre 'Vetor'es.

-- | Soma dois 'Vetor'es.
somaVetores :: Vetor -> Vetor -> Vetor
somaVetores (Cartesiano x1 y1) (Cartesiano x2 y2) = Cartesiano (x1+x2) (y1+y2)
somaVetores v1 v2 = somaVetores (polar2cart v1) (polar2cart v2)

graus2rad:: Double -> Double
graus2rad g = g * (pi/180) 

rad2graus:: Double -> Double
rad2graus r = r*(180/pi)

polar2cart:: Vetor -> Vetor 
polar2cart (Polar r a) = Cartesiano (r * (cos $ graus2rad a)) (r * (sin $ graus2rad a))
polar2cart c@(Cartesiano x y) = c 


-- | Subtrai dois 'Vetor'es.
subtraiVetores :: Vetor -> Vetor -> Vetor
subtraiVetores (Cartesiano x1 y1) (Cartesiano x2 y2) = Cartesiano (x1-x2) (y1-y2)
subtraiVetores v1 v2 = subtraiVetores (polar2cart v1) (polar2cart v2)




-- | Multiplica um escalar por um 'Vetor'.
multiplicaVetor :: Double -> Vetor -> Vetor
multiplicaVetor a (Cartesiano x1 y1) = Cartesiano (a*x1) (a*y1)
multiplicaVetor a (Polar d ang) = multiplicaVetor a (polar2cart (Polar d ang))



-- ** FunÃ§Ãµes sobre rectas.

-- | Um segmento de reta Ã© definido por dois pontos.
type Reta = (Ponto,Ponto)

-- | Testar se dois segmentos de reta se intersetam.
--
-- __NB:__ Aplique as equaÃ§Ãµes matemÃ¡ticas bem conhecidas, como explicado por exemplo em <http://www.cs.swan.ac.uk/~cssimon/line_intersection.html>.
--intersetam :: Reta -> Reta -> Bool
--intersetam ((Cartesiano x1 y1),(Cartesiano x2 y2)) ((Cartesiano x3 y3),(Cartesiano x4 y4)) = 
  --  let  ta = ((y3-y4)*(x1-x3) + (x4-x3)*(y1-y2))/((x4-x3)*(y1-y2) - (x1-x2)*(y4-y3))
    --     tb = ((y1-y2)*(x1-x3) + (x2-x1)*(y1-y3))/((x4-x3)*(y1-y2) - (x1-x2)*(y4-y3))
--    in  if ta>=0 && ta<=1 && tb>=0 && tb<=1 then True else False
 
-- | Calcular o ponto de intersecao entre dois segmentos de reta.
--
-- __NB:__ Aplique as equaÃ§Ãµes matemÃ¡ticas bem conhecidas, como explicado por exemplo em <http://www.cs.swan.ac.uk/~cssimon/line_intersection.html>.
--intersecao :: Reta -> Reta -> Ponto
--intersecao ((Cartesiano x1 y1),(Cartesiano x2 y2)) ((Cartesiano x3 y3),(Cartesiano x4 y4)) =
--        let ta = ((y3-y4)*(x1-x3) + (x4-x3)*(y1-y2))/((x4-x3)*(y1-y2) - (x1-x2)*(y4-y3))
--        in somaVetores (Cartesiano x1 y1) (multiplicaVetor ta (subtraiVetores (Cartesiano x2 y2) (Cartesiano x1 y1)))
--intersecao (v1, v2) (v3, v4) = intersecao ((polar2cart v1),(polar2cart v2)) ((polar2cart v3),(polar2cart v4))
        
intersetam :: Reta -> Reta -> Bool
intersetam ( p1, p2 ) ( p3, p4 ) = 0 <= t_a && t_a <= 1 && 0 <= t_b && t_b <= 1 
        where
            Cartesiano x1 y1 = polar2cart p1
            Cartesiano x2 y2 = polar2cart p2
            Cartesiano x3 y3 = polar2cart p3
            Cartesiano x4 y4 = polar2cart p4

            t_a = ( (y3-y4)*(x1-x3)+(x4-x3)*(y1-y3) ) / ( (x4-x3)*(y1-y2)-(x1-x2)*(y4-y3) )
            t_b = ( (y1-y2)*(x1-x3)+(x2-x1)*(y1-y3) ) / ( (x4-x3)*(y1-y2)-(x1-x2)*(y4-y3) )

intersecao :: Reta -> Reta -> Ponto
intersecao ( p1, p2 ) (p3, p4) = Cartesiano (x1 + t_a*(x2-x1)) (y1 + t_a*(y2 - y1))
        where
            Cartesiano x1 y1 = polar2cart p1
            Cartesiano x2 y2 = polar2cart p2
            Cartesiano x3 y3 = polar2cart p3
            Cartesiano x4 y4 = polar2cart p4

            t_a = ( (y3-y4)*(x1-x3)+(x4-x3)*(y1-y3) ) / ( (x4-x3)*(y1-y2)-(x1-x2)*(y4-y3) )
-- ** FunÃ§Ãµes sobre listas

-- *** FunÃ§Ãµes gerais sobre listas.
--
-- FunÃ§Ãµes nÃ£o disponÃ­veis no 'Prelude', mas com grande utilidade.

-- | Verifica se o indice pertence Ã  lista.
--
-- __SugestÃ£o:__ use a funÃ§Ã£o 'length' que calcula tamanhos de listas

eIndiceListaValido :: Int -> [a] -> Bool
eIndiceListaValido x [] = False
eIndiceListaValido x (h:t) = if x>=0 && x<=length (h:t)-1 then True else False
                            

-- ** FunÃ§Ãµes sobre matrizes.

-- *** FunÃ§Ãµes gerais sobre matrizes.

-- | A dimensÃ£o de um mapa dada como um par (/nÃºmero de linhas/,/nÃºmero de colunhas/).
type DimensaoMatriz = (Int,Int)

-- | Uma posiÃ§Ã£o numa matriz dada como um par (/linha/,/colunha/).
-- As coordenadas sÃ£o dois nÃºmeros naturais e comeÃ§am com (0,0) no canto superior esquerdo, com as linhas incrementando para baixo e as colunas incrementando para a direita:
--
-- <<http://li1.lsd.di.uminho.pt/images/posicaomatriz.png posicaomatriz>>
type PosicaoMatriz = (Int,Int)

-- | Uma matriz Ã© um conjunto de elementos a duas dimensÃµes.
--
-- Em notaÃ§Ã£o matemÃ¡tica, Ã© geralmente representada por:
--
-- <<https://upload.wikimedia.org/wikipedia/commons/d/d8/Matriz_organizacao.png matriz>>
type Matriz a = [[a]]

-- | Calcula a dimensÃ£o de uma matriz.
--
-- __NB:__ Note que nÃ£o existem matrizes de dimensÃ£o /m * 0/ ou /0 * n/, e que qualquer matriz vazia deve ter dimensÃ£o /0 * 0/.
--
-- __SugestÃ£o:__ relembre a funÃ§Ã£o 'length', referida anteriormente.

dimensaoMatriz :: Matriz a -> DimensaoMatriz
dimensaoMatriz [[]] = (0,0)
dimensaoMatriz m = (length m,length (head m))


-- | Verifica se a posiÃ§Ã£o pertence Ã  matriz.
ePosicaoMatrizValida :: PosicaoMatriz -> Matriz a -> Bool 
ePosicaoMatrizValida (a,b) (h:t) = if a>=0 && b >= 0 && a<=(length (h:t)) && b <=(length h) then True else False 

-- * FunÃ§Ãµes recursivas.

-- ** FunÃ§Ãµes sobre Ã¢ngulos

-- | Normaliza um Ã¢ngulo na gama [0..360).
--  Um Ã¢ngulo pode ser usado para representar a rotaÃ§Ã£o
--  que um objecto efectua. Normalizar um Ã¢ngulo na gama [0..360)
--  consiste, intuitivamente, em extrair a orientaÃ§Ã£o do
--  objecto que resulta da aplicaÃ§Ã£o de uma rotaÃ§Ã£o. Por exemplo, Ã© verdade que:
--
-- prop> normalizaAngulo 360 = 0
-- prop> normalizaAngulo 390 = 30
-- prop> normalizaAngulo 720 = 0
-- prop> normalizaAngulo (-30) = 330
normalizaAngulo :: Angulo -> Angulo
normalizaAngulo x = if x>=0 && x<360 then x else
                    if x>=360 then normalizaAngulo (x-360) else
                    if x<0 then normalizaAngulo (x+360) else error "erro"


-- ** FunÃ§Ãµes sobre listas.

-- | Devolve o elemento num dado Ã­ndice de uma lista.
--
-- __SugestÃ£o:__ NÃ£o use a funÃ§Ã£o (!!) :: [a] -> Int -> a :-)
encontraIndiceLista :: Int -> [a] -> a
encontraIndiceLista _ [p] = p  
encontraIndiceLista idx (h:t) =if idx == 0 then h else encontraIndiceLista (idx-1) t 


-- | Modifica um elemento num dado Ã­ndice.
--
-- __NB:__ Devolve a prÃ³pria lista se o elemento nÃ£o existir.

atualizaIndiceLista :: Int -> a -> [a] -> [a]
atualizaIndiceLista idx elem (h:t) = if idx==0 then (elem:t) else h:(atualizaIndiceLista (idx-1) elem t)


-- ** FunÃ§Ãµes sobre matrizes.

-- | Devolve o elemento numa dada 'Posicao' de uma 'Matriz'.
--encontraPosicaoMatriz :: PosicaoMatriz -> Matriz a -> a
--encontraPosicaoMatriz (m,n) (h:t) = if m==0 then encontraIndiceLista n h else encontraPosicaoMatriz (m-1,n) t 

encontraPosicaoMatriz :: PosicaoMatriz -> Matriz a -> a
encontraPosicaoMatriz (m,n) mat = encontraIndiceLista n (encontraIndiceLista m mat)




-- __NB:__ Devolve a prÃ³pria 'Matriz' se o elemento nÃ£o existir.
atualizaPosicaoMatriz :: PosicaoMatriz -> a -> Matriz a -> Matriz a
atualizaPosicaoMatriz (m,n) elem (h:t) = if m==0 then (atualizaIndiceLista n elem h):t else h:(atualizaPosicaoMatriz (m-1,n) elem t)

