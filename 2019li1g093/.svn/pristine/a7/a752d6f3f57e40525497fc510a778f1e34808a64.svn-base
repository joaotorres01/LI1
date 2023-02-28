-- == Tarefa 4
-- |  O objetivo desta Tarefa era adicionar físicas realisticas ao Jogo, ou seja, tinhamos que definir duas funções distintas que seriam usadas numa função prinicipal. A primeira
-- | destas duas funções servia para definir como é que seria a aceleração dos Jogadores, estivessem eles no Ar ou no Chão, sendo que tivemos que criar funções que nos diziam, tendo
-- | em conta o tempo de andamento do Jogador, alterando assim a sua velocidade, se estivesse no chão, ou também a sua gravidade, se estivesse no Ar. Na segunda função era-nos pedido
-- | que definissemos as interações que aconteciam quando o Jogador se queria mover, estivesse ele no Ar, no Chão ou Morto. Para isto tivemos que recorrer ao uso de vetores que nos 
-- | ajudavam a perceber melhor as interações do vetor velocidade com o vetor gravidade. Quando o Jogador estava no Ar, dividimos em 3 casos, quando ele ficava no Ar na mesma peça,
-- | sendo que apenas alterávamos a sua distância e altura, quando ficava no Ar no início da Peça seguinte, sendo que alterávamos apenas a sua altura e distância, e por último 
-- | quando ele embatia na Peça atual, sendo que depois definiamos se o Jogador ficava Morto ou no Chão, tendo em conta a diferença de inclinações entre ele e a Peça. Quando o Jogador
-- | estava nochão também dividimos em 3 casos, sendo que o primeiro era quando ele não andava o suficiente para mudar de Peça, alterando apenas a distância. Nos outros 2 casos tinhamos
-- | que verificar se o Jogador ao ficar no início da Peça seguinte se esta se tratava duma Rampa, tendo o Jogador que ficar no Ar, ou se se tratava de uma Reta, ficando o Jogador no Chão.
-- | Por último, verificamos o cas em que o Jogador estava Morto, sendo que apenas foi preciso ver se o tempo era superior ou inferior ao Timeout, podendo o Jogador continuar Morto
-- | ou ficar no Chão sem acelerar.

module Tarefa4_2019li1g093 where

import LI11920
import Tarefa2_2019li1g093
import Tarefa1_2019li1g093
import Tarefa0_2019li1g093
-- * Testes
-- | Testes unitários da Tarefa 4.
--
-- Cada teste é um par (/tempo/,/'Mapa'/,/'Jogador'/).
testesT4 :: [(Double,Mapa,Jogador)]
testesT4 = [t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13]
          where
               t1 = (1,[[Recta Terra 0, Rampa Lama 0 2, Rampa Relva 2 0, Rampa Relva 0 2, Recta Terra 2],[Recta Terra 0, Recta Relva 0, Recta Boost 0, Recta Lama 0, Recta Terra 0]],(Jogador 0 1.2 0.9 2 (Chao True)))--jogador está no chao, acelera e continua no chao na mesma peça
               t2 = (3,[[Recta Terra 0,Recta Boost 0,Recta Relva 0,Recta Terra 0],[Recta Terra 0,Rampa Terra 0 1,Recta Terra 1,Rampa Terra 1 0]],(Jogador 0 1.5 1 2 (Chao True)))--jogador está no chão numa peça, anda uma distancia suficiente para passar de peça e fica no inicio da peça seguinte
               t3 = (2,[[Recta Terra 0,Recta Boost 0,Recta Terra 0],[Recta Terra 0,Recta Relva 0,Recta Relva 0],[Recta Terra 0,Rampa Terra 0 2,Rampa Relva 2 3]],(Jogador 2 0.5 0 2 (Morto 1.0)))--jogador está morto e perde o timeout
               t4 = (0.5,[[Recta Terra 0,Recta Relva 0,Recta Relva 0,Recta Terra 0],[Recta Terra 0,Rampa Terra 0 1,Recta Terra 1,Rampa Terra 1 0]],(Jogador 0 0.5 0 2 (Morto 1.0)))--jogador está morto e não perde o timeout
               t5 = (2,[[Recta Terra 0, Rampa Relva 0 2, Rampa Relva 2 0, Rampa Relva 0 2, Recta Terra 2],[Recta Terra 0, Recta Relva 0, Recta Boost 0, Recta Lama 0, Recta Terra 0]],(Jogador 0 2.5 1 2 (Ar 2 0 10)))--jogador está no Ar e cai numa rampa, ficando morto graças à diferença de inclinação ser >=45
               t6 = (2,[[Recta Terra 0,Recta Boost 0,Recta Terra 0],[Recta Terra 0,Recta Boost 0,Recta Terra 0]], (Jogador 0 1 1 1 (Ar 1 0 10)))--jogador está no Ar e cai numa reta, ficando vivo graças à diferença de inclinaçao ser <45
               t7 = (2,[[Recta Terra 0, Rampa Relva 0 2, Rampa Relva 2 0, Rampa Relva 0 2, Recta Terra 2],[Recta Terra 0, Recta Relva 0, Recta Boost 0, Recta Lama 0, Recta Terra 0]],(Jogador 0 1.3 1 2 (Chao True)))--jogador passa de uma rampa para outra peça, com uma diferença de inclinação >=45
               t8 = (2,[[Recta Terra 0, Rampa Relva 0 2, Rampa Relva 2 0, Rampa Relva 0 2, Recta Terra 2],[Recta Terra 0, Recta Relva 0, Recta Boost 0, Recta Lama 0, Recta Terra 0]],(Jogador 0 2.3 1 2 (Chao True)))--jogador passa de uma rampa para outra peça, com uma diferença de inclinacão <45
               t9 = (0.3,[[Recta Terra 0,Recta Boost 0,Recta Terra 0],[Recta Terra 0,Recta Boost 0,Recta Terra 0]],(Jogador 0 0 1 2 (Chao False)))--jogador encontra-se no Chao numa peça e não anda o suficiente para mudar de peça
               t10 = (1.5,[[Recta Terra 0,Recta Boost 0,Recta Terra 0],[Recta Terra 0,Recta Boost 0,Recta Terra 0]], (Jogador 0 1 1 1 (Ar 5 0 1)))--jogador está no Ar e permanece no Ar no inicio da peça seguinte
               t11 = (1,[[Recta Terra 0, Rampa Cola 0 2, Rampa Relva 2 0, Rampa Relva 0 2, Recta Terra 2],[Recta Terra 0, Recta Relva 0, Recta Boost 0, Recta Lama 0, Recta Terra 0]],(Jogador 0 1.2 0.9 2 (Chao True)))--jogador está no chao, acelera e continua no chao na mesma peça, com Cola
               t12 = (0.1,[[Recta Terra 0,Recta Boost 0,Recta Terra 0],[Recta Terra 0,Recta Boost 0,Recta Terra 0]], (Jogador 0 1 1 1 (Ar 5 0 1)))--jogador está no Ar e permanece no Ar a meio da sua peça
               t13 = (1.5, [[Recta Terra 0, Rampa Terra 0 2, Recta Terra 2, Rampa Terra 2 0],[Recta Terra 0, Recta Terra 0, Recta Terra 0, Recta Terra 0]], (Jogador 0 2.6 1 1 (Chao True))) --jogador está numa reta e passa para uma rampa que desce, ficando no ar devido à diferença de inclinaçao

-- * Funções principais da Tarefa 4.

-- | Avança o estado de um 'Jogador' um 'passo' em frente, durante um determinado período de tempo.
passo :: Double -- ^ O tempo decorrido.
     -> Mapa    -- ^ O mapa utilizado.
     -> Jogador -- ^ O estado anterior do 'Jogador'.
     -> Jogador -- ^ O estado do 'Jogador' após um 'passo'.
passo t m j = move t m (acelera t m j)

-- | Altera a velocidade de um 'Jogador', durante um determinado período de tempo.
acelera :: Double -- ^ O tempo decorrido.
     -> Mapa    -- ^ O mapa utilizado.
     -> Jogador -- ^ O estado anterior do 'Jogador'.
     -> Jogador -- ^ O estado do 'Jogador' após acelerar.
acelera t m (Jogador p d v c (Chao b)) = (Jogador p d (v' v (Chao b) (piso peca) t) c (Chao b))
                              where
                                   peca = encontraPosicaoMatriz (p,dist) m
                                   dist = floor d
acelera t m (Jogador p d v c (Ar a i g)) = (Jogador p d (vAr v t ) c (Ar a i (g' g t)))
acelera t m (Jogador p d v c (Morto time)) = (Jogador p d v c (Morto time))

-- | Função que recebe uma Peça e diz qual o Piso dessa Peça
piso :: Peca -> Piso
piso (Recta x p) = x
piso (Rampa x h hf) = x


-- | Altera a posição de 'Jogador', durante um determinado período de tempo.
move :: Double -- ^ O tempo decorrido.
     -> Mapa    -- ^ O mapa utilizado.
     -> Jogador -- ^ O estado anterior do 'Jogador'.
     -> Jogador -- ^ O estado do 'Jogador' após se movimentar.
move t m (Jogador p d v c (Ar a i g)) = if verificaDist x d && (intersetam (pontoInicial, pontoFinal) (reta patual d))==False
                                        then (Jogador p dist v c (Ar y4 i g))
                                        else moveAr (Jogador p d v c (Ar a i g)) t m
                              where
                                dist = (fromIntegral((floor d)+1)) 
                                patual = encontraPosicaoMatriz (p,(floor d)) m
                                vetorVel = Polar v i
                                vetorg = Polar g (-90)
                                pontoInicial@(Cartesiano x2 y2) = (Cartesiano d a)
                                vetorFinal@(Cartesiano x y) = polar2cart (multiplicaVetor t (somaVetores vetorVel vetorg))
                                pontoFinal@(Cartesiano x3 y3) = somaVetores pontoInicial vetorFinal 
                                pontoAr@(Cartesiano x4 y4) = intersecao (pontoInicial,pontoFinal) (Cartesiano dist 0, Cartesiano dist 1)
move t m (Jogador p d v c (Chao b)) = if verificaDist x d 
                                      then moveChao  (Jogador p d v c (Chao b)) patual pseg 
                                      else (Jogador p (x+d) v c (Chao b))
                              where
                                pseg = encontraPosicaoMatriz (p,((floor d)+1)) m
                                patual = encontraPosicaoMatriz (p,(floor d)) m
                                incatual = inclinacaoPecaAnterior patual
                                vetorVel = Polar v incatual
                                vetorg = Polar 0 (-90)
                                vetorFinal@(Cartesiano x y) = polar2cart (multiplicaVetor t (somaVetores vetorVel vetorg))
move t m (Jogador p d v c (Morto time)) = if t<time  
                                          then (Jogador p d v c (Morto (time-t))) 
                                          else (Jogador p d v c (Chao False))

-- | Função que calcula qual o valor da velocidade, tendo em conta qual o atrito de cada Peça
v' :: Double -- valor da velocidade do Jogador, não tendo em conta qual a peça em que está 
    -> EstadoJogador -- Estado do Jogador
    -> Piso -- Piso em que se encontra o Jogador
    -> Double -- tempo que o jogador se vai movimentar
    -> Double -- valor da velocidade depois de ter sido "descontado" o valor do Atrito
v' v e piso t = if (v + ((fromIntegral(accelMota v e) - ((atrito piso e) * v)) * t)) >= 0 
                then (v + ((fromIntegral(accelMota v e) - ((atrito piso e) * v)) * t)) 
                else 0.0

-- | Função que verifica se o Jogador pode acelerar mais ou se já vai na máxima aceleração
accelMota :: Double -> EstadoJogador-> Int
accelMota v e = if (v < 2 && ifAcelera e) then 1 else 0   

-- | Funçao que recebe o Piso da Peça onde o Jogador se encontra e o seu Estado e dá o valor do Atrito correspondente
atrito :: Piso -- Piso da Peça onde o Jogador se encontra
        -> EstadoJogador -- Estado do Jogador
        -> Double -- Valor do atrito correspondente ao Piso onde o jogador se encontra
atrito p e = if p==Terra && ifchao e then 0.25 else
             if p==Relva && ifchao e then 0.75 else
             if p==Boost && ifchao e  then -0.50 else
             if p==Lama && ifchao e then 1.50 else 3.00 

-- | Função que calcula qual a velocidade do jogador quando este está no Ar
vAr :: Double -- velocidade atual do jogador
    -> Double -- tempo em que o jogador se vai movimentar
    -> Double -- velocidade do jogador no Ar
vAr v t =  v - (0.125 * v * t) 
--if  v - (0.125 * v * t) > 0 then  v - (0.125 * v * t)  else 0.0

-- | Função que vê qual o efeito da gravidade no Jogador
g' :: Double -- gravidade atual do jogador
    -> Double -- tempo em que o jogador se vai movimentar
    -> Double -- gravidade atualizada do jogador
g' g t  = g + t

-- | Função que verifica se um jogador está no Chão
ifAcelera :: EstadoJogador -> Bool
ifAcelera (Chao True) = True
ifAcelera _ = False

-- | Função que altera as caracteristicas do Jogador quando este está no Ar e vai mudar de Peça
moveAr :: Jogador -- Jogador que se está a mover
        -> Double -- Tempo que o jogador vai estar em movimento
        -> Mapa -- Mapa onde o jogador se encontra
        -> Jogador -- Jogador depois de ter percorrido a trajetória no Ar
moveAr (Jogador p d v c (Ar a i g)) t map = if intersetam (reta patual d) (Cartesiano d a, pontoFinal) == False 
                                            then (Jogador p (x+d) v c (Ar (y+a) i g))
                                            else if abs(i-inc)>=45 
                                            then (Jogador p z 0 c (Morto 1.0))
                                            else (Jogador p z v c (Chao False))               
                              where
                               inc = inclinacaoPecaAnterior pseg
                               patual = encontraPosicaoMatriz (p,(floor d)) map
                               pseg = encontraPosicaoMatriz (p,((floor d)+1)) map
                               vetorVel = Polar v i
                               vetorg = Polar g (-90)
                               vetorFinal@(Cartesiano x y) = polar2cart (multiplicaVetor t (somaVetores vetorVel vetorg))
                               pontoFinal = (Cartesiano (x+d) (y+a))
                               intersec@(Cartesiano z h) = intersecao (reta patual d) (Cartesiano d a, pontoFinal)

-- | Função que altera as caracteristicas do Jogador quando este está no Chao e vai mudar de Peça
moveChao :: Jogador -- Jogador que se está a mover
          -> Peca -- Peça onde o Jogador se encontra
          -> Peca -- Peça para a qual o Jogador se vai mover
          -> Jogador -- Jogador depois de ter percorrido a sua trajetória no chão
moveChao (Jogador p d v c (Chao b)) patual pseg = if incseg >= incatual 
                                                  then (Jogador p dist v c (Chao b))
                                                  else (Jogador p dist v c (Ar altAr (inclinacaoPecaAnterior patual) 0)) 
                              where 
                                   incatual = inclinacaoPecaAnterior patual
                                   incseg = inclinacaoPecaAnterior pseg
                                   dist = fromIntegral((floor d)+1)
                                   altAr = altPeca dist pseg

-- | Funçao que recebe uma Peça e uma distancia e faz a reta correspondente ao ponto onde o jogador está e o fim da peça onde se encontra
reta :: Peca -> Double-> Reta 
reta (Recta p h) d  = ((Cartesiano (fromIntegral (floor d)) (fromIntegral h)),(Cartesiano (fromIntegral (floor (d+1))) (fromIntegral h)))
reta (Rampa p hi hf) d = ((Cartesiano (fromIntegral (floor d)) (fromIntegral hi)),(Cartesiano (fromIntegral (floor (d+1))) (fromIntegral hf)))

 
-- | verifica se o Ponto final fica na peça seguinte ou se fica na sua peça
verificaDist :: Double -> Double -> Bool
verificaDist x d = if floor(x+d) > floor d 
                   then True 
                   else False