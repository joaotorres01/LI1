-- | Este módulo define funções comuns da Tarefa 6 do trabalho prático.
module Tarefa6_2019li1g093 where

import LI11920
import Tarefa4_2019li1g093
import Tarefa2_2019li1g093
import Tarefa0_2019li1g093

-- * Funções principais da Tarefa 6.

-- | Define um ro'bot' capaz de jogar autonomamente o jogo.
bot :: Int          -- ^ O identificador do 'Jogador' associado ao ro'bot'.
    -> Estado       -- ^ O 'Estado' para o qual o ro'bot' deve tomar uma decisão.
    -> Maybe Jogada -- ^ Uma possível 'Jogada' a efetuar pelo ro'bot'.
bot indice e = Just (jogar indice e)

-- | Função que lê o Estado atual do Jogo e escolhe a jogada mais indicada para o robot efetuar
jogar :: Int -- Número do jogador que vai efetuar a jogada
    -> Estado -- Estado atual do jogo
    -> Jogada -- Jogada mais indicada descoberta pelo robot
jogar indice (Estado m lj) = if ifAr e == True 
                             then 
                                if (intersetam (pontoInicial, pontoFinal) (reta patual d)) == True 
                                then inclinar e patual 
                                else inclinar e pfrente 
                             else
                             if ifchao e == True 
                             then 
                                 if piso (ptras) == Boost && c>0 
                                 then Dispara 
                                 else movimentar jog m pfrente pesq pdir 
                             else Acelera
                                where
                                    jog@(Jogador p d v c e) = encontraIndiceLista indice lj
                                    pfrente = encontraPosicaoMatriz (p,(dist+1)) m 
                                    patual = encontraPosicaoMatriz (p,dist) m
                                    ptras = encontraPosicaoMatriz (p,dist-1) m
                                    pesq = encontraPosicaoMatriz (p-1, dist+1) m
                                    pdir = encontraPosicaoMatriz (p+1, dist+1) m
                                    dist = floor d
                                    pontoInicial@(Cartesiano x2 y2) = (Cartesiano d a)
                                    pontoFinal@(Cartesiano x3 y3) = somaVetores pontoInicial vetorFinal
                                    vetorFinal@(Cartesiano x y) = polar2cart (multiplicaVetor 0.2 (somaVetores vetorVel vetorg))
                                    vetorVel = Polar v i
                                    vetorg = Polar g (-90)
                                    (Ar a i g) = calcEstado e

-- | Função que dado um Estado serve para dar esse mesmo Estado
calcEstado :: EstadoJogador -> EstadoJogador 
calcEstado x = x 

-- | Função que vê qual o Piso de uma Peça e indica o seu respetivo atrito
calcAtrito :: Piso -> Double
calcAtrito Terra = 0.25
calcAtrito Relva = 0.75
calcAtrito Boost = -0.25
calcAtrito Lama = 1.50
calcAtrito Cola = 3.00

-- | Função que vê a posiçao atual de um jogador no mapa, as peças que tem à sua frente e decide qual a jogada mais indicada a efetuar
movimentar :: Jogador -- Jogador que se quer mover
            -> Mapa -- Mapa atual do Jogo
            -> Peca -- Peça da mesma pista mas uma posiçao à frente
            -> Peca -- Peça da pista à esquerda do jogador e uma posição à frente
            -> Peca -- Peça da pista à direita do jogador e uma posição à frente
            -> Jogada -- Jogada mais indicada descoberta pelo robot
movimentar (Jogador p d v c e) m pfrente pesq pdir  | piso pfrente == Cola = 
                                                        if calcAtrito (piso pdir) < calcAtrito (piso pesq) 
                                                        then Movimenta D 
                                                        else Movimenta E
                                                    | ifRampa pfrente && piso pfrente == Boost = Acelera
                                                    | ifRampa pesq && piso pesq == Cola = Movimenta E
                                                    | ifRampa pdir && piso pdir == Cola = Movimenta D
                                                    | ifRampa pesq && ifRampa pdir && ifRampa pfrente == False = Acelera
                                                    | ifRampa pfrente && ifRampa pesq && ifRampa pdir == False = Movimenta D
                                                    | ifRampa pfrente && ifRampa pdir && ifRampa pesq == False = Movimenta E
                                                    | calcAtrito (piso pfrente) <= calcAtrito (piso pesq) && calcAtrito (piso pfrente) <= calcAtrito (piso pdir) = Acelera
                                                    | t>p && calcAtrito (piso pdir) < calcAtrito (piso pfrente) && calcAtrito (piso pdir) <= calcAtrito (piso pesq) = Movimenta D
                                                    | p>0 && calcAtrito (piso pesq) < calcAtrito (piso pfrente) && calcAtrito (piso pesq) < calcAtrito (piso pdir) = Movimenta E
                                                    | t==p && calcAtrito (piso pfrente) > calcAtrito (piso pesq) = Movimenta E
                                                    | p==0 && calcAtrito (piso pfrente) > calcAtrito (piso pdir) = Movimenta D
                                                                        where 
                                                                            t = length m 

-- | Função que recebe um estado de um Jogador no Ar e uma Peça, fazendo com que o jogador fique com uma inclinação que nao o faça "morrer" quando embater no chão
inclinar :: EstadoJogador -- Estado do Jogador que se encontra no Ar
            -> Peca -- Peça onde o jogador irá aterra
            -> Jogada -- Jogada mais indicada descoberta pelo robot
inclinar (Ar a i g) peca = if (i-(inclinacaoPecaAnterior peca)) >= 45 
                           then Movimenta B 
                           else
                             if (i-(inclinacaoPecaAnterior peca)) <= -45 
                             then Movimenta C 
                             else Acelera
inclinar _ peca = Acelera

-- | Função que recebe uma Peça e verifica se é uma Rampa ou não
ifRampa :: Peca -> Bool
ifRampa (Rampa _ _ _) = True
ifRampa _ = False

-- |
-- == Conclusão
-- |
-- |    Concluindo, pensamos que o trabalho cumpre os requisitos pedidos, sendo que o jogo ficou completamente funcional e com as dinâmicas propostas pelos professores da disciplina.
-- | Em suma, pensamos que este projeto nos ajudou bastante na introdução ao mundo da programação e que apesar de termos tido algumas dificuldades que não conseguimos ultrapassar da
-- | melhor forma, mais concretamente na realização dos gráficos da Tarefa 5, estamos satisfeitos com o resultado final e, acima de tudo, com a nossa evolução, pois nenhum de nós tinha
-- | experiência com programação

