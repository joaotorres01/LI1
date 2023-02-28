-- |
-- = Relatório do Projeto de Laboratórios de Informática I 2019
-- |
-- == Apresentação
-- |
-- |  O trabalho apresentado neste relatório foi desenvolvido no âmbito da disciplina Laboratórios de Informática I e foi realizado por Gonçalo Carvalho (a93260) e João Torres (a93231),
-- | alunos do 1º ano do curso de Mestrado Integrado em Engenharia Informática (MiEI) da Universidade do Minho. O objetivo do projeto era implementar um jogo 2D de corridas entre motas.
-- |  A ideia geral do jogo é percorrer a pista o mais rápido possível, realizando saltos e evitando obstáculos. O projeto foi dividido em 6 tarefas, nas quais tivemos que programar 
-- | a jogababilidade, as físicas, os gráficos e um bot inteligente.
-- |
-- == Introdução
-- | 
-- |  Para este projeto foi-nos proposto progamar um jogo inspirado no Excite Bike, jogo de 1984, em Haskell, um tipo de programação funcional.
-- |  Sendo que nenhum de nós tinha experiência em programação, tivemos como primeiro desafio o início da Tarefa 1, pois não sabiamos por onde, ou como, começar. Passado este
-- | primeiro problema, começamos a ganhar algum à vontade a programar e conseguimos resolver a primeira Tarefa com relativa facilidade. Ao iniciar a Tarefa 2 deparámo-nos com um 
-- | problema mais complexo e que exigia mais tempo, sendo que isto ficou explícito no número de linhas de código que tivemos nesta tarefa, cerca de 260. Quando chegamos à Tarefa 3 
-- | deparámo-nos com o nosso segundo grande obstáculo, pois para conseguir resolver de forma eficaz esta Tarefa era preciso uma experiência em programação que nós não tinhamos, sendo que 
-- | ficamos com uma nota mediana nessa Tarefa. Na 2ª fase do projeto tivemos como grande desafio a Tarefa 5, onde tinhamos que criar os gráficos do jogo, sendo que optamos por
-- | uns gráficos mais simples 
-- |
-- == Tarefa 1 
-- |  Nesta tarefa foi-nos pedido para progamar uma função que gerasse mapas aleatórios . A nossa abordagem baseou-se na criação de funções mais curtas que começavam por dividir
-- | os números gerados numa lista de pares, sendo que o primeiro correspondia ao tipo de Piso e o segundo ao tipo de Peça. Desta forma criamos uma função que ao receber um par dizia
-- | de que piso se tratava, sendo esta usada numa outra função que recebia o segundo número do par e dizia que tipo de Peça era, ficando assim com uma função que nos dava todas as Peças
-- | do mapa. Após isto criamos uma função que usava recursivamente as outras duas funções para gerar uma pista, sendo que esta foi usada recursivamente numa função final usada para
-- | gerar o Mapa.

module Tarefa1_2019li1g093 where


import LI11920
import System.Random
import Data.List.Split

-- * Testes

-- | Testes unitários da Tarefa 1.
--
-- Cada teste é um triplo (/número de 'Pista's/,/comprimento de cada 'Pista' do 'Mapa'/,/semente de aleatoriedades/).
testesT1 :: [(Int,Int,Int)]
testesT1 = [(1,2,3),(5,1,2),(8,4,8),(4,7,1),(6,2,7),(9,3,6),(1,99,45),(66,1,47),(38,91,32),(9,13,12),(4,9,15)]

-- * Funções pré-definidas da Tarefa 1.

geraAleatorios :: Int -> Int -> [Int]
geraAleatorios n seed = take n (randomRs (0,9) (mkStdGen seed))

-- * Funções principais da Tarefa 1.

gera :: Int -> Int -> Int -> Mapa
gera npistas comprimento semente = fazMapa (agrupa (geraAleatorios (2*(npistas*(comprimento-1)) ) semente)) npistas comprimento


--1)
-- a funçao 'agrupa' recebe uma lista de valores pseudo aleatórios com valores entre 0 e 9 e irá agrupa-los 2 a 2, pois cada peça necessita de 2 números para ser gerada
agrupa :: [Int] -- lista de valores pseudo aleatórios com valores entre 0 e 9
          -> [(Int,Int)] -- lista de valores pseudo aleatórios com valores entre 0 e 9 com os números agrupados 2 a 2
agrupa (h1:h2:t) = (h1,h2): agrupa t

-- a funçao 'tipoPiso' serve para gerar um 'Piso' para cada 'Peca' 
tipoPiso :: Int -- 1º número aleatório dos pares que a 'agrupa' gera
         -> Peca -- verifica o piso que será gerado para as rectas e para as rampas, visto que nas rampas tem de ser dada uma altura inicial e uma final, enquanto que nas rectas basta uma altura (pois a inicial é igual à final)
         -> Piso -- o 'Piso' que a função vai gerar para cada 'Peca'
tipoPiso x (Recta p num) = if x>=0 && x<=1 then Terra else
                                   if x>=2 && x<=3 then Relva else
                                   if x==4 then Lama else
                                   if x==5 then Boost else p
tipoPiso x (Rampa p num a) = if x>=0 && x<=1 then Terra else
                                   if x>=2 && x<=3 then Relva else
                                   if x==4 then Lama else
                                   if x==5 then Boost else p

-- funçao que retorna a altura final de uma determinada 'Peca'
alturaPeca :: Peca -- a 'Peca' que vai ser analisada
           -> Int -- a altura dessa 'Peca'
alturaPeca (Recta p a) = a 
alturaPeca (Rampa p num a) = a 

-- funçao que determina o tipo de uma peca com base na peca anteriror e no 2º número aleatório dos pares que a 'agrupa' gera
tipoPeca :: (Int,Int) -- par gerado pela agrupa
         -> Peca --peca anterior
         -> Peca --peca a ser gerada
tipoPeca (x,y) p = if y==0 || y==1 then Rampa (tipoPiso x p) (alturaPeca p) ((alturaPeca p)+y+1) else
                   if y>=2 && y<=5 then if ((alturaPeca p)-(y-1))>=0 then Rampa (tipoPiso x p) (alturaPeca p) ((alturaPeca p)-(y-1)) 
                                        else if alturaPeca p >0 then Rampa (tipoPiso x p) (alturaPeca p) 0 else Recta (tipoPiso x p) 0
                    else Recta (tipoPiso x p) (alturaPeca p)

--- funçao que gera uma pista
fazPista :: [(Int,Int)] -- lista obtida pela funçao gera
            -> Peca -- peca em que o jogador se encontra
            -> Bool -- bool que verifica se a peça é a primeira de cada pista (Recta Terra 0) e caso isto seja verdade faz com que esta peça nao seja construida
            -> Pista -- pista que será gerada
fazPista l p True = (Recta Terra 0) : (fazPista l p False)
fazPista [] p _ = []
fazPista (h:t) p b = (tipoPeca h p) : (fazPista t (tipoPeca h p) b)

-- funçao que gera um mapa 
fazMapa :: [(Int,Int)] -- lista obtida pela funçao gera
        -> Int -- numero de pistas
        -> Int --comprimento de cada pista
        -> Mapa -- mapa que será gerado
fazMapa _ 0 _ = []
fazMapa l npistas comprimento = (fazPista (take (comprimento-1) l) (Recta Terra 0) True) : (fazMapa (drop (comprimento-1) l) (npistas-1) comprimento)