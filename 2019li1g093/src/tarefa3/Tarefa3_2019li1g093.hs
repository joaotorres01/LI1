-- == Tarefa 3
-- | 	Nesta tarefa era pedido que fizessemos uma função que permitisse "poupar" passos ao construir o mapa, ou seja, identificar padrões no mapa, peças iguais em posições próximas,
-- | para que o número de instruções necessárias para construir o mapa fosse o menor possível. Primeiramente, começamos por identificar o mapa Peça a Peça, de modo a ganhar uma base
-- | para o resto da Tarefa. Findo este passo, optamos por tentar identificar padrões horizontais no mapa, ou seja, se houvesse Peças iguais de forma consecutiva seria apenas precisa
-- | uma instrução para as "construir". Como sugerido pelos professores, tentamos depois identificar padrões verticais, tentar dar a mesma instrução para construir Peças iguais de 
-- | pistas diferentes com distância igual. Aqui tivemos um problema pois não conseguimos criar uma função que efetuasse de forma eficaz este objetivo, ou seja, não diminuíamos o 
-- | número de instruções com esta função. Também nos foi proposto tentar identificar padrões verticais desfasados, ou seja, tentar idfentificar um conjunto de Peças iguais em pistas
-- | diferentes, não tendo que estar em distâncias iguais, de modo a tentar diminuir as instruções, sendo que também não arranjamos uma solução eficaz para esta proposta. Dados estes
-- | problemas decidimos melhorar ao máximo a identificação de padrões horizontais, atingindo assim uma taxa de compressão de 27%. A maior dificuldade desta Tarefa foi o facto de não
-- | haver um rumo certo a seguir, ou seja, mesmo conseguindo "construir" o mapa, não teríamos a pontuação toda, sendo precisa uma capacidade/experiência de programação que não tinhamos

module Tarefa3_2019li1g093 where

import LI11920
import Constroi
import Tarefa1_2019li1g093
import Tarefa0_2019li1g093
import Data.List
--pista p intrucao, mapa p instrucao, peca p instrucao

-- * Testes

-- | Testes unitários da Tarefa 3.
--
-- Cada teste é um 'Mapa'.
testesT3 :: [Mapa]
testesT3 = [(gera 2 5 1), (gera 1 2 3), (gera 5 100 2), (gera 8 4 8), (gera 4 7 1), (gera 6 2 7), (gera 9 3 6), (gera 1 99 45), (gera 66 3 47), (gera 38 91 32), (gera 9 13 12), (gera 4 9 15)]
-- * Funções principais da Tarefa 3.

-- | Desconstrói um 'Mapa' numa sequência de 'Instrucoes'.
--
-- __NB:__ Uma solução correcta deve retornar uma sequência de 'Instrucoes' tal que, para qualquer mapa válido 'm', executar as instruções '(desconstroi m)' produza o mesmo mapa 'm'.
--
-- __NB:__ Uma boa solução deve representar o 'Mapa' dado no mínimo número de 'Instrucoes', de acordo com a função 'tamanhoInstrucoes'.

-- funçao que descontroi um mapa e o transforma em instruções
desconstroi :: Mapa -- 'Mapa' gerado  
            -> Instrucoes -- instruçoes para o qual o mapa é transformado
desconstroi (h:t) = desfazMapa (h:t) 0 

-- | funçao que transforma uma peça numa Instruçao
desfazPeca :: Peca -- peca que vai ser convertida numa 'Instrucao'
            -> [Int]--numero da pista em que a peca se encontra
            -> Instrucao --instruçao para o qual a peça é transformada
desfazPeca (Recta p hi) [n] = Anda [n] p
desfazPeca (Rampa p hi hf) [n] = if hf > hi then Sobe [n] p (hf-hi) else Desce [n] p (abs (hf-hi)) 
     
-- | funçao que transforma uma Pista em Instruçoes                            
desfazPista:: Pista --pista que vai ser convertida em 'Intrucoes'
            -> Int --numero da pista que se vai desfazer 
            -> Instrucoes --instruçoes para o qual a pista é transformada
desfazPista [] _ = []
desfazPista (h:t) x = desfazPeca h [x]: desfazPista t x  


-- | funçao que transforma um mapa inteiro em instruçoes
desfazMapa:: Mapa -- Mapa do jogo
            -> Int --número da primeira pista a construir
            -> Instrucoes -- instruçoes para o qual o mapa é transformado
desfazMapa [] _ = []
desfazMapa (x:xs) c = repetir (tail x) c ++ desfazMapa xs (c+1) 

-- | funçao que tem em conta os padrões horizontais de uma Pista e reduz o número de instruções
repetir :: Pista -- pista que se quer construir
           -> Int --numero da pista
           -> Instrucoes -- instruçoes tendo em conta os padroes horizontais da pista, de forma a reduzir o numero de instruçoes
repetir [] _ = []
repetir [p] x = desfazPista [p] x
repetir (h:t:ts) x = if length (takeWhile(==h) (t:ts)) > length [] then (Repete (length (h:takeWhile(==h) (t:ts))) (desfazPista [h] x)) : (repetir (dropWhile(==h) (t:ts)) x) else (desfazPeca h [x]):(repetir (t:ts) x)


