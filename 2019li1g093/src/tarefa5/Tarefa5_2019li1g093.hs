-- == Tarefa 5
-- |    Aqui o objetivo era criar gráficos originais para a nossa versão do Excite Bike, desde as Peças ao Jogador. Aqui nós deparámo-nos com o maior obstáculo do projeto todo, já que
-- | tivemos que aprender, de uma forma autodidata, uma extensão desta linguagem. A nossa abordagem para a realização desta tarefa foi de fazer gráficos mais minimalistas e começar
-- | por tentar fazer o mínimo, ou seja, começar por "desenhar" apenas a pista com o Jogador. Para conseguirmos atingir este nosso primeiro objetivo começamos por procurar imagens que
-- | achassemos que correspondiam aos pisos disponíveis. Finda esta procura de imagens, tivemos de dimensionar todas as imagens para o mesmo tamanho, para que todas as peças ocupassem
-- | o mesmo número de pixeis. Após este processo, começamos a trabalhar na correspondência das imagens para quando a Peça era uma rampa, sendo que optamos por recorrer à biblioteca
-- | Juicy disponibilizada pelos professores, mais concretamente, usando as funções sobeDynamicImage, desceDynamicImage e fromDynamicImage, sendo que nos permitiram dimensionar 
-- | automaicamente as rampas, tendo em conta a sua inclinação. De seguida, preocupámo-nos em arranjar uma imagem para o nosso Jogador, sendo que depois de encontrada, também esta
-- | teve que ser dimensionada e recortada. Após terminarmos esta primeira fase da Tarefa, decidimos tentar fazer um menu que permitisse ao Jogador, decidir quando arrancar para a 
-- | corrida e ver primeiro quais os comandos para controlar o Jogador. Para cumprirmos este segundo objetivo da Tarefa tivemos que arranjar imagens para corresponder aos menus de inicio
-- | e dos Comandos, sendo que foi necessário recorrer a editores de imagem para redimensionar e alterar conforme o nosso gosto pessoal as fotos dos menus. Para fazer os menus tivemos
-- | que criar um novo tipo de informação, denominado Menu, que diferenciava se estavamos no Menu de Inicio, no Menu dos Comandos ou no Menu de Jogo.

module Main where

import LI11920
import Tarefa1_2019li1g093
import Tarefa2_2019li1g093
import Tarefa4_2019li1g093
import Tarefa0_2019li1g093
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.Pure.Game
import Juicy
import Codec.Picture.Types as Juicy
import Codec.Picture as Juicy
import Graphics.Gloss
import Graphics.Gloss.Juicy 


-- | Estado prefefinido do Jogo
estadoInicial :: Estado 
estadoInicial = Estado 
                (gera 4 15 48)
                [Jogador 0 0 0 4 (Chao False)]

-- | Função que lê o Estado do Jogo e na lista de imagens e desenha a interface do jogo 
desenhaEstado :: Estado -> [DynamicImage] -> [Picture]
desenhaEstado (Estado m ((Jogador p d v c e):_)) [terra,lama,boost,cola,relva,jogador] =
   (desenhaMapa m  (-400) (0) [terra,lama,boost,cola,relva]) ++ [(desenhaJog (Jogador p d v c e) ((realToFrac (d*60)-400)) (realToFrac(altJogador (Jogador p d v c e) patual)*60) [jogador])]
                                                            where
                                                                patual = encontraPosicaoMatriz (p,dist) m
                                                                dist = floor d


-- | Função que recebe um Jogador e a peça onde ele está e nos diz a que altura se encontra
altJogador :: Jogador -> Peca -> Double
altJogador (Jogador p d v c (Chao b)) (Recta piso h) = fromIntegral h
altJogador (Jogador p d v c (Chao b)) (Rampa piso hi hf) = altPeca d (Rampa piso hi hf)
altJogador (Jogador p d v c (Ar a i g)) peca = a
altJogador (Jogador p d v c (Morto t)) (Recta piso h) = fromIntegral h
altJogador (Jogador p d v  c (Morto t)) (Rampa piso hi hf) = altPeca d (Rampa piso hi hf)

                                                         
-- | Função que recebe um jogador e o desenha no mapa
desenhaJog :: Jogador -> Float -> Float -> [DynamicImage] -> Picture
desenhaJog jog x y (jogador:_) = Pictures [imag]
                                where
                                    imag = (Translate x y jogad)
                                    jogad = (Pictures [jogDynamic])
                                    Just jogDynamic = fromDynamicImage jogador


-- | Função que desenha todas as pistas, formando assim o mapa
desenhaMapa :: Mapa -> Float -> Float -> [DynamicImage] -> [Picture]
desenhaMapa (h:t) x y imagens = (desenhaPista h x y imagens) ++ (desenhaMapa t x (y-60) imagens)
desenhaMapa [] _ _  _ = []

-- | Função que desenha todas as Peças de uma pista
desenhaPista :: Pista -> Float -> Float -> [DynamicImage] -> [Picture]
desenhaPista (h:t) x y imagens = (desenhaPeca h x y imagens) : (desenhaPista t (x+60) y imagens)
desenhaPista [] _ _ _ = []                            

-- | Função que recebe uma Peça e vê qual a imagem que lhe corresponde
desenhaPeca :: Peca -> Float -> Float -> [DynamicImage] -> Picture
desenhaPeca (Recta p h) x y (terra:lama:boost:cola:relva:_) = 
    let textura  
            | p==Terra = terra
            | p==Lama = lama
            | p==Boost = boost
            | p==Cola = cola
            | otherwise = relva
        Just text = fromDynamicImage textura 
    in Translate x (y+(fromIntegral (h*60))) text
desenhaPeca (Rampa p hi hf) x y (terra:lama:boost:cola:relva:_) = 
       															 let
       															 	textura 
            																| p==Terra = terra
            																| p==Lama = lama
            																| p==Boost = boost
            																| p==Cola = cola
           															 		| otherwise = relva
       															 	textura' = if hf>hi then sobeDynamicImage (hf-hi) textura else desceDynamicImage (hi-hf) textura
        															Just text = fromDynamicImage textura' 
        														in if hi>hf 
       															   then Translate x (y+(fromIntegral (hf*30) + fromIntegral(hi*30))) text 
       															   else Translate x (y+(fromIntegral (hi*30) + fromIntegral(hf*30))) text



-- | Função que faz com que o EstadoGloss que se altere quando certas teclas são pressionadas
reageEvento :: Event -> EstadoGloss -> EstadoGloss
reageEvento (EventKey (Char 'w')    Down _ _) (e,Jogo,imagens)  = (jogada 0 Acelera e,Jogo,imagens)
reageEvento (EventKey (Char 's')    Down _ _) (e,Jogo,imagens)  = (jogada 0 Desacelera e,Jogo,imagens)
reageEvento (EventKey (Char 'a')    Down _ _) (e,Jogo,imagens)  = (jogada 0 (Movimenta E) e,Jogo,imagens) 
reageEvento (EventKey (Char 'd')    Down _ _) (e,Jogo,imagens)  = (jogada 0 (Movimenta D) e,Jogo,imagens) 
reageEvento (EventKey (Char 'c')    Down _ _) (e,Jogo,imagens)  = (jogada 0 (Dispara) e,Jogo,imagens)
reageEvento (EventKey (SpecialKey KeyEnter)    Down _ _) (e,Inicio,imagens) = (estadoInicial,Jogo,imagens)
reageEvento (EventKey (SpecialKey KeyEnter)    Down _ _) (e,Comandos,imagens) = (e,Comandos,imagens)
reageEvento (EventKey (SpecialKey KeyDown)    Down _ _) (e,Inicio,imagens) = (e,Comandos,imagens)
reageEvento (EventKey (SpecialKey KeyUp)    Down _ _) (e,Comandos,imagens) = (e,Inicio,imagens)
reageEvento (EventKey (SpecialKey KeySpace)    Down _ _) (e,menu,imagens) = (e,Inicio,imagens)
reageEvento _ e = e -- ignora qualquer outro evento

-- | Função que faz com que o Estado de Jogo reaja ao longo do tempo
reageTempo :: Float -> Estado -> Estado 
reageTempo t e = e


type EstadoGloss = (Estado,Menu,[DynamicImage])

data Menu = Inicio | Comandos | Jogo  

-- | EstadoGloss predefinido do jogo
estadoGlossInicial :: [DynamicImage] -> EstadoGloss
estadoGlossInicial imagens = (estadoInicial,Inicio,imagens)

-- | FrameRate predefinida
fr :: Int
fr = 50

-- | Display predefinido
dm :: Display
dm = FullScreen
--dm = InWindow "Novo Jogo" (400, 400) (0, 0)

-- | Função que faz com que o EstadoGloss reaja com o passar do tempo
reageTempoGloss :: Float -> EstadoGloss -> EstadoGloss
reageTempoGloss time ((Estado m (h:t)),menu,imagens) = ((Estado m (aplicaPasso (Estado m (h:t)) time)),menu,imagens)

-- | Função que faz com que o EstadoGloss se altere quando certas teclas são pressionadas
reageEventoGloss :: Event -> EstadoGloss -> EstadoGloss
reageEventoGloss ev (e,menu,imagens) = reageEvento ev (e,menu,imagens)

-- | Função que recebe um Estado e o tempo e aplica a todos os jogadores desse estado a função passo
aplicaPasso :: Estado -> Float -> [Jogador]
aplicaPasso (Estado m (h:t)) time  = (passo (realToFrac time) m h) : (aplicaPasso (Estado m t) time)
aplicaPasso (Estado m []) time = []

-- | Função que recebe um EstadoGloss e "desenha" no ecrã o correspondente a esse EstadoGloss
desenhaEstadoGloss :: EstadoGloss -> Picture
desenhaEstadoGloss (e,menu,(fundoMenu:jogar:comandos:fundo:t)) = 
    let
    	estado = (Pictures [menuDynamic])
    	Just menuDynamic = fromDynamicImage fundoMenu
    	jogarImag = (Pictures [jogarDynamic])
    	Just jogarDynamic = fromDynamicImage jogar
    	comandosImag = (Pictures [comandosDynamic])
    	Just comandosDynamic = fromDynamicImage comandos
    	fundo2 = (Pictures [fundoDynamic])
    	Just fundoDynamic = fromDynamicImage fundo
    in if ifInicio menu then Pictures [estado] else
    	if ifComandos menu then Pictures [comandosImag] 
    	else Pictures ([fundo2]++(desenhaEstado e t))
            
-- | Função que diz se o Menu é o menu de Início                      
ifInicio :: Menu -> Bool
ifInicio Inicio = True
ifInicio _ = False

-- | Função que diz se o Menu é o menu de Comandos
ifComandos :: Menu -> Bool
ifComandos Comandos = True
ifComandos _ = False

-- | Função que faz com que seja possivel testar os gráficos
main :: IO ()
main = do 
    Right terra <- readImage "../texturas/terra.png"
    Right lama <- readImage "../texturas/lama.png"
    Right boost <- readImage "../texturas/boost.png"
    Right cola <- readImage "../texturas/cola.png"
    Right relva <- readImage "../texturas/relva.png"
    Right fundo <- readImage "../texturas/fundo.png"
    Right jogador <- readImage "../texturas/jogador.png"
    Right fundoMenu <- readImage "../texturas/fundomenu.png"
    Right jogar <- readImage "../texturas/jogar.png"
    Right comandos <- readImage "../texturas/comandos.png"
    play dm
        black
        fr
        (estadoGlossInicial [fundoMenu,jogar,comandos,fundo,terra,lama,boost,cola,relva,jogador])
        desenhaEstadoGloss
        reageEventoGloss
        reageTempoGloss