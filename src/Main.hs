module Main (main) where

import Graphics.Gloss.Interface.Pure.Game
import System.Random (StdGen, newStdGen, randomR)

larguraTela, alturaTela, larguraPlayer, alturaPlayer, larguraBola, larguraParede, alturaParede :: Float
larguraTela = 800
alturaTela = 600
larguraPlayer = 120
alturaPlayer = 30
larguraBola = 15
larguraParede = 50
alturaParede = 20
velBola = 300
velPlayer = 500

type Player = (Float, Float, Float) -- X, Y, VEL

type Bola = (Float, Float, Float, Float) -- X, Y, vx, vy

type Parede = (Float, Float) -- X, Y

type Score = (Int, Int) -- Score, Highest

type Mundo = (Player, Bola, [Parede], Status, Score)

data Status =
    Start
  | Playing
  | Lose
  | Win
  deriving (Eq)

mundoStart :: Mundo
mundoStart = ((0, 0, 0), (0, 0, 0, 0), [], Start, (0, 0))

inicializaMundo :: Int -> Mundo
inicializaMundo maxSc =
    (newPlayer,
    bola,
    paredes,
    Playing,
    (0, maxSc))
  where
    newPlayer = (0, -alturaTela/2 + 30, 0)
    bola = (20, 0, -velBola, -velBola)
    paredes = [(x * (larguraParede + 10), alturaTela/2 - 120 + (alturaParede + 5) * y) | x <- [-6 .. 6], y <- [-3 .. 4]]


tamTitulo, posXTitulo, posYTitulo :: Float
tamTitulo = 0.25
posXTitulo = -350
posYTitulo = alturaTela / 2 - 100

desenhaTelaVitoria :: Score -> Picture
desenhaTelaVitoria (sc, max) =
  Pictures
    [ Color white $ Translate posXTitulo posYTitulo $ Scale (tamTitulo * 2) (tamTitulo * 2) $ Text "VOCE VENCEU!",
      Color white $ Translate posXTitulo (posYTitulo - 100) $ Scale tamTitulo tamTitulo $ Text ("SEU SCORE: " ++ show sc),
      Color white $ Translate posXTitulo (posYTitulo - 200) $ Scale tamTitulo tamTitulo $ Text ("HIGHEST SCORE: " ++ show max),
      Color white $ Translate posXTitulo (posYTitulo - 350) $ Scale tamTitulo tamTitulo $ Text "Aperte Espaco para jogar novamente"
    ]

desenhaTelaDerrota :: Score -> Picture
desenhaTelaDerrota (sc, max) =
  Pictures
    [ Color white $ Translate posXTitulo posYTitulo $ Scale (tamTitulo * 2) (tamTitulo * 2) $ Text "VOCE PERDEU!",
      Color white $ Translate posXTitulo (posYTitulo - 100) $ Scale tamTitulo tamTitulo $ Text ("SEU SCORE: " ++ show sc),
      Color white $ Translate posXTitulo (posYTitulo - 200) $ Scale tamTitulo tamTitulo $ Text ("HIGHEST SCORE: " ++ show max),
      Color white $ Translate posXTitulo (posYTitulo - 350) $ Scale tamTitulo tamTitulo $ Text "Aperte Espaco para jogar novamente"
    ]

desenhaSplashScreen :: Picture
desenhaSplashScreen =
  Pictures
    [ Color white $ Translate (posXTitulo + 70) posYTitulo $ Scale (tamTitulo * 2) (tamTitulo * 2) $ Text "Haskell Breakout!",
      Color white $ Translate (posXTitulo + 200) (posYTitulo - 100) $ Scale tamTitulo tamTitulo $ Text "Controles:",
      Color white $ Translate posXTitulo (posYTitulo - 200) $ Scale tamTitulo tamTitulo $ Text "'A'/'S' e Esquerda/Direita movem o jogador",
      Color white $ Translate posXTitulo (posYTitulo - 350) $ Scale tamTitulo tamTitulo $ Text "Aperte Espaco para comecar"
    ]

desenhaScore :: Score -> Picture
desenhaScore sc = Pictures [score, hs]
  where 
    score = Color white $ Translate 150 (-200) $ Scale 0.25 0.25 $ Text ("Score: " ++ show (fst sc))
    hs = Color white $ Translate 150 (-250) $ Scale 0.25 0.25 $ Text ("Record: " ++ show (snd sc))

desenhaPlayer :: Player -> Picture
desenhaPlayer (x, y, _) = color (makeColorI 0 177 255 255) $ translate x y $ rectangleSolid larguraPlayer alturaPlayer

desenhaBola :: Bola -> Picture
desenhaBola (x, y, _, _) = color white $ translate x y $ circleSolid larguraBola

clrDarkOrange, clrDarkYellow, clrDarkGreen, clrDarkRed :: Color
clrDarkOrange = makeColorI 170 126 0 255
clrDarkYellow = makeColorI 210 190 27 255
clrDarkGreen = makeColorI 0 180 70 255
clrDarkRed = makeColorI 180 0 0 255

obtemIndiceBlocosPorAltura :: Float -> Integer
obtemIndiceBlocosPorAltura y
  | y > 250 = 4
  | y > 200 = 3
  | y > 150 = 2
  | otherwise = 1

desenhaParede :: Parede -> Picture
desenhaParede (px, py) = color color1 $ translate px py $ rectangleSolid larguraParede alturaParede
  where
    i = obtemIndiceBlocosPorAltura py
    color1
      | i == 1 = clrDarkGreen
      | i == 2 = clrDarkYellow
      | i == 3 = clrDarkOrange
      | i == 4 = clrDarkRed
      | otherwise = clrDarkRed

desenhaParedes :: [Parede] -> Picture
desenhaParedes ps = pictures $ map desenhaParede ps

desenhaMundo :: Mundo -> Picture
desenhaMundo (player, bola, ps, status, score) =
  case status of
    Start -> desenhaSplashScreen
    Playing -> Pictures [playerPic, bolaPic, paredesPic, scorePic]
    Lose -> desenhaTelaDerrota  score
    Win -> desenhaTelaVitoria score
  where
    playerPic = desenhaPlayer player
    bolaPic = desenhaBola bola
    paredesPic = desenhaParedes ps
    scorePic = desenhaScore score

atualizaPosicaoPlayer :: Float -> Player -> Player
atualizaPosicaoPlayer dt (x, y, vel) = (xres, y, vel)
  where
    dx = vel * dt
    x1 = x + dx
    xres
      | x1 > larguraTela/2 - larguraPlayer/2 = larguraTela/2 - larguraPlayer/2
      | x1 < -larguraTela/2 + larguraPlayer/2 = -larguraTela/2 + larguraPlayer/2
      | otherwise = x1

colidiuPlayer :: (Float, Float) -> (Float, Float) -> Bool
colidiuPlayer (xp, yp) (xb, yb) =
  dentroX && dentroY
  where
    comecoYPlayer = yp + alturaPlayer/2 + larguraBola
    fimYPlayer = yp - alturaPlayer/2
    dentroX = xb > xp - larguraPlayer / 2 && xb < xp + larguraPlayer / 2
    dentroY = yb < comecoYPlayer && yb > fimYPlayer

colidiuParede :: (Float, Float) -> (Float, Float) -> Bool
colidiuParede (xp, yp) (xb, yb) =
  dentroX && dentroY
  where
    comecoYParede = yp + alturaParede
    fimYParede = yp - alturaParede
    dentroX = xb > xp - larguraParede / 2 && xb < xp + larguraParede / 2
    dentroY = yb < comecoYParede && yb > fimYParede

filtraParedesColisao :: (Float, Float) -> [Parede] -> [Parede]
filtraParedesColisao (xb, yb) = filter (not . colidiuParede (xb, yb))

obtemIncrementoScore :: Float -> Int
obtemIncrementoScore y = addScore
  where
    i = obtemIndiceBlocosPorAltura y
    addScore
      | i == 1 = 100
      | i == 2 = 200
      | i == 3 = 300
      | i == 4 = 400
      | otherwise = 400

atualizaScore :: Mundo -> Mundo -> Mundo
atualizaScore m1@(_, _, ps1, _, _) m2@(p, b@(x, y, vx, vy), ps2, s, score) =  (p, b, ps2, s, (scoreRes, maxRes))
  where
    (sc, max) = score
    diffBlocos = length ps1 - length ps2
    incremento = if diffBlocos > 0
                  then obtemIncrementoScore (y+25) * diffBlocos
                 else
                  0
    scoreRes = sc + incremento
    maxRes = if scoreRes > max then scoreRes else max


{-
  Recebe o mundo do jogo e atualiza a posição da bola
  Verifica se a bola colidiu com o player e atualiza suas posições de acordo
  Verifica se a bola atingiu algum bloco, deletando o bloco e incrementando a pontuação
-}
atualizaPosicaoBola :: Float -> Mundo -> Mundo
atualizaPosicaoBola dt (p@(xp, yp, _), (x, y, vx, vy), ps, s, score) = (p, (x1, yRes, vxRes, vyRes), paredesRes, s, score)
  where
    dx = vx * dt
    dy = vy * dt
    x1 = x + dx
    y1 = y + dy
    clPlayer = colidiuPlayer (xp, yp) (x1, y1)
    paredesRes = filtraParedesColisao (x1, y1) ps
    clParede = length paredesRes < length ps
    clRes = clPlayer || clParede
    velBolaIncremento = if clParede then 2 else 0
    vxRes
      | (x1 > larguraTela / 2 - larguraBola/2) || (clRes && vx < 0) = - velBola - velBolaIncremento
      | (x1 < -larguraTela / 2 + larguraBola/2) || (clRes && vx > 0) = velBola + velBolaIncremento
      | otherwise = vx
    vyRes
      | (y1 > alturaTela / 2 - larguraBola/2) || clRes && vy > 0  = -velBola -velBolaIncremento
      | clRes && vy < 0 = velBola + velBolaIncremento
      | otherwise = vy
    yRes
      | clParede && vy > 0 = y1 - 5
      | clParede && vy < 0 = y1 + 5
      | otherwise = y1

atualizaMundo :: Float -> Mundo -> Mundo
atualizaMundo dt (player, bola, paredes, s, score) = m4
  where
    m1 = (atualizaPosicaoPlayer dt player, bola, paredes, s, score)
    m2 = atualizaPosicaoBola dt m1
    m3 = atualizaStatusGame m2
    m4 = atualizaScore m1 m3

{-
  Recebe o mundo do jogo e atualiza ele a depender do Status
  , mudar para a tela de vitória caso acabem os inimigos
-}
atualizaStatusGame :: Mundo -> Mundo
atualizaStatusGame m@(p, b@(x, y, vx, vy), ps, s, sc) = mundoRes
  where
    novoMundo = inicializaMundo $ snd sc
    mundoRes
      | s == Playing && null ps = (p, b, ps, Win, sc)
      | s == Playing && (y < -alturaTela/2 - 100) = (p, b, ps, Lose, sc)
      | otherwise = m

inputHandler :: Event -> Mundo -> Mundo
inputHandler (EventKey (SpecialKey KeyRight) Down _ _) ((x, y, v), b, ps, s, sc) = ((x, y, velPlayer), b, ps, s, sc)
inputHandler (EventKey (SpecialKey KeyRight) Up _ _) ((x, y, v), b, ps, s, sc) = ((x, y, 0), b, ps, s, sc)
inputHandler (EventKey (SpecialKey KeyLeft) Down _ _) ((x, y, v), b, ps, s, sc) = ((x, y, -velPlayer), b, ps, s, sc)
inputHandler (EventKey (SpecialKey KeyLeft) Up _ _) ((x, y, v), b, ps, s, sc) = ((x, y, 0), b, ps, s, sc)
inputHandler (EventKey (SpecialKey KeySpace) Up _ _) ((x, y, v), b, ps, s, sc) =
  case s of
    Playing -> ((x, y, 0), b, ps, s, sc)
    _ -> inicializaMundo $ snd sc
inputHandler (EventKey (Char 'a') Down _ _) ((x, y, v), b, ps, s, sc) = ((x, y, -velPlayer), b, ps, s, sc)
inputHandler (EventKey (Char 'a') Up _ _) ((x, y, v), b, ps, s, sc) = ((x, y, 0), b, ps, s, sc)
inputHandler (EventKey (Char 'd') Down _ _) ((x, y, v), b, ps, s, sc) = ((x, y, velPlayer), b, ps, s, sc)
inputHandler (EventKey (Char 'd') Up _ _) ((x, y, v), b, ps, s, sc) = ((x, y, 0), b, ps, s, sc)
inputHandler _ m = m

main :: IO ()
main = do
  rg <- newStdGen

  play
    janela
    black
    60
    mundoStart
    desenhaMundo
    inputHandler
    atualizaMundo
  where
    janela = InWindow "Breakout" (800, 600) (10, 10)