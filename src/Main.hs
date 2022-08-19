module Main (main) where

import Graphics.Gloss.Interface.Pure.Game

larguraTela, alturaTela, larguraPlayer, alturaPlayer, larguraBola, larguraParede, alturaParede :: Float
larguraTela = 800
alturaTela = 600
larguraPlayer = 120
alturaPlayer = 30
larguraBola = 15
larguraParede = 50
alturaParede = 20
velBola = 250
velPlayer = 300

type Player = (Float, Float, Float) -- X, Y, VEL

type Bola = (Float, Float, Float, Float) -- X, Y, vx, vy

type Parede = (Float, Float) -- X, Y

type Mundo = (Player, Bola, [Parede], Status)

data Status =
    Start
  | Playing
  | Lose
  | Win
  deriving (Eq)

mundoStart :: Mundo
mundoStart = ((0, 0, 0), (0, 0, 0, 0), [], Start)

inicializaMundo :: Mundo
inicializaMundo =
    (newPlayer,
    bola,
    paredes,
    Playing)
  where
    newPlayer = (-50, -alturaTela/2 + 50, 0)
    bola = (20, 0, -velBola, -velBola)
    paredes = [(x * (larguraParede + 10), alturaTela/2 - 120 + (alturaParede + 5) * y) | x <- [-5 .. 5], y <- [-3 .. 3]]


tamTitulo, posXTitulo, posYTitulo :: Float
tamTitulo = 0.25
posXTitulo = -350
posYTitulo = alturaTela / 2 - 100

desenhaTelaVitoria :: Picture
desenhaTelaVitoria =
  Pictures
    [ Color white $ Translate posXTitulo posYTitulo $ Scale (tamTitulo * 2) (tamTitulo * 2) $ Text "YOU WIN!",
      Color white $ Translate posXTitulo (posYTitulo - 100) $ Scale tamTitulo tamTitulo $ Text "SCORE: 123450 !!",
      Color white $ Translate posXTitulo (posYTitulo - 200) $ Scale tamTitulo tamTitulo $ Text "HIGHEST SCORE: 99999999999999",
      Color white $ Translate posXTitulo (posYTitulo - 350) $ Scale tamTitulo tamTitulo $ Text "Aperte Espaco para jogar novamente"
    ]

desenhaTelaDerrota :: Picture
desenhaTelaDerrota =
  Pictures
    [ Color white $ Translate posXTitulo posYTitulo $ Scale (tamTitulo * 2) (tamTitulo * 2) $ Text "YOU LOSE!",
      Color white $ Translate posXTitulo (posYTitulo - 100) $ Scale tamTitulo tamTitulo $ Text "SCORE: 123450 !!",
      Color white $ Translate posXTitulo (posYTitulo - 200) $ Scale tamTitulo tamTitulo $ Text "HIGHEST SCORE: 99999999999999",
      Color white $ Translate posXTitulo (posYTitulo - 350) $ Scale tamTitulo tamTitulo $ Text "Aperte Espaco para jogar novamente"
    ]

desenhaSplashScreen :: Picture
desenhaSplashScreen =
  Pictures
    [ Color white $ Translate (posXTitulo + 70) posYTitulo $ Scale (tamTitulo * 2) (tamTitulo * 2) $ Text "Haskell Breakout!",
      Color white $ Translate (posXTitulo + 200) (posYTitulo - 100) $ Scale tamTitulo tamTitulo $ Text "Controles:",
      Color white $ Translate posXTitulo (posYTitulo - 200) $ Scale tamTitulo tamTitulo $ Text "'A'/'S' e Esquerda/Direita movem o jogador",
      Color white $ Translate posXTitulo (posYTitulo - 250) $ Scale tamTitulo tamTitulo $ Text "Espaco Atira",
      Color white $ Translate posXTitulo (posYTitulo - 350) $ Scale tamTitulo tamTitulo $ Text "Aperte Espaco para comecar"
    ]

desenhaPlayer :: Player -> Picture
desenhaPlayer (x, y, _) = color red $ translate x y $ rectangleSolid larguraPlayer alturaPlayer

desenhaBola :: Bola -> Picture
desenhaBola (x, y, _, _) = color green $ translate x y $ circleSolid larguraBola

desenhaParede :: Parede -> Picture
desenhaParede (px, py) = color yellow $ translate px py $ rectangleSolid larguraParede alturaParede

desenhaParedes :: [Parede] -> Picture
desenhaParedes ps = pictures $ map desenhaParede ps

desenhaMundo :: Mundo -> Picture
desenhaMundo (player, bola, ps, status) =
  case status of
    Start -> desenhaSplashScreen
    Playing -> Pictures [playerPic, bolaPic, paredesPic]
    Lose -> desenhaTelaDerrota
    Win -> desenhaTelaVitoria
  where
    playerPic = desenhaPlayer player
    bolaPic = desenhaBola bola
    paredesPic = desenhaParedes ps

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

atualizaPosicaoBola :: Float -> Mundo -> Mundo
atualizaPosicaoBola dt (p@(xp, yp, _), (x, y, vx, vy), ps, s) = (p, (x1, yRes, vxRes, vyRes), paredesRes, s)
  where
    dx = vx * dt
    dy = vy * dt
    x1 = x + dx
    y1 = y + dy
    parede = head ps
    (paredeX, paredeY) = parede
    clPlayer = colidiuPlayer (xp, yp) (x1, y1)
    clParede = colidiuParede (paredeX, paredeY) (x1, y1)
    clRes = clPlayer || clParede
    vxRes
      | (x1 > larguraTela / 2 - larguraBola/2) || (clRes && vx < 0) = -velBola
      | (x1 < -larguraTela / 2 + larguraBola/2) || (clRes && vx > 0) = velBola
      | otherwise = vx
    vyRes
      | y1 > alturaTela / 2 - larguraBola/2 = -velBola
      | clRes && vy > 0 = -velBola
      | clRes && vy < 0 = velBola
      | otherwise = vy
    yRes
      | clParede && vy > 0 = y1 - 5
      | clParede && vy < 0 = y1 + 5
      | otherwise = y1
    paredesRes = filtraParedesColisao (x1, y1) ps

atualizaMundo :: Float -> Mundo -> Mundo
atualizaMundo dt (player, bola, paredes, s) = m3
  where
    m1 = (atualizaPosicaoPlayer dt player, bola, paredes, s)
    m2 = atualizaPosicaoBola dt m1
    m3 = atualizaStatusGame m2

atualizaStatusGame :: Mundo -> Mundo
atualizaStatusGame (p, b@(x, y, vx, vy), ps, s) = mundoRes
  where
    novoMundo = inicializaMundo
    mundoRes
      | s == Playing && null ps = (p, b, ps, Win)
      | s == Playing && (y < -alturaTela/2 - 100) = (p, b, ps, Lose)
      | otherwise = (p, b, ps, s)

inputHandler :: Event -> Mundo -> Mundo
inputHandler (EventKey (SpecialKey KeyRight) Down _ _) ((x, y, v), b, ps, s) = ((x, y, velPlayer), b, ps, s)
inputHandler (EventKey (SpecialKey KeyRight) Up _ _) ((x, y, v), b, ps, s) = ((x, y, 0), b, ps, s)
inputHandler (EventKey (SpecialKey KeyLeft) Down _ _) ((x, y, v), b, ps, s) = ((x, y, -velPlayer), b, ps, s)
inputHandler (EventKey (SpecialKey KeyLeft) Up _ _) ((x, y, v), b, ps, s) = ((x, y, 0), b, ps, s)
inputHandler (EventKey (SpecialKey KeySpace) Up _ _) ((x, y, v), b, ps, s) =
  case s of
    Start -> inicializaMundo
    Lose -> inicializaMundo
    Win -> inicializaMundo
    _ -> ((x, y, 0), b, ps, s)
inputHandler (EventKey (Char 'a') Down _ _) ((x, y, v), b, ps, s) = ((x, y, velPlayer), b, ps, s)
inputHandler (EventKey (Char 'a') Up _ _) ((x, y, v), b, ps, s) = ((x, y, 0), b, ps, s)
inputHandler (EventKey (Char 'd') Down _ _) ((x, y, v), b, ps, s) = ((x, y, -velPlayer), b, ps, s)
inputHandler (EventKey (Char 'd') Up _ _) ((x, y, v), b, ps, s) = ((x, y, 0), b, ps, s)
inputHandler _ m = m

main :: IO ()
main = do
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