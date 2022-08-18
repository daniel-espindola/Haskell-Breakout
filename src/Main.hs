module Main (main) where

import Graphics.Gloss.Interface.Pure.Game

larguraTela, alturaTela :: Float
larguraTela = 800
alturaTela = 600

larguraPlayer = 120
alturaPlayer = 30

larguraBola = 15

larguraParede = 50
alturaParede = 20

type Player = (Float, Float, Float) -- X, Y, VEL

type Bola = (Float, Float, Float, Float) -- X, Y, vx, vy

type Parede = (Float, Float) -- X, Y, vx, vy

type Mundo = (Player, Bola, [Parede])

inicializaMundo :: Mundo
inicializaMundo = (newPlayer, bola, paredes)
  where
    newPlayer = (0, -alturaTela/2 + 50, 0)
    bola = (0, 0, -200, 200)
    paredes = [(x * (larguraParede + 10), alturaTela/2 - 120 + (alturaParede + 5) * y) | x <- [-5 .. 5], y <- [-3 .. 3]]

desenhaPlayer :: Player -> Picture
desenhaPlayer (x, y, _) = color red $ translate x y $ rectangleSolid larguraPlayer alturaPlayer

desenhaBola :: Bola -> Picture
desenhaBola (x, y, _, _) = color green $ translate x y $ circleSolid larguraBola

desenhaParede :: Parede -> Picture
desenhaParede (px, py) = color yellow $ translate px py $ rectangleSolid larguraParede alturaParede

desenhaParedes :: [Parede] -> Picture
desenhaParedes ps = pictures $ map desenhaParede ps

desenhaMundo (player, bola, ps) = Pictures [playerPic, bolaPic, paredesPic]
  where
    playerPic = desenhaPlayer player
    bolaPic = desenhaBola bola
    paredesPic = desenhaParedes ps

inputHandler :: Event -> Mundo -> Mundo
inputHandler (EventKey (SpecialKey KeyRight) Down _ _) ((x, y, v), b, ps) = ((x, y, 200), b, ps)
inputHandler (EventKey (SpecialKey KeyRight) Up _ _) ((x, y, v), b, ps) = ((x, y, 0), b, ps)
inputHandler (EventKey (SpecialKey KeyLeft) Down _ _) ((x, y, v), b, ps) = ((x, y, -200), b, ps)
inputHandler (EventKey (SpecialKey KeyLeft) Up _ _) ((x, y, v), b, ps) = ((x, y, 0), b, ps)
inputHandler _ m = m

atualizaPosicaoPlayer :: Float -> Player -> Player
atualizaPosicaoPlayer dt (x, y, vel) = (xres, y, vel)
  where
    dx = vel * dt
    x1 = x + dx
    xres
      | x1 > larguraTela/2 - larguraPlayer/2 = larguraTela/2 - larguraPlayer/2
      | x1 < -larguraTela/2 + larguraPlayer/2 = -larguraTela/2 + larguraPlayer/2
      | otherwise = x1

colidiuPlayer (xp, yp) (xb, yb) =
  dentroH && dentroY
  where
    comecoYPlayer = yp + alturaPlayer/2 + larguraBola
    fimYPlayer = yp - alturaPlayer/2
    dentroH = xb > xp - larguraPlayer/2 && xb < xp + larguraPlayer/2
    dentroY = yb < comecoYPlayer && yb > fimYPlayer

atualizaPosicaoBola dt (p@(xp, yp, _), (x, y, vx, vy), ps) = (p, (x1, y1, vxRes, vyRes), ps)
  where
    dx = vx * dt
    dy = vy * dt
    x1 = x + dx
    y1 = y + dy
    cl = colidiuPlayer (xp, yp) (x, y)
    vxRes
      | (x1 > larguraTela / 2 - larguraBola/2) || (cl && vx < 0) = -200
      | (x1 < -larguraTela / 2 + larguraBola/2) || (cl && vx > 0) = 200
      | otherwise = vx
    vyRes
      | y1 > alturaTela / 2 - larguraBola/2 = -200
      | cl = 200
      | otherwise = vy

atualizaMundo :: Float -> Mundo -> Mundo
atualizaMundo dt (player, bola, paredes) = m2
  where
    m1 = (atualizaPosicaoPlayer dt player, bola, paredes)
    m2 = atualizaPosicaoBola dt m1

main :: IO ()
main = do

  play
    janela
    black
    60
    inicializaMundo
    desenhaMundo
    inputHandler
    atualizaMundo
  where
    janela = InWindow "Breakout" (800, 600) (10, 10)