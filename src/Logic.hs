module Logic where

import Graphics.Gloss.Interface.Pure.Game
import qualified SDL
import qualified SDL.Mixer as Mix

data Sounds = Sounds
  { frPdlBounce :: Mix.Chunk    -- ^ Sound of ball hitting paddle front.
  , sdPdlBounce :: Mix.Chunk    -- ^ Sound of ball hitting paddle top/bottom.
  , topWallBounce :: Mix.Chunk  -- ^ Sound of ball hitting top wall.
  , btmWallBounce :: Mix.Chunk  -- ^ Sound of ball hitting bottom wall.
  , begin :: Mix.Chunk          -- ^ Sound when play begins.
  , bkgndMusic :: Mix.Chunk     -- ^ Background music.
  , defeat :: Mix.Chunk         -- ^ Game over (defeat) sound.
  , victory :: Mix.Chunk        -- ^ Game over (victory) sound.
  } deriving (Show, Eq)

larguraTela, alturaTela, larguraPlayer, alturaPlayer, larguraBola, larguraParede, alturaParede :: Float
larguraTela = 800
alturaTela = 600
larguraPlayer = 120
alturaPlayer = 30
larguraBola = 15
larguraParede = 50
alturaParede = 20
velBola = 300

type Player = (Float, Float, Float) -- X, Y, VEL

type Bola = (Float, Float, Float, Float) -- X, Y, vx, vy

type Parede = (Float, Float) -- X, Y

type Score = (Int, Int) -- Score, Highest

type Mundo = (Player, Bola, [Parede], Status, Score, Sounds)

data Status =
    Start
  | Playing
  | Lose
  | Win
  deriving (Eq)

mundoStart :: Sounds -> Mundo
mundoStart snd = ((0, 0, 0), (0, 0, 0, 0), [], Start, (0, 0), snd)

inicializaMundo :: Int -> Sounds -> Mundo
inicializaMundo maxSc sounds =
    (newPlayer,
    bola,
    paredes,
    Playing,
    (0, maxSc),
    sounds)
  where
    newPlayer = (0, -alturaTela/2 + 30, 0)
    bola = (20, 0, -velBola, -velBola)
    paredes = [(x * (larguraParede + 10), alturaTela/2 - 120 + (alturaParede + 5) * y) | x <- [-6 .. 6], y <- [-3 .. 4]]

obtemIndiceBlocosPorAltura :: Float -> Integer
obtemIndiceBlocosPorAltura y
  | y > 250 = 4
  | y > 200 = 3
  | y > 150 = 2
  | otherwise = 1

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
atualizaScore m1@(_, _, ps1, _, _m, _) m2@(p, b@(x, y, vx, vy), ps2, s, score, snd2) =  (p, b, ps2, s, (scoreRes, maxRes), snd2)
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
atualizaPosicaoBola dt (p@(xp, yp, _), (x, y, vx, vy), ps, s, score, sounds) = (p, (x1, yRes, vxRes, vyRes), paredesRes, s, score, sounds)
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

atualizaMundo :: Float -> Mundo -> IO Mundo
atualizaMundo dt (player, bola, paredes, s, score, sounds) = return m4
  where
    m1 = (atualizaPosicaoPlayer dt player, bola, paredes, s, score, sounds)
    m2 = atualizaPosicaoBola dt m1
    m3 = atualizaStatusGame m2
    m4 = atualizaScore m1 m3

{-
  Recebe o mundo do jogo e atualiza ele a depender do Status
  , mudar para a tela de vitória caso acabem os inimigos
-}
atualizaStatusGame :: Mundo -> Mundo
atualizaStatusGame m@(p, b@(x, y, vx, vy), ps, s, sc, sounds) = mundoRes
  where
    novoMundo = inicializaMundo (snd sc) sounds
    mundoRes
      | s == Playing && null ps = (p, b, ps, Win, sc, sounds)
      | s == Playing && (y < -alturaTela/2 - 100) = (p, b, ps, Lose, sc, sounds)
      | otherwise = m