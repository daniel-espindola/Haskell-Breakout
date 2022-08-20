module Main (main) where

import Logic
import Graphics.Gloss.Interface.Pure.Game

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
    Lose -> desenhaTelaDerrota score
    Win -> desenhaTelaVitoria score
  where
    playerPic = desenhaPlayer player
    bolaPic = desenhaBola bola
    paredesPic = desenhaParedes ps
    scorePic = desenhaScore score


velPlayer = 500

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