module Main (main) where

import Logic
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.IO.Game  ( playIO, Event(..), Key(..)
                                         , SpecialKey(..), KeyState(..) )
import qualified SDL
import qualified SDL.Mixer as Mix

tamTitulo, posXTitulo, posYTitulo :: Float
tamTitulo = 0.25
posXTitulo = -350
posYTitulo = alturaTela / 2 - 100

desenhaTelaVitoria :: Score -> IO Picture
desenhaTelaVitoria (sc, max) = return $
  Pictures
    [ Color white $ Translate posXTitulo posYTitulo $ Scale (tamTitulo * 2) (tamTitulo * 2) $ Text "VOCE VENCEU!",
      Color white $ Translate posXTitulo (posYTitulo - 100) $ Scale tamTitulo tamTitulo $ Text ("SEU SCORE: " ++ show sc),
      Color white $ Translate posXTitulo (posYTitulo - 200) $ Scale tamTitulo tamTitulo $ Text ("HIGHEST SCORE: " ++ show max),
      Color white $ Translate posXTitulo (posYTitulo - 350) $ Scale tamTitulo tamTitulo $ Text "Aperte Espaco para jogar novamente"
    ]

desenhaTelaDerrota :: Score -> IO Picture
desenhaTelaDerrota (sc, max) = return $
  Pictures
    [ Color white $ Translate posXTitulo posYTitulo $ Scale (tamTitulo * 2) (tamTitulo * 2) $ Text "VOCE PERDEU!",
      Color white $ Translate posXTitulo (posYTitulo - 100) $ Scale tamTitulo tamTitulo $ Text ("SEU SCORE: " ++ show sc),
      Color white $ Translate posXTitulo (posYTitulo - 200) $ Scale tamTitulo tamTitulo $ Text ("HIGHEST SCORE: " ++ show max),
      Color white $ Translate posXTitulo (posYTitulo - 350) $ Scale tamTitulo tamTitulo $ Text "Aperte Espaco para jogar novamente"
    ]

desenhaSplashScreen :: IO Picture
desenhaSplashScreen = return $
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

desenhaMundo :: Mundo -> IO Picture
desenhaMundo (player, bola, ps, status, score, sounds) =
  case status of
    Start -> desenhaSplashScreen
    Playing -> return $ Pictures [playerPic, bolaPic, paredesPic, scorePic]
    Lose -> desenhaTelaDerrota score
    Win -> desenhaTelaVitoria score
  where
    playerPic = desenhaPlayer player
    bolaPic = desenhaBola bola
    paredesPic = desenhaParedes ps
    scorePic = desenhaScore score

velPlayer = 500

inputHandler :: Event -> Mundo -> IO Mundo
inputHandler (EventKey (SpecialKey KeyRight) Down _ _) ((x, y, v), b, ps, s, sc, sound) = return ((x, y, velPlayer), b, ps, s, sc, sound)
inputHandler (EventKey (SpecialKey KeyRight) Up _ _) ((x, y, v), b, ps, s, sc, sound) = return ((x, y, 0), b, ps, s, sc, sound)
inputHandler (EventKey (SpecialKey KeyLeft) Down _ _) ((x, y, v), b, ps, s, sc, sound) = return ((x, y, -velPlayer), b, ps, s, sc, sound)
inputHandler (EventKey (SpecialKey KeyLeft) Up _ _) ((x, y, v), b, ps, s, sc, sound) = return ((x, y, 0), b, ps, s, sc, sound)
inputHandler (EventKey (SpecialKey KeySpace) Up _ _) ((x, y, v), b, ps, s, sc, sound) =
  case s of
    Playing -> return ((x, y, 0), b, ps, s, sc, sound)
    _ -> return $ inicializaMundo (snd sc) sound
inputHandler (EventKey (Char 'a') Down _ _) ((x, y, v), b, ps, s, sc, sound) = return ((x, y, -velPlayer), b, ps, s, sc, sound)
inputHandler (EventKey (Char 'a') Up _ _) ((x, y, v), b, ps, s, sc, sound) = return ((x, y, 0), b, ps, s, sc, sound)
inputHandler (EventKey (Char 'd') Down _ _) ((x, y, v), b, ps, s, sc, sound) = return ((x, y, velPlayer), b, ps, s, sc, sound)
inputHandler (EventKey (Char 'd') Up _ _) ((x, y, v), b, ps, s, sc, sound) = return ((x, y, 0), b, ps, s, sc, sound)
inputHandler _ m = return m

loadSounds :: IO Sounds
loadSounds = do
  pdlFr <- Mix.load "audio/paddle-bounce-front.wav"
  pdlSd <- Mix.load "audio/paddle-bounce-side.wav"
  topWall <- Mix.load "audio/wall-bounce-top.wav"
  btmWall <- Mix.load "audio/wall-bounce-bottom.wav"
  begin <- Mix.load "audio/begin-play.ogg"
  music <- Mix.load "audio/background-music.ogg"
  defeat <- Mix.load "audio/defeat.wav"
  victory <- Mix.load "audio/victory.ogg"
  return $ Sounds pdlFr pdlSd topWall btmWall begin music defeat victory

main :: IO ()
main = do
  SDL.initialize [SDL.InitAudio]  -- Setup audio via SDL
  let chunkSz = 256
    in Mix.withAudio Mix.defaultAudio chunkSz $ do
        snds <- loadSounds
        Mix.play $ begin snds
        Mix.play $ bkgndMusic snds
        playIO 
          janela 
          black 
          60 
          (mundoStart snds)
          desenhaMundo 
          inputHandler
          atualizaMundo

  SDL.quit
  where
    janela = InWindow "Breakout" (800, 600) (10, 10)