module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact

f x = if (x < 15) && (x >= 0) then -2*x^2 + 30*x else 0

type Time = Float
type Jumps = Int
type Catchs = Int

type World = (Jumps, Time, Catchs)

draw :: Picture -> World -> Picture
draw pic (j, x, c) = Pictures $ [bg, circle_, text_, line_] ++ [jumps, time, catchs, scores] ++ [info, info_]
  where
    circle_ = Color white $ Translate 0 (f x) $ Circle 40
    text_ = Color white $ Translate (-60) (-100) $ Scale 0.1 0.1 $ Text "Press J to Jump"
    bg = Translate 100 100 $ Scale 0.1 0.1 pic
    line_ = Translate 0 (-40) $ Color white $ Line [(-100,0), (100,0)]

    jumps = Translate (-50) 160 $ Scale 0.15 0.15 $ Color white $ Text $ "Jumps : " <> show j
    time = Translate (-35) (-160) $ Scale 0.1 0.1 $ Color white $ Text $ "Time : " <> show (round $ x/5)
    catchs = Translate (-25) 135 $ Scale 0.08 0.08 $ Color white $ Text $ "Catchs : " <> show c
    scores = Translate (-285) 135 $ Scale 0.08 0.08 $ Color white $ Text $ "Scores : " <> show (fromIntegral c / fromIntegral j)

    info = Translate (-285) 180 $ Scale 0.08 0.08 $ Color white $ Text $ "Jump if the time is"
    info_ = Translate (-285) 163 $ Scale 0.08 0.08 $ Color white $ Text $ "being 100.."

update :: Float -> World -> World
update _ (j, x, s) = (j, x + 0.2, s)

input :: Event -> World -> World
input (EventKey (Char 'j') (Down) _ (_, _)) (j, x, s) =
  if f x == 0 then (if (round $ x/5) == 100 then (j+1, 0, s+1) else (j+1, 0, s)) else (j, x, s)

input _ w = w

main = do
  mybmp <- loadBMP "moon.bmp"
  play 
    (InWindow "Catch100" (600,400) (50,50))
    black 100 (0, 0.0, 0)
    (draw mybmp) input update