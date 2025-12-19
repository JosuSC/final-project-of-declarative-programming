{-# LANGUAGE RecordWildCards #-}

module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Data.Maybe (fromMaybe)
import qualified Data.Map as M

-- ==========================
-- Configuración visual
-- ==========================

cellSize :: Float
cellSize = 36

padding :: Float
padding = 16

bgColor :: Color
bgColor = makeColorI 23 25 35 255

panelColor :: Color
panelColor = makeColorI 34 38 49 255

gridColor :: Color
gridColor = makeColorI 60 68 86 255

hiddenColor :: Color
hiddenColor = makeColorI 78 90 110 255

revealedColor :: Color
revealedColor = makeColorI 200 210 220 255

flagColor :: Color
flagColor = makeColorI 240 90 70 255

mineColor :: Color
mineColor = makeColorI 250 210 60 255

textColor :: Color
textColor = white

-- Color por número
numColor :: Int -> Color
numColor n = case n of
  1 -> makeColorI 50 140 230 255
  2 -> makeColorI 60 170 120 255
  3 -> makeColorI 230 90 90 255
  4 -> makeColorI 150 100 220 255
  5 -> makeColorI 200 120 60 255
  6 -> makeColorI 80 190 180 255
  7 -> makeColorI 90 90 90 255
  8 -> makeColorI 40 40 40 255
  _ -> black

-- ==========================
-- Dificultades
-- ==========================

data Difficulty = Easy | Medium | Hard deriving (Eq, Show)

difficultyParams :: Difficulty -> (Int, Int, Int)
difficultyParams Easy   = (9, 9, 10)    -- filas, cols, minas
difficultyParams Medium = (16, 16, 40)
difficultyParams Hard   = (16, 30, 99)

-- ==========================
-- Tablero y estado
-- ==========================

type Pos = (Int, Int)

data Cell = Cell
  { isMine     :: Bool
  , isRevealed :: Bool
  , isFlagged  :: Bool
  , adjMines   :: Int
  } deriving (Show, Eq)

type Board = M.Map Pos Cell

data Phase
  = Menu
  | Playing
  | Won
  | Lost
  deriving (Eq, Show)

data World = World
  { phase      :: Phase
  , difficulty :: Difficulty
  , rows       :: Int
  , cols       :: Int
  , minesN     :: Int
  , board      :: Board
  , firstClick :: Bool
  , rng        :: StdGen
  , elapsed    :: Float
  }

-- ==========================
-- Utilidades
-- ==========================

inBounds :: World -> Pos -> Bool
inBounds World{..} (r,c) = r >= 0 && r < rows && c >= 0 && c < cols

neighbors :: Pos -> [Pos]
neighbors (r,c) = [(r+dr, c+dc) | dr <- [-1..1], dc <- [-1..1], (dr,dc) /= (0,0)]

-- ==========================
-- Generación del tablero
-- ==========================

emptyBoard :: World -> Board
emptyBoard World{..} = M.fromList
  [ ((r,c), Cell False False False 0) | r <- [0..rows-1], c <- [0..cols-1] ]

placeMines :: StdGen -> World -> Pos -> (Board, StdGen)
placeMines g w@World{..} safePos =
  let spaces = [(r,c) | r <- [0..rows-1], c <- [0..cols-1]]
      candidates = filter (\p -> p /= safePos && notElem p (neighbors safePos)) spaces
      (chosen, g') = chooseUnique g minesN candidates
      b0 = emptyBoard w
      bM = foldr (\p b -> M.adjust (\c -> c{isMine=True}) p b) b0 chosen
      bA = computeAdj bM w
  in (bA, g')

computeAdj :: Board -> World -> Board
computeAdj b w =
  M.mapWithKey (\p cell ->
    if isMine cell
      then cell { adjMines = -1 }
      else cell { adjMines = length . filter id $ map (\q -> fromMaybe False (isMine <$> M.lookup q b)) (neighbors p) }
  ) b

chooseUnique :: StdGen -> Int -> [Pos] -> ([Pos], StdGen)
chooseUnique g n xs = go g n xs []
  where
    go gen k pool acc
      | k <= 0 || null pool = (acc, gen)
      | otherwise =
          let (i, gen') = randomR (0, length pool - 1) gen
              picked = pool !! i
              pool' = take i pool ++ drop (i+1) pool
          in go gen' (k-1) pool' (picked:acc)

-- ==========================
-- Revelado y banderas
-- ==========================

revealAt :: Pos -> World -> World
revealAt p w@World{..}
  | not (inBounds w p) = w
  | otherwise =
      case M.lookup p board of
        Nothing -> w
        Just cell
          | isFlagged cell || isRevealed cell -> w
          | isMine cell -> w { board = M.adjust (\c -> c{isRevealed=True}) p board
                             , phase = Lost
                             }
          | adjMines cell == 0 ->
              let b' = floodReveal [p] board
              in checkWin w { board = b' }
          | otherwise ->
              checkWin w { board = M.adjust (\c -> c{isRevealed=True}) p board }

floodReveal :: [Pos] -> Board -> Board
floodReveal queue b = go queue b
  where
    go [] bb = bb
    go (q:qs) bb =
      case M.lookup q bb of
        Nothing -> go qs bb
        Just c
          | isRevealed c || isFlagged c -> go qs bb
          | isMine c -> go qs bb
          | otherwise ->
              let c' = c { isRevealed = True }
                  bb' = M.insert q c' bb
              in if adjMines c' == 0
                    then go (neighbors q ++ qs) bb'
                    else go qs bb'

toggleFlag :: Pos -> World -> World
toggleFlag p w@World{..}
  | not (inBounds w p) = w
  | phase /= Playing = w
  | otherwise =
      case M.lookup p board of
        Nothing -> w
        Just c
          | isRevealed c -> w
          | otherwise -> w { board = M.adjust (\x -> x{isFlagged = not (isFlagged x)}) p board }

checkWin :: World -> World
checkWin w@World{..} =
  let cells = M.elems board
      unrevealedSafe = any (\c -> not (isMine c) && not (isRevealed c)) cells
  in if unrevealedSafe then w else w { phase = Won }

-- ==========================
-- Interfaz: renderizado
-- ==========================

main :: IO ()
main = do
  gen <- getStdRandom (\g -> (g,g))
  let w0 = initialWorld gen
      (winW, winH) = idealWindowSize w0
  play
    (InWindow "Busca Minas — Gloss" (winW, winH) (100, 100))
    bgColor
    60
    w0
    renderWorld
    handleEvent
    stepWorld

initialWorld :: StdGen -> World
initialWorld gen =
  let d = Easy
      (r,c,m) = difficultyParams d
  in World
      { phase = Menu
      , difficulty = d
      , rows = r
      , cols = c
      , minesN = m
      , board = emptyBoard (World Menu d r c m M.empty True gen 0)
      , firstClick = True
      , rng = gen
      , elapsed = 0
      }

renderWorld :: World -> Picture
renderWorld w@World{..} =
  Pictures
    [ uncurry backgroundPanel (idealWindowSize w)
    , translate (-fromIntegral screenW/2 + padding) (fromIntegral screenH/2 - padding) $
        scale 0.12 0.12 $ color textColor $ Text titleText
    , hudBar w
    , case phase of
        Menu    -> renderMenu w
        Playing -> renderBoard w
        Won     -> Pictures [renderBoard w, overlayMessage "¡Has ganado! Pulsa R para reiniciar o M para menú"]
        Lost    -> Pictures [renderBoard w, overlayMessage "Has perdido... Pulsa R para reiniciar o M para menú"]
    ]
  where
    titleText = "BUSCAMINAS"
    (screenW, screenH) = idealWindowSize w

backgroundPanel :: Int -> Int -> Picture
backgroundPanel w h =
  color panelColor $ rectangleSolid (fromIntegral w) (fromIntegral h)

hudBar :: World -> Picture
hudBar w@World{..} =
  let (screenW, screenH) = idealWindowSize w
      flagsPlaced = length [() | c <- M.elems board, isFlagged c]
      minesLeft = max 0 (minesN - flagsPlaced)
      timeS :: Int
      timeS = floor elapsed
      diffLabel = case difficulty of
        Easy -> "Fácil"
        Medium -> "Medio"
        Hard -> "Difícil"
      phaseLabel = case phase of
        Menu -> "MENÚ"
        Playing -> "JUGANDO"
        Won -> "VICTORIA"
        Lost -> "DERROTA"
      barY = fromIntegral screenH/2 - 80
      barW = fromIntegral screenW - 2*padding
      barH = 56
      bar = translate 0 barY $ Pictures
        [ translate 2 (-2) $ color (makeColorI 12 12 12 120) $ roundedRect barW barH 12
        , color (makeColorI 28 32 42 255) $ roundedRect barW barH 12
        , translate (-barW/2 + 18) (-10) $ scale 0.14 0.14 $ color (makeColorI 220 220 220 255) $
            Text (phaseLabel ++ "  |  " ++ diffLabel)
        , translate (-28) (-10) $ scale 0.14 0.14 $ color (makeColorI 220 220 220 255) $
            Text ("Minas: " ++ show minesLeft ++ "  |  Banderas: " ++ show flagsPlaced)
        , translate (barW/2 - 160) (-10) $ scale 0.14 0.14 $ color (makeColorI 220 220 220 255) $
            Text ("Tiempo: " ++ show timeS ++ "s")
        ]
  in bar

statusBar :: World -> Picture
statusBar World{..} =
  let msg = case phase of
        Menu    -> "Elige dificultad: 1=Fácil, 2=Medio, 3=Difícil"
        Playing -> "Clic izq: revelar | Clic der: bandera | R: reiniciar | M: menú"
        Won     -> "¡Victoria! R: reiniciar | M: menú"
        Lost    -> "Derrota. R: reiniciar | M: menú"
  in scale 0.12 0.12 $ color (makeColorI 220 220 220 255) $ Text msg

renderMenu :: World -> Picture
renderMenu World{..} =
  let items =
        [ ("Fácil (9x9, 10 minas)", Easy)
        , ("Medio (16x16, 40 minas)", Medium)
        , ("Difícil (16x30, 99 minas)", Hard)
        ]
      pics = zipWith (\i (label,_) ->
                        translate 0 (120 - fromIntegral i * 80) $
                          menuButton label) [0..] items
      hint = translate 0 (-160) $ scale 0.14 0.14 $
               color (makeColorI 210 210 210 255) $
                 Text "Pulsa 1, 2 o 3 para empezar"
  in Pictures (pics ++ [hint])

menuButton :: String -> Picture
menuButton label =
  Pictures
    [ color gridColor $ roundedRect 480 60 14
    , translate 0 0 $ scale 0.16 0.16 $ color textColor $ Text label
    ]

roundedRect :: Float -> Float -> Float -> Picture
roundedRect w h r =
  Pictures
    [ translate 0 0 $ color gridColor $ rectangleSolid w h
    , translate (-w/2 + r) ( h/2 - r) $ color gridColor $ circleSolid r
    , translate ( w/2 - r) ( h/2 - r) $ color gridColor $ circleSolid r
    , translate (-w/2 + r) (-h/2 + r) $ color gridColor $ circleSolid r
    , translate ( w/2 - r) (-h/2 + r) $ color gridColor $ circleSolid r
    ]

renderBoard :: World -> Picture
renderBoard w@World{..} =
  let (gridW, gridH) = (fromIntegral cols * cellSize, fromIntegral rows * cellSize)
      originX = -gridW/2
      originY = gridH/2
      cellsPic = [ renderCell w (r,c) originX originY | r <- [0..rows-1], c <- [0..cols-1] ]
      frame = color gridColor $ rectangleWire (gridW + 8) (gridH + 8)
      shadow = translate 4 (-4) $ color (makeColorI 15 15 15 120) $ rectangleSolid (gridW + 8) (gridH + 8)
      back = color (makeColorI 40 46 58 255) $ rectangleSolid (gridW + 8) (gridH + 8)
  in translate 0 (-40) $ Pictures [shadow, back, frame, Pictures cellsPic]

renderCell :: World -> Pos -> Float -> Float -> Picture
renderCell w@World{..} (r,c) ox oy =
  let x = ox + fromIntegral c * cellSize + cellSize/2
      y = oy - fromIntegral r * cellSize - cellSize/2
      cell = board M.! (r,c)
      baseRect clr = translate x y $ color clr $ roundedRect (cellSize-4) (cellSize-4) 6
      content =
        if isRevealed cell
          then if isMine cell
                 then translate x y $ Pictures
                        [ color revealedColor $ roundedRect (cellSize-4) (cellSize-4) 6
                        , scale 0.4 0.4 $ color mineColor $ ThickCircle 16 8
                        ]
                 else if adjMines cell == 0
                        then translate x y $ color revealedColor $ roundedRect (cellSize-4) (cellSize-4) 6
                        else Pictures
                          [ translate x y $ color revealedColor $ roundedRect (cellSize-4) (cellSize-4) 6
                          , translate (x - 10) (y - 10) $
                              scale 0.14 0.14 $ color (numColor (adjMines cell)) $ Text (show (adjMines cell))
                          ]
          else if isFlagged cell
                 then Pictures
                   [ baseRect hiddenColor
                   , translate x y $ flagIcon
                   ]
                 else baseRect hiddenColor
  in content

flagIcon :: Picture
flagIcon =
  Pictures
    [ translate (-8) (-10) $ color (makeColorI 60 60 60 255) $ rectangleSolid 3 20
    , translate 2 0 $ color flagColor $ polygon [(-8,8),(10,0),(-8,-8)]
    , translate 0 (-12) $ color (makeColorI 150 110 90 255) $ rectangleSolid 10 4
    ]

overlayMessage :: String -> Picture
overlayMessage msg =
  translate 0 0 $ Pictures
    [ color (makeColorI 0 0 0 100) $ rectangleSolid 1000 600
    , translate 0 0 $ scale 0.2 0.2 $ color white $ Text msg
    ]

idealWindowSize :: World -> (Int, Int)
idealWindowSize World{..} =
  let w = max 1024 (round (fromIntegral cols * cellSize + 2*padding + 200))
      h = max 768  (round (fromIntegral rows * cellSize + 2*padding + 160))
  in (w, h)

-- ==========================
-- Eventos
-- ==========================

handleEvent :: Event -> World -> World
handleEvent (EventKey (Char '1') Down _ _) w = startGame Easy w
handleEvent (EventKey (Char '2') Down _ _) w = startGame Medium w
handleEvent (EventKey (Char '3') Down _ _) w = startGame Hard w
handleEvent (EventKey (Char 'r') Down _ _) w = restartSame w
handleEvent (EventKey (Char 'R') Down _ _) w = restartSame w
handleEvent (EventKey (Char 'm') Down _ _) w = toMenu w
handleEvent (EventKey (Char 'M') Down _ _) w = toMenu w

-- Click izquierdo / derecho
handleEvent (EventKey (MouseButton LeftButton) Down _ mousePos) w =
  case phase w of
    Playing -> leftClick mousePos w
    _       -> w

handleEvent (EventKey (MouseButton RightButton) Down _ mousePos) w =
  case phase w of
    Playing -> rightClick mousePos w
    _       -> w

handleEvent _ w = w

startGame :: Difficulty -> World -> World
startGame d w@World{..} =
  let (r,c,m) = difficultyParams d
      b = emptyBoard (World Playing d r c m M.empty True rng 0)
  in w { phase = Playing
       , difficulty = d
       , rows = r
       , cols = c
       , minesN = m
       , board = b
       , firstClick = True
       , elapsed = 0
       }

restartSame :: World -> World
restartSame w@World{..} =
  startGame difficulty w

toMenu :: World -> World
toMenu w@World{..} =
  let (r,c,m) = difficultyParams difficulty
  in w { phase = Menu
       , rows = r, cols = c, minesN = m
       , board = emptyBoard w
       , firstClick = True
  , elapsed = 0
       }

-- ==========================
-- Conversión de clic a celda
-- ==========================

mouseToCell :: World -> (Float, Float) -> Maybe Pos
mouseToCell w@World{..} (mx,my) =
  let (gridW, gridH) = (fromIntegral cols * cellSize, fromIntegral rows * cellSize)
      originX = -gridW/2
      originY = gridH/2 - 40
      gx = mx - 0
      gy = my + 40
      cx = floor ((gx - originX) / cellSize)
      cy = floor ((originY - gy) / cellSize)
      p = (cy, cx)
  in if inBounds w p then Just p else Nothing

leftClick :: (Float, Float) -> World -> World
leftClick pos w@World{..} =
  case mouseToCell w pos of
    Nothing -> w
    Just p ->
      if firstClick
        then
          let (b', g') = placeMines rng w p
              w' = w { board = b', rng = g', firstClick = False }
          in revealAt p w'
        else revealAt p w

rightClick :: (Float, Float) -> World -> World
rightClick pos w =
  case mouseToCell w pos of
    Nothing -> w
    Just p  -> toggleFlag p w

-- ==========================
-- Step (animación suave)
-- ==========================

stepWorld :: Float -> World -> World
stepWorld dt w@World{..} = w { elapsed = elapsed + dt }

