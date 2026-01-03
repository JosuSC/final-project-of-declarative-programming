{-|
Module      : Render
Description : Renderizado gráfico para MATCOMINESWEEPER
Copyright   : (c) Josué Javier Senarega Claro, Ronald Cabrera Martínez, 2025
License     : MIT
Maintainer  : Estudiantes C-311, MATCOM, Universidad de La Habana

Este módulo se encarga de todo el renderizado gráfico del juego:
  - Pantalla de splash/presentación
  - Menú de selección de dificultad
  - Tablero de juego
  - HUD (información del juego)
  - Mensajes de victoria/derrota

Se utiliza la librería Gloss, que proporciona una API declarativa
para gráficos 2D. El paradigma funcional de Gloss encaja perfectamente
con Haskell: describimos QUÉ queremos dibujar, no CÓMO dibujarlo.
-}

{-# LANGUAGE RecordWildCards #-}

module Render 
    ( -- * Renderizado principal
      renderWorld
      -- * Cálculos de dimensiones
    , idealWindowSize
    , mouseToCell
    ) where

import Types
import Config
import Board (countFlags, countRemainingMines)
import Graphics.Gloss
import qualified Data.Map as M

-- ============================================================================
-- RENDERIZADO PRINCIPAL
-- ============================================================================

{-|
  Función principal de renderizado.
  Recibe el estado del mundo y produce una imagen (Picture).
  
  Esta función actúa como un "router" que decide qué pantalla
  mostrar según la fase actual del juego.
-}
renderWorld :: World -> Picture
renderWorld w@World{..} =
    case phase of
        Splash  -> renderSplash w
        Menu    -> renderMenuScreen w
        Playing -> renderPlayingScreen w
        Won     -> renderEndScreen w "VICTORIA!" "Has descubierto todas las celdas seguras"
        Lost    -> renderEndScreen w "DERROTA" "Has pisado una mina"

-- ============================================================================
-- PANTALLA DE SPLASH (PRESENTACIÓN)
-- ============================================================================

{-|
  Renderiza la pantalla de presentación inicial.
  
  Muestra:
  - Logo/título del juego
  - Subtítulo con información académica
  - Créditos de los autores
  - Efecto de fade-in animado
-}
renderSplash :: World -> Picture
renderSplash World{..} =
    let 
        (screenW, screenH) = (1024, 768)
        
        -- Efecto de fade basado en el tiempo
        alpha = min 1.0 (splashTime / 0.8)
        fadeColor a = makeColorI 255 255 255 (round (255 * a * alpha))
        
        -- Fondo con gradiente simulado
        background = Pictures
            [ color splashBgColor $ rectangleSolid (fromIntegral screenW) (fromIntegral screenH)
            , translate 0 (-100) $ color (makeColorI 30 50 100 40) $ circleSolid 400
            , translate 100 100 $ color (makeColorI 80 40 120 30) $ circleSolid 300
            ]
        
        -- Título principal con efecto
        titleShadow = translate 3 (-3) $ scale 0.6 0.6 $ 
            color (makeColorI 0 0 0 100) $ Text "MATCOMINESWEEPER"
        titleMain = scale 0.6 0.6 $ 
            color (fadeColor 1.0) $ Text "MATCOMINESWEEPER"
        title = translate (-380) 120 $ Pictures [titleShadow, titleMain]
        
        -- Subtítulo
        subtitle = translate (-320) 40 $ scale 0.2 0.2 $ 
            color (fadeColor 0.9) $ 
            Text "Proyecto Final de Programación Declarativa"
        
        -- Información de la universidad
        uniInfo = translate (-180) (-20) $ scale 0.15 0.15 $ 
            color (fadeColor 0.7) $ 
            Text "MATCOM — Universidad de La Habana"
        
        -- Indicador de carga
        loadingDots = 
            let numDots = (floor (splashTime * 3) `mod` 4) :: Int
                dotsText = replicate numDots '.'
            in translate (-80) (-120) $ scale 0.15 0.15 $ 
               color (fadeColor 0.8) $ Text ("Cargando" ++ dotsText)
        
        -- Créditos
        credits = Pictures
            [ translate (-150) (-280) $ scale 0.1 0.1 $ 
                color (fadeColor 0.5) $ Text "Creado por:"
            , translate (-200) (-310) $ scale 0.1 0.1 $ 
                color (fadeColor 0.5) $ Text "Josue Javier Senarega Claro — C-311"
            , translate (-185) (-335) $ scale 0.1 0.1 $ 
                color (fadeColor 0.5) $ Text "Ronald Cabrera Martinez — C-311"
            ]
        
        -- Decoración: pequeñas minas
        decorations = Pictures
            [ translate (-350) (-180) $ scale 0.3 0.3 $ renderMineIcon
            , translate 350 (-180) $ scale 0.3 0.3 $ renderMineIcon
            , translate (-400) 200 $ scale 0.2 0.2 $ renderFlagIcon
            , translate 400 200 $ scale 0.2 0.2 $ renderFlagIcon
            ]
        
    in Pictures [background, title, subtitle, uniInfo, loadingDots, credits, decorations]

-- ============================================================================
-- PANTALLA DE MENÚ
-- ============================================================================

{-|
  Renderiza el menú de selección de dificultad.
  
  Presenta tres opciones de dificultad con botones estilizados
  y una interfaz clara e intuitiva.
-}
renderMenuScreen :: World -> Picture
renderMenuScreen w@World{..} =
    let 
        (screenW, screenH) = idealWindowSize w
        
        -- Fondo
        background = color bgColor $ 
            rectangleSolid (fromIntegral screenW) (fromIntegral screenH)
        
        -- Título
        title = translate (-200) 260 $ scale 0.35 0.35 $ 
            color accentColor $ Text "MATCOMINESWEEPER"
        
        subtitle = translate (-155) 210 $ scale 0.12 0.12 $ 
            color (makeColorI 180 180 190 255) $ Text "Selecciona la dificultad"
        
        -- Botones de dificultad
        easyBtn = translate 0 100 $ 
            renderMenuButton "FACIL" "9 x 9 con 10 minas" (difficulty == Easy)
        mediumBtn = translate 0 0 $ 
            renderMenuButton "MEDIO" "16 x 16 con 40 minas" (difficulty == Medium)
        hardBtn = translate 0 (-100) $ 
            renderMenuButton "DIFICIL" "16 x 30 con 99 minas" (difficulty == Hard)
        
        -- Instrucciones
        instructions = translate (-140) (-200) $ scale 0.1 0.1 $ 
            color (makeColorI 150 150 160 255) $ 
            Text "Presiona 1, 2 o 3 para comenzar"
        
        -- Controles
        controls = Pictures
            [ translate (-200) (-260) $ scale 0.08 0.08 $ 
                color (makeColorI 120 120 130 255) $ 
                Text "Controles: Clic izq = revelar | Clic der = bandera"
            , translate (-130) (-290) $ scale 0.08 0.08 $ 
                color (makeColorI 120 120 130 255) $ 
                Text "R = reiniciar | M = menu"
            ]
        
    in Pictures [background, title, subtitle, easyBtn, mediumBtn, hardBtn, instructions, controls]

{-|
  Renderiza un botón del menú.
  
  @param label      Texto principal del botón
  @param sublabel   Texto secundario (descripción)
  @param isSelected Si el botón está seleccionado (para resaltarlo)
-}
renderMenuButton :: String -> String -> Bool -> Picture
renderMenuButton label sublabel isSelected =
    let 
        -- Colores según selección
        btnColor = if isSelected 
                   then makeColorI 60 70 95 255 
                   else makeColorI 45 52 70 255
        borderColor = if isSelected 
                      then accentColor 
                      else makeColorI 70 80 100 255
        
        -- Sombra
        shadow = translate 3 (-3) $ 
            color (makeColorI 10 10 15 150) $ 
            rectangleSolid buttonWidth buttonHeight
        
        -- Cuerpo del botón
        body = color btnColor $ rectangleSolid buttonWidth buttonHeight
        
        -- Borde
        border = color borderColor $ rectangleWire buttonWidth buttonHeight
        
        -- Texto principal (centrado)
        labelPic = translate (-fromIntegral (length label) * 7) 8 $ 
            scale 0.15 0.15 $ color textColor $ Text label
        
        -- Subtexto (centrado)
        sublabelPic = translate (-fromIntegral (length sublabel) * 3.5) (-15) $ 
            scale 0.09 0.09 $ color (makeColorI 160 165 180 255) $ Text sublabel
        
    in Pictures [shadow, body, border, labelPic, sublabelPic]

-- ============================================================================
-- PANTALLA DE JUEGO
-- ============================================================================

{-|
  Renderiza la pantalla principal del juego.
  Incluye el HUD (información) y el tablero.
-}
renderPlayingScreen :: World -> Picture
renderPlayingScreen w =
    let 
        (screenW, screenH) = idealWindowSize w
        
        -- Fondo
        background = color bgColor $ 
            rectangleSolid (fromIntegral screenW) (fromIntegral screenH)
        
    in Pictures [background, renderHUD w, renderBoard w]

{-|
  Renderiza el HUD (Head-Up Display) con información del juego.
  
  Muestra:
  - Dificultad actual
  - Minas restantes
  - Banderas colocadas
  - Tiempo transcurrido
-}
renderHUD :: World -> Picture
renderHUD w@World{..} =
    let 
        (screenW, _) = idealWindowSize w
        
        flagsPlaced = countFlags board
        minesLeft = countRemainingMines minesN board
        timeSeconds = floor elapsed :: Int
        
        -- Posición del HUD
        hudY = fromIntegral (snd (idealWindowSize w)) / 2 - 45
        hudW = fromIntegral screenW - 2 * padding
        hudH = 60
        
        -- Fondo del HUD
        hudBg = translate 0 hudY $ Pictures
            [ translate 2 (-2) $ color (makeColorI 10 10 15 100) $ 
                rectangleSolid hudW hudH
            , color panelColor $ rectangleSolid hudW hudH
            , color gridColor $ rectangleWire hudW hudH
            ]
        
        -- Información (distribuida horizontalmente)
        startX = -hudW / 2 + 30
        
        -- Icono y contador de minas
        mineInfo = translate (startX) hudY $ Pictures
            [ translate 0 0 $ scale 0.25 0.25 $ renderMineIcon
            , translate 30 (-5) $ scale 0.12 0.12 $ 
                color textColor $ Text (show minesLeft)
            ]
        
        -- Icono y contador de banderas
        flagInfo = translate (startX + 120) hudY $ Pictures
            [ translate 0 0 $ scale 0.25 0.25 $ renderFlagIcon
            , translate 30 (-5) $ scale 0.12 0.12 $ 
                color textColor $ Text (show flagsPlaced)
            ]
        
        -- Dificultad
        diffInfo = translate 0 hudY $ scale 0.1 0.1 $ 
            color (makeColorI 180 185 200 255) $ 
            Text (difficultyName difficulty)
        
        -- Tiempo
        timeInfo = translate (hudW / 2 - 100) hudY $ Pictures
            [ translate (-30) (-5) $ scale 0.12 0.12 $ 
                color textColor $ Text "⏱"
            , translate 0 (-5) $ scale 0.12 0.12 $ 
                color textColor $ Text (formatTime timeSeconds)
            ]
        
    in Pictures [hudBg, mineInfo, flagInfo, diffInfo, timeInfo]

{-|
  Formatea el tiempo en formato MM:SS
-}
formatTime :: Int -> String
formatTime secs = 
    let mins = secs `div` 60
        s = secs `mod` 60
    in (if mins < 10 then "0" else "") ++ show mins ++ ":" ++
       (if s < 10 then "0" else "") ++ show s

{-|
  Renderiza el tablero de juego completo.
  
  El tablero se centra en la pantalla y se dibuja celda por celda.
-}
renderBoard :: World -> Picture
renderBoard w@World{..} =
    let 
        -- Dimensiones del tablero en píxeles
        gridW = fromIntegral cols * cellSize
        gridH = fromIntegral rows * cellSize
        
        -- Origen (esquina superior izquierda del tablero)
        originX = -gridW / 2
        originY = gridH / 2
        
        -- Sombra del tablero
        shadow = translate 4 (-4) $ 
            color (makeColorI 10 10 15 100) $ 
            rectangleSolid (gridW + 8) (gridH + 8)
        
        -- Fondo del tablero
        boardBg = color (makeColorI 40 45 60 255) $ 
            rectangleSolid (gridW + 8) (gridH + 8)
        
        -- Borde del tablero
        boardBorder = color gridColor $ 
            rectangleWire (gridW + 10) (gridH + 10)
        
        -- Celdas individuales
        cells = Pictures 
            [ renderCell w (r, c) originX originY 
            | r <- [0..rows-1]
            , c <- [0..cols-1]
            ]
        
    in translate 0 (-30) $ Pictures [shadow, boardBg, boardBorder, cells]

{-|
  Renderiza una celda individual del tablero.
  
  El aspecto visual depende del estado de la celda:
  - No revelada: cuadrado gris con efecto 3D
  - Con bandera: cuadrado gris + icono de bandera
  - Revelada (vacía): cuadrado claro
  - Revelada (número): cuadrado claro + número coloreado
  - Revelada (mina): cuadrado + icono de mina
-}
renderCell :: World -> Pos -> Float -> Float -> Picture
renderCell World{..} (r, c) originX originY =
    let 
        -- Posición central de la celda
        x = originX + fromIntegral c * cellSize + cellSize / 2
        y = originY - fromIntegral r * cellSize - cellSize / 2
        
        -- Obtener datos de la celda
        cell = board M.! (r, c)
        
        -- Tamaño de la celda con margen
        cs = cellSize - 3
        
        -- Renderizado según estado
        cellPic = 
            if isRevealed cell
                then 
                    -- Celda revelada
                    if isMine cell
                        then renderMineCellAt x y cs        -- Mina visible
                        else if adjMines cell == 0
                            then renderEmptyCellAt x y cs   -- Vacía
                            else renderNumberCellAt x y cs (adjMines cell)  -- Número
                else 
                    -- Celda oculta
                    if isFlagged cell
                        then renderFlaggedCellAt x y cs     -- Con bandera
                        else renderHiddenCellAt x y cs      -- Normal
        
    in cellPic

{-|
  Renderiza una celda oculta (no revelada, sin bandera).
  Tiene un efecto 3D sutil para parecer un botón.
-}
renderHiddenCellAt :: Float -> Float -> Float -> Picture
renderHiddenCellAt x y size =
    translate x y $ Pictures
        [ -- Sombra inferior derecha
          translate 2 (-2) $ color (makeColorI 30 35 45 255) $ 
            rectangleSolid size size
        , -- Cuerpo principal
          color hiddenColor $ rectangleSolid size size
        , -- Highlight superior izquierdo
          translate (-size/4) (size/4) $ 
            color (makeColorI 110 125 150 80) $ 
            rectangleSolid (size/2) (size/2)
        ]

{-|
  Renderiza una celda con bandera.
-}
renderFlaggedCellAt :: Float -> Float -> Float -> Picture
renderFlaggedCellAt x y size =
    translate x y $ Pictures
        [ -- Base (igual que celda oculta)
          translate 2 (-2) $ color (makeColorI 30 35 45 255) $ 
            rectangleSolid size size
        , color hiddenColor $ rectangleSolid size size
        , -- Icono de bandera
          scale 0.6 0.6 $ renderFlagIcon
        ]

{-|
  Renderiza una celda revelada vacía (0 minas adyacentes).
-}
renderEmptyCellAt :: Float -> Float -> Float -> Picture
renderEmptyCellAt x y size =
    translate x y $ color revealedColor $ rectangleSolid size size

{-|
  Renderiza una celda revelada con número.
-}
renderNumberCellAt :: Float -> Float -> Float -> Int -> Picture
renderNumberCellAt x y size n =
    translate x y $ Pictures
        [ color revealedColor $ rectangleSolid size size
        , translate (-6) (-8) $ scale 0.16 0.16 $ 
            color (numColor n) $ Text (show n)
        ]

{-|
  Renderiza una celda con mina (cuando se pierde o se gana).
-}
renderMineCellAt :: Float -> Float -> Float -> Picture
renderMineCellAt x y size =
    translate x y $ Pictures
        [ color (makeColorI 200 60 60 255) $ rectangleSolid size size
        , scale 0.5 0.5 $ renderMineIcon
        ]

-- ============================================================================
-- PANTALLA DE FIN DE JUEGO
-- ============================================================================

{-|
  Renderiza la pantalla de fin de juego (victoria o derrota).
  Muestra el tablero de fondo con un overlay y un mensaje.
-}
renderEndScreen :: World -> String -> String -> Picture
renderEndScreen w title subtitle =
    let 
        -- Tablero de fondo
        gameScreen = renderPlayingScreen w
        
        -- Overlay semitransparente
        overlay = color (makeColorI 0 0 10 180) $ 
            rectangleSolid 2000 2000
        
        -- Panel de mensaje
        panelW = 500
        panelH = 180
        panel = Pictures
            [ translate 2 (-2) $ color (makeColorI 0 0 0 100) $ 
                rectangleSolid panelW panelH
            , color panelColor $ rectangleSolid panelW panelH
            , color accentColor $ rectangleWire panelW panelH
            ]
        
        -- Título
        titleColor = if title == "VICTORIA!" 
                     then makeColorI 100 220 120 255 
                     else makeColorI 255 100 100 255
        titlePic = translate (-fromIntegral (length title) * 12) 35 $ 
            scale 0.3 0.3 $ color titleColor $ Text title
        
        -- Subtítulo
        subtitlePic = translate (-fromIntegral (length subtitle) * 4) (-10) $ 
            scale 0.12 0.12 $ color textColor $ Text subtitle
        
        -- Instrucciones
        instructions = translate (-120) (-55) $ scale 0.1 0.1 $ 
            color (makeColorI 180 180 190 255) $ 
            Text "R = Reiniciar  |  M = Menu"
        
    in Pictures [gameScreen, overlay, panel, titlePic, subtitlePic, instructions]

-- ============================================================================
-- ICONOS
-- ============================================================================

{-|
  Icono de bandera para marcar minas.
-}
renderFlagIcon :: Picture
renderFlagIcon = Pictures
    [ -- Mástil
      translate 0 (-8) $ color (makeColorI 80 85 95 255) $ 
        rectangleSolid 3 24
    , -- Bandera (triángulo)
      color flagColor $ polygon [(-2, 12), (14, 4), (-2, -4)]
    , -- Base
      translate 0 (-18) $ color (makeColorI 100 90 80 255) $ 
        rectangleSolid 14 4
    ]

{-|
  Icono de mina.
-}
renderMineIcon :: Picture
renderMineIcon = Pictures
    [ -- Cuerpo de la mina
      color mineColor $ circleSolid 14
    , -- Picos
      color mineColor $ rectangleSolid 28 4
    , color mineColor $ rectangleSolid 4 28
    , rotate 45 $ color mineColor $ rectangleSolid 24 4
    , rotate 45 $ color mineColor $ rectangleSolid 4 24
    , -- Brillo
      translate (-4) 4 $ color (makeColorI 255 255 255 150) $ circleSolid 4
    ]

-- ============================================================================
-- CÁLCULOS DE DIMENSIONES
-- ============================================================================

{-|
  Calcula el tamaño ideal de la ventana según la dificultad.
  
  El tamaño se ajusta para que el tablero quepa cómodamente
  con espacio para el HUD y márgenes.
-}
idealWindowSize :: World -> (Int, Int)
idealWindowSize World{..} =
    let 
        -- Espacio necesario para el tablero
        gridW = fromIntegral cols * cellSize
        gridH = fromIntegral rows * cellSize
        
        -- Añadir espacio para HUD y márgenes
        w = max 800 (round (gridW + 2 * padding + 100))
        h = max 600 (round (gridH + 2 * padding + 150))
    in (w, h)

{-|
  Convierte las coordenadas del mouse a una posición del tablero.
  
  Gloss usa coordenadas donde (0,0) está en el centro de la ventana.
  El tablero está desplazado 30 píxeles hacia abajo para dejar
  espacio al HUD.
  
  @param world Estado del mundo
  @param mousePos Posición del mouse (x, y)
  @return Just (fila, columna) si el mouse está sobre el tablero, Nothing si no
-}
mouseToCell :: World -> (Float, Float) -> Maybe Pos
mouseToCell w@World{..} (mx, my) =
    let 
        -- Dimensiones del tablero
        gridW = fromIntegral cols * cellSize
        gridH = fromIntegral rows * cellSize
        
        -- Origen del tablero (esquina superior izquierda)
        originX = -gridW / 2
        originY = gridH / 2 - 30  -- Ajuste por el desplazamiento del tablero
        
        -- Calcular fila y columna
        col = floor ((mx - originX) / cellSize)
        row = floor ((originY - my) / cellSize)
        
        pos = (row, col)
    in 
        if row >= 0 && row < rows && col >= 0 && col < cols
            then Just pos
            else Nothing
