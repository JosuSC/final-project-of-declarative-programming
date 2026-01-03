{-|
Module      : Config
Description : Configuración visual y constantes para MATCOMINESWEEPER
Copyright   : (c) Josué Javier Senarega Claro, Ronald Cabrera Martínez, 2025
License     : MIT
Maintainer  : Estudiantes C-311, MATCOM, Universidad de La Habana

Este módulo centraliza todas las constantes de configuración visual del juego.
Tener las configuraciones en un solo lugar permite:
  - Ajustar fácilmente la apariencia del juego
  - Mantener consistencia visual en todo el código
  - Facilitar el mantenimiento y las pruebas
-}

module Config 
    ( -- * Dimensiones
      cellSize
    , padding
    , buttonWidth
    , buttonHeight
    , buttonSpacing
      -- * Colores principales
    , bgColor
    , panelColor
    , gridColor
    , hiddenColor
    , revealedColor
    , flagColor
    , mineColor
    , textColor
    , accentColor
    , splashBgColor
      -- * Colores de números
    , numColor
      -- * Configuración de ventana
    , windowTitle
    , fps
      -- * Tiempos
    , splashDuration
    ) where

import Graphics.Gloss

-- ============================================================================
-- DIMENSIONES
-- ============================================================================

{-|
  Tamaño en píxeles de cada celda del tablero.
  Un valor de 36 proporciona buen balance entre visibilidad y espacio.
-}
cellSize :: Float
cellSize = 36

{-|
  Padding (margen) interno en píxeles.
  Se usa para separar elementos de los bordes.
-}
padding :: Float
padding = 20

{-|
  Ancho estándar de los botones del menú.
-}
buttonWidth :: Float
buttonWidth = 320

{-|
  Alto estándar de los botones del menú.
-}
buttonHeight :: Float
buttonHeight = 55

{-|
  Espaciado vertical entre botones del menú.
-}
buttonSpacing :: Float
buttonSpacing = 75

-- ============================================================================
-- PALETA DE COLORES
-- ============================================================================
-- Usamos una paleta moderna y agradable inspirada en temas oscuros
-- populares en editores de código (Dracula, One Dark, etc.)

{-|
  Color de fondo principal de la aplicación.
  Un azul muy oscuro que es agradable a la vista.
-}
bgColor :: Color
bgColor = makeColorI 25 28 38 255

{-|
  Color del panel de fondo detrás de elementos UI.
  Ligeramente más claro que el fondo principal.
-}
panelColor :: Color
panelColor = makeColorI 35 40 52 255

{-|
  Color de la cuadrícula y bordes.
  Proporciona contraste sutil sin ser agresivo.
-}
gridColor :: Color
gridColor = makeColorI 68 76 96 255

{-|
  Color de las celdas no reveladas.
  Un gris azulado que invita a hacer clic.
-}
hiddenColor :: Color
hiddenColor = makeColorI 88 100 125 255

{-|
  Color de las celdas reveladas (sin mina).
  Un color claro que indica "seguro".
-}
revealedColor :: Color
revealedColor = makeColorI 215 220 230 255

{-|
  Color de las banderas.
  Un rojo/naranja vibrante para visibilidad.
-}
flagColor :: Color
flagColor = makeColorI 255 95 86 255

{-|
  Color de las minas.
  Amarillo/dorado que resalta sobre cualquier fondo.
-}
mineColor :: Color
mineColor = makeColorI 255 215 64 255

{-|
  Color del texto principal.
  Blanco puro para máxima legibilidad.
-}
textColor :: Color
textColor = white

{-|
  Color de acento para elementos destacados.
  Un azul brillante estilo MATCOM.
-}
accentColor :: Color
accentColor = makeColorI 80 160 255 255

{-|
  Color de fondo de la pantalla splash.
  Azul oscuro profundo con tinte púrpura.
-}
splashBgColor :: Color
splashBgColor = makeColorI 20 22 35 255

-- ============================================================================
-- COLORES POR NÚMERO DE MINAS ADYACENTES
-- ============================================================================

{-|
  Devuelve el color correspondiente al número de minas adyacentes.
  Sigue la convención clásica del Buscaminas de Windows:
  
  - 1: Azul
  - 2: Verde
  - 3: Rojo
  - 4: Púrpura oscuro
  - 5: Marrón/Naranja
  - 6: Cyan
  - 7: Negro
  - 8: Gris
  
  Esta codificación por colores ayuda al jugador a identificar
  rápidamente la densidad de minas en cada área.
-}
numColor :: Int -> Color
numColor n = case n of
    1 -> makeColorI 64 150 255 255   -- Azul brillante
    2 -> makeColorI 80 190 120 255   -- Verde esmeralda
    3 -> makeColorI 240 90 90 255    -- Rojo coral
    4 -> makeColorI 160 100 230 255  -- Púrpura
    5 -> makeColorI 220 130 60 255   -- Naranja
    6 -> makeColorI 70 200 190 255   -- Cyan/Turquesa
    7 -> makeColorI 60 60 70 255     -- Gris oscuro
    8 -> makeColorI 100 100 110 255  -- Gris medio
    _ -> black

-- ============================================================================
-- CONFIGURACIÓN DE VENTANA
-- ============================================================================

{-|
  Título de la ventana del juego.
-}
windowTitle :: String
windowTitle = "MATCOMINESWEEPER — Proyecto Final de Programación Declarativa"

{-|
  Frames por segundo para la animación.
  60 FPS proporciona animaciones suaves sin consumir
  demasiados recursos.
-}
fps :: Int
fps = 60

-- ============================================================================
-- TIEMPOS
-- ============================================================================

{-|
  Duración en segundos de la pantalla splash.
  3 segundos es suficiente para leer sin ser tedioso.
-}
splashDuration :: Float
splashDuration = 5.0
