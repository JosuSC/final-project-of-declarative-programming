{-|
Module      : Main
Description : Punto de entrada para MATCOMINESWEEPER
Copyright   : (c) Josué Javier Senarega Claro, Ronald Cabrera Martínez, 2025
License     : MIT
Maintainer  : Estudiantes C-311, MATCOM, Universidad de La Habana

MATCOMINESWEEPER - Proyecto Final de Programación Declarativa
=============================================================

Este es el módulo principal que inicia el juego Buscaminas.
El juego está implementado siguiendo los principios de la
programación funcional y declarativa:

  * Inmutabilidad: El estado del juego nunca se modifica,
    se crean nuevas versiones con cada acción.
    
  * Funciones puras: La mayoría de las funciones son puras,
    facilitando el razonamiento y las pruebas.
    
  * Separación de responsabilidades: El código está organizado
    en módulos con responsabilidades claras:
    - Types: Definición de tipos de datos
    - Config: Constantes de configuración
    - Board: Lógica del juego
    - Render: Visualización gráfica
    - Events: Manejo de entrada del usuario

Arquitectura con Gloss
-----------------------
Usamos la librería Gloss que implementa el patrón de arquitectura
"The Elm Architecture" (TEA), muy popular en programación funcional:

  1. Model (World): Estado completo del juego
  2. View (renderWorld): Función pura que dibuja el estado
  3. Update (handleEvent, stepWorld): Funciones que producen nuevos estados

Flujo del juego:
  Splash → Menu → Playing → Won/Lost
                     ↑         │
                     └─────────┘

Uso:
  - Presiona 1, 2 o 3 para seleccionar dificultad
  - Clic izquierdo para revelar una celda
  - Clic derecho para poner/quitar bandera
  - R para reiniciar, M para volver al menú
-}

module Main (main) where

-- Importación de módulos propios
import Types
import Config
import Render (renderWorld, idealWindowSize)
import Events (handleEvent, stepWorld)

-- Importación de librerías externas
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random (StdGen, getStdRandom)
import qualified Data.Map as M

-- ============================================================================
-- PUNTO DE ENTRADA
-- ============================================================================

{-|
  Función principal del programa.
  
  Inicializa el generador de números aleatorios y lanza el juego
  utilizando la función 'play' de Gloss.
  
  La función 'play' recibe:
    1. Configuración de la ventana
    2. Color de fondo
    3. FPS (frames por segundo)
    4. Estado inicial del mundo
    5. Función de renderizado (World -> Picture)
    6. Función de eventos (Event -> World -> World)
    7. Función de actualización (Float -> World -> World)
-}
main :: IO ()
main = do
    -- Obtener el generador de números aleatorios del sistema
    gen <- getStdRandom (\g -> (g, g))
    
    -- Crear el estado inicial del mundo
    let initialState = createInitialWorld gen
    
    -- Calcular el tamaño de la ventana
    let (windowW, windowH) = idealWindowSize initialState
    
    -- Configurar y lanzar el juego
    play
        -- Configuración de la ventana
        (InWindow windowTitle (windowW, windowH) (100, 50))
        -- Color de fondo (se usa el del estado, pero Gloss necesita uno)
        bgColor
        -- Frames por segundo
        fps
        -- Estado inicial
        initialState
        -- Función de renderizado
        renderWorld
        -- Función de manejo de eventos
        handleEvent
        -- Función de actualización por frame
        stepWorld

-- ============================================================================
-- INICIALIZACIÓN
-- ============================================================================

{-|
  Crea el estado inicial del mundo.
  
  El juego comienza en la pantalla de splash (presentación).
  La dificultad por defecto es Easy, pero el jugador puede
  cambiarla en el menú.
  
  @param gen Generador de números aleatorios
  @return Estado inicial del mundo
-}
createInitialWorld :: StdGen -> World
createInitialWorld gen =
    let 
        -- Dificultad por defecto
        defaultDiff = Easy
        (r, c, m) = difficultyParams defaultDiff
    in World
        { phase = Splash          -- Comenzar en pantalla de presentación
        , difficulty = defaultDiff
        , rows = r
        , cols = c
        , minesN = m
        , board = M.empty         -- Tablero vacío (se llena al empezar partida)
        , firstClick = True       -- El primer clic genera las minas
        , rng = gen
        , elapsed = 0             -- Cronómetro en cero
        , splashTime = 0          -- Tiempo en splash
        }
