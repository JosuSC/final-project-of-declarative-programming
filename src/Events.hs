{-|
Module      : Events
Description : Manejo de eventos para MATCOMINESWEEPER
Copyright   : (c) Josué Javier Senarega Claro, Ronald Cabrera Martínez, 2025
License     : MIT
Maintainer  : Estudiantes C-311, MATCOM, Universidad de La Habana

Este módulo se encarga del manejo de todos los eventos del juego:
  - Eventos de teclado (selección de dificultad, reinicio, menú)
  - Eventos de mouse (revelar celdas, colocar banderas)
  
La separación del manejo de eventos en un módulo independiente
permite mantener la lógica de interacción clara y organizada.
-}

{-# LANGUAGE RecordWildCards #-}

module Events 
    ( -- * Manejo de eventos
      handleEvent
      -- * Actualización del estado
    , stepWorld
    ) where

import Types
import Config
import Board
import Render (mouseToCell)
import Graphics.Gloss.Interface.IO.Game
import qualified Data.Map as M

-- ============================================================================
-- MANEJO DE EVENTOS
-- ============================================================================

{-|
  Función principal de manejo de eventos.
  
  Recibe un evento de Gloss y el estado actual del mundo,
  y devuelve el nuevo estado del mundo.
  
  El comportamiento depende de la fase actual del juego:
  - Splash: cualquier tecla o clic avanza al menú
  - Menu: teclas 1/2/3 seleccionan dificultad
  - Playing: mouse para jugar, R para reiniciar, M para menú
  - Won/Lost: R para reiniciar, M para menú
-}
handleEvent :: Event -> World -> World

-- ============================================================================
-- EVENTOS EN PANTALLA SPLASH
-- ============================================================================

-- Cualquier tecla o clic en splash -> ir al menú
handleEvent (EventKey _ Down _ _) w@World{phase = Splash} = 
    w { phase = Menu }

-- ============================================================================
-- EVENTOS EN MENÚ
-- ============================================================================

-- Tecla 1: seleccionar dificultad Fácil
handleEvent (EventKey (Char '1') Down _ _) w@World{phase = Menu} = 
    startGame Easy w

-- Tecla 2: seleccionar dificultad Media
handleEvent (EventKey (Char '2') Down _ _) w@World{phase = Menu} = 
    startGame Medium w

-- Tecla 3: seleccionar dificultad Difícil
handleEvent (EventKey (Char '3') Down _ _) w@World{phase = Menu} = 
    startGame Hard w

-- Teclas de atajo adicionales para accesibilidad
handleEvent (EventKey (SpecialKey KeyF1) Down _ _) w@World{phase = Menu} = 
    startGame Easy w
handleEvent (EventKey (SpecialKey KeyF2) Down _ _) w@World{phase = Menu} = 
    startGame Medium w
handleEvent (EventKey (SpecialKey KeyF3) Down _ _) w@World{phase = Menu} = 
    startGame Hard w

-- ============================================================================
-- EVENTOS DURANTE EL JUEGO
-- ============================================================================

-- Clic izquierdo: revelar celda
handleEvent (EventKey (MouseButton LeftButton) Down _ mousePos) w@World{phase = Playing} =
    handleLeftClick mousePos w

-- Clic derecho: colocar/quitar bandera
handleEvent (EventKey (MouseButton RightButton) Down _ mousePos) w@World{phase = Playing} =
    handleRightClick mousePos w

-- Tecla R: reiniciar partida (minúscula o mayúscula)
handleEvent (EventKey (Char 'r') Down _ _) w@World{phase = Playing} = 
    restartGame w
handleEvent (EventKey (Char 'R') Down _ _) w@World{phase = Playing} = 
    restartGame w

-- Tecla M: volver al menú (minúscula o mayúscula)
handleEvent (EventKey (Char 'm') Down _ _) w@World{phase = Playing} = 
    goToMenu w
handleEvent (EventKey (Char 'M') Down _ _) w@World{phase = Playing} = 
    goToMenu w

-- ============================================================================
-- EVENTOS EN PANTALLAS DE FIN (VICTORIA/DERROTA)
-- ============================================================================

-- Tecla R: reiniciar partida
handleEvent (EventKey (Char 'r') Down _ _) w@World{phase = Won} = restartGame w
handleEvent (EventKey (Char 'R') Down _ _) w@World{phase = Won} = restartGame w
handleEvent (EventKey (Char 'r') Down _ _) w@World{phase = Lost} = restartGame w
handleEvent (EventKey (Char 'R') Down _ _) w@World{phase = Lost} = restartGame w

-- Tecla M: volver al menú
handleEvent (EventKey (Char 'm') Down _ _) w@World{phase = Won} = goToMenu w
handleEvent (EventKey (Char 'M') Down _ _) w@World{phase = Won} = goToMenu w
handleEvent (EventKey (Char 'm') Down _ _) w@World{phase = Lost} = goToMenu w
handleEvent (EventKey (Char 'M') Down _ _) w@World{phase = Lost} = goToMenu w

-- ============================================================================
-- EVENTOS NO MANEJADOS
-- ============================================================================

-- Cualquier otro evento: ignorar
handleEvent _ w = w

-- ============================================================================
-- FUNCIONES AUXILIARES DE EVENTOS
-- ============================================================================

{-|
  Maneja el clic izquierdo del mouse.
  
  Si es el primer clic de la partida, genera el tablero asegurando
  que la celda clickeada sea segura. Luego revela la celda.
-}
handleLeftClick :: (Float, Float) -> World -> World
handleLeftClick mousePos w@World{..} =
    case mouseToCell w mousePos of
        Nothing -> w  -- Clic fuera del tablero
        Just pos ->
            if firstClick
                then
                    -- Primer clic: generar tablero y revelar
                    let (newBoard, newRng) = placeMines rng w pos
                        w' = w { board = newBoard
                               , rng = newRng
                               , firstClick = False
                               }
                    in revealAt pos w'
                else
                    -- Clics posteriores: solo revelar
                    revealAt pos w

{-|
  Maneja el clic derecho del mouse.
  Alterna la bandera en la celda clickeada.
-}
handleRightClick :: (Float, Float) -> World -> World
handleRightClick mousePos w =
    case mouseToCell w mousePos of
        Nothing -> w  -- Clic fuera del tablero
        Just pos -> toggleFlag pos w

-- ============================================================================
-- FUNCIONES DE TRANSICIÓN DE ESTADO
-- ============================================================================

{-|
  Inicia una nueva partida con la dificultad especificada.
  
  Crea un tablero vacío (las minas se colocarán en el primer clic)
  y resetea todos los contadores.
-}
startGame :: Difficulty -> World -> World
startGame diff w@World{..} =
    let 
        (r, c, m) = difficultyParams diff
        newBoard = emptyBoard r c
    in w { phase = Playing
         , difficulty = diff
         , rows = r
         , cols = c
         , minesN = m
         , board = newBoard
         , firstClick = True
         , elapsed = 0
         }

{-|
  Reinicia la partida actual con la misma dificultad.
-}
restartGame :: World -> World
restartGame w@World{..} = startGame difficulty w

{-|
  Vuelve al menú principal.
  Mantiene la dificultad seleccionada para conveniencia del jugador.
-}
goToMenu :: World -> World
goToMenu w@World{..} =
    let 
        (r, c, m) = difficultyParams difficulty
    in w { phase = Menu
         , rows = r
         , cols = c
         , minesN = m
         , board = emptyBoard r c
         , firstClick = True
         , elapsed = 0
         }

-- ============================================================================
-- ACTUALIZACIÓN DEL ESTADO
-- ============================================================================

{-|
  Función de actualización del mundo llamada cada frame.
  
  @param dt Delta time (tiempo transcurrido desde el último frame)
  @param w  Estado actual del mundo
  @return   Nuevo estado del mundo
  
  Esta función se encarga de:
  - Actualizar el tiempo transcurrido durante la partida
  - Manejar la transición automática del splash al menú
  - Cualquier animación o lógica basada en tiempo
-}
stepWorld :: Float -> World -> World
stepWorld dt w@World{..} =
    case phase of
        -- En splash: contar tiempo y cambiar a menú automáticamente
        Splash ->
            let newSplashTime = splashTime + dt
            in if newSplashTime >= splashDuration
                   then w { phase = Menu, splashTime = newSplashTime }
                   else w { splashTime = newSplashTime }
        
        -- Durante el juego: actualizar el cronómetro
        Playing -> 
            w { elapsed = elapsed + dt }
        
        -- En otras fases: no hacer nada especial
        _ -> w
