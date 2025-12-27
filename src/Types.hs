{-|
Module      : Types
Description : Definición de tipos de datos para MATCOMINESWEEPER
Copyright   : (c) Josué Javier Senarega Claro, Ronald Cabrera Martínez, 2025
License     : MIT
Maintainer  : Estudiantes C-311, MATCOM, Universidad de La Habana

Este módulo contiene todas las definiciones de tipos de datos utilizados
en el juego Buscaminas. Separar los tipos en un módulo aparte permite:
  - Mayor claridad en la estructura del código
  - Reutilización de tipos en diferentes módulos
  - Facilidad de mantenimiento y extensión
-}

{-# LANGUAGE RecordWildCards #-}

module Types 
    ( -- * Tipos básicos del tablero
      Pos
    , Cell(..)
    , Board
      -- * Fases del juego
    , Phase(..)
      -- * Niveles de dificultad  
    , Difficulty(..)
      -- * Estado del mundo
    , World(..)
      -- * Funciones auxiliares de dificultad
    , difficultyParams
    , difficultyName
    ) where

import qualified Data.Map as M
import System.Random (StdGen)

-- ============================================================================
-- TIPOS BÁSICOS
-- ============================================================================

{-|
  Posición en el tablero representada como (fila, columna).
  Usamos un tipo alias para mayor claridad semántica.
  - La fila 0 está en la parte superior del tablero
  - La columna 0 está en el extremo izquierdo
-}
type Pos = (Int, Int)

{-|
  Representa una celda individual del tablero de Buscaminas.
  Cada celda tiene las siguientes propiedades:
  
  - 'isMine': Indica si la celda contiene una mina
  - 'isRevealed': Indica si la celda ha sido descubierta por el jugador
  - 'isFlagged': Indica si el jugador ha colocado una bandera
  - 'adjMines': Número de minas adyacentes (8 direcciones). Si es -1, la celda es mina
-}
data Cell = Cell
    { isMine     :: Bool  -- ^ ¿Es una mina?
    , isRevealed :: Bool  -- ^ ¿Ha sido revelada?
    , isFlagged  :: Bool  -- ^ ¿Tiene bandera?
    , adjMines   :: Int   -- ^ Cantidad de minas adyacentes
    } deriving (Show, Eq)

{-|
  El tablero es un mapa de posiciones a celdas.
  Usamos Data.Map por su eficiencia O(log n) en búsquedas
  y su naturaleza inmutable que facilita el razonamiento funcional.
-}
type Board = M.Map Pos Cell

-- ============================================================================
-- FASES DEL JUEGO
-- ============================================================================

{-|
  Representa las diferentes fases o estados en los que puede estar el juego.
  El flujo típico es: Splash -> Menu -> Playing -> (Won | Lost)
  
  - 'Splash': Pantalla de presentación inicial
  - 'Menu': Menú de selección de dificultad
  - 'Playing': Partida en curso
  - 'Won': El jugador ha ganado (reveló todas las celdas seguras)
  - 'Lost': El jugador ha perdido (reveló una mina)
-}
data Phase
    = Splash   -- ^ Pantalla de presentación
    | Menu     -- ^ Menú de selección de dificultad
    | Playing  -- ^ Jugando
    | Won      -- ^ Victoria
    | Lost     -- ^ Derrota
    deriving (Eq, Show)

-- ============================================================================
-- NIVELES DE DIFICULTAD
-- ============================================================================

{-|
  Los tres niveles de dificultad estándar del Buscaminas clásico.
  Cada nivel tiene diferentes dimensiones de tablero y cantidad de minas.
-}
data Difficulty 
    = Easy    -- ^ Fácil: 9x9 con 10 minas
    | Medium  -- ^ Medio: 16x16 con 40 minas
    | Hard    -- ^ Difícil: 16x30 con 99 minas
    deriving (Eq, Show)

{-|
  Devuelve los parámetros de configuración para cada dificultad.
  
  @return Una tupla (filas, columnas, cantidad_de_minas)
  
  Ejemplos:
  
  >>> difficultyParams Easy
  (9, 9, 10)
  
  >>> difficultyParams Hard
  (16, 30, 99)
-}
difficultyParams :: Difficulty -> (Int, Int, Int)
difficultyParams Easy   = (9, 9, 10)     -- Tablero pequeño, pocas minas
difficultyParams Medium = (16, 16, 40)   -- Tablero mediano, dificultad moderada
difficultyParams Hard   = (16, 30, 99)   -- Tablero grande, muchas minas

{-|
  Devuelve el nombre en español de cada dificultad.
  Útil para mostrar en la interfaz gráfica.
-}
difficultyName :: Difficulty -> String
difficultyName Easy   = "Fácil"
difficultyName Medium = "Medio"
difficultyName Hard   = "Difícil"

-- ============================================================================
-- ESTADO DEL MUNDO
-- ============================================================================

{-|
  Estado completo del juego en un momento dado.
  Este es el tipo principal que contiene toda la información necesaria
  para renderizar el juego y procesar eventos.
  
  Siguiendo el patrón de arquitectura de Gloss, el estado es inmutable
  y se actualiza mediante funciones puras que devuelven un nuevo estado.
-}
data World = World
    { phase        :: Phase       -- ^ Fase actual del juego
    , difficulty   :: Difficulty  -- ^ Dificultad seleccionada
    , rows         :: Int         -- ^ Número de filas del tablero
    , cols         :: Int         -- ^ Número de columnas del tablero
    , minesN       :: Int         -- ^ Cantidad total de minas
    , board        :: Board       -- ^ Estado del tablero
    , firstClick   :: Bool        -- ^ ¿Es el primer clic? (para generar minas)
    , rng          :: StdGen      -- ^ Generador de números aleatorios
    , elapsed      :: Float       -- ^ Tiempo transcurrido en segundos
    , splashTime   :: Float       -- ^ Tiempo en pantalla de splash
    }
