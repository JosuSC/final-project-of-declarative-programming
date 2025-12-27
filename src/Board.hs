{-|
Module      : Board
Description : Lógica del tablero para MATCOMINESWEEPER
Copyright   : (c) Josué Javier Senarega Claro, Ronald Cabrera Martínez, 2025
License     : MIT
Maintainer  : Estudiantes C-311, MATCOM, Universidad de La Habana

Este módulo implementa toda la lógica del tablero de Buscaminas:
  - Generación de tableros vacíos
  - Colocación aleatoria de minas
  - Cálculo de minas adyacentes
  - Revelado de celdas (incluyendo flood-fill)
  - Manejo de banderas
  - Detección de victoria/derrota

La lógica está completamente separada de la representación gráfica,
siguiendo el principio de separación de responsabilidades.
-}

{-# LANGUAGE RecordWildCards #-}

module Board 
    ( -- * Construcción del tablero
      emptyBoard
    , placeMines
      -- * Operaciones sobre el tablero
    , revealAt
    , toggleFlag
      -- * Verificaciones
    , checkWin
    , inBounds
      -- * Utilidades
    , neighbors
    , countFlags
    , countRemainingMines
    ) where

import Types
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import System.Random

-- ============================================================================
-- CONSTRUCCIÓN DEL TABLERO
-- ============================================================================

{-|
  Crea un tablero vacío (sin minas) del tamaño especificado en el World.
  Todas las celdas comienzan:
    - Sin mina
    - No reveladas
    - Sin bandera
    - Con 0 minas adyacentes
  
  Esta función se usa al iniciar una nueva partida antes de colocar las minas.
-}
emptyBoard :: Int -> Int -> Board
emptyBoard numRows numCols = M.fromList
    [ ((r, c), Cell False False False 0) 
    | r <- [0..numRows-1]
    , c <- [0..numCols-1] 
    ]

{-|
  Coloca las minas en el tablero de forma aleatoria.
  
  IMPORTANTE: La primera celda clickeada y sus vecinas están protegidas
  (no tendrán minas). Esto garantiza que el primer clic siempre sea seguro,
  una característica estándar del Buscaminas clásico.
  
  @param gen     Generador de números aleatorios
  @param world   Estado actual del mundo (para obtener dimensiones)
  @param safePos Posición del primer clic (protegida)
  @return        Tupla con el nuevo tablero y el generador actualizado
-}
placeMines :: StdGen -> World -> Pos -> (Board, StdGen)
placeMines gen w@World{..} safePos =
    let 
        -- Todas las posiciones posibles en el tablero
        allPositions = [(r, c) | r <- [0..rows-1], c <- [0..cols-1]]
        
        -- Posiciones candidatas: excluimos la celda clickeada y sus vecinas
        -- Esto garantiza que el primer clic siempre revele un área segura
        candidates = filter (\p -> p /= safePos && notElem p (neighbors safePos)) allPositions
        
        -- Seleccionamos aleatoriamente las posiciones para las minas
        (chosenPositions, gen') = chooseUnique gen minesN candidates
        
        -- Creamos un tablero vacío
        emptyB = emptyBoard rows cols
        
        -- Colocamos las minas en las posiciones elegidas
        boardWithMines = foldr placeMineAt emptyB chosenPositions
        
        -- Calculamos los números de minas adyacentes para cada celda
        finalBoard = computeAdjacentMines boardWithMines rows cols
        
    in (finalBoard, gen')
  where
    -- Función auxiliar para colocar una mina en una posición
    placeMineAt :: Pos -> Board -> Board
    placeMineAt pos = M.adjust (\cell -> cell { isMine = True }) pos

{-|
  Calcula el número de minas adyacentes para cada celda del tablero.
  
  Para cada celda que NO es mina, cuenta cuántas de sus 8 celdas
  vecinas contienen minas. Este número se muestra al revelar la celda
  y es la información principal que usa el jugador para deducir
  dónde están las minas.
-}
computeAdjacentMines :: Board -> Int -> Int -> Board
computeAdjacentMines b numRows numCols =
    M.mapWithKey updateCell b
  where
    updateCell :: Pos -> Cell -> Cell
    updateCell pos cell
        | isMine cell = cell { adjMines = -1 }  -- Las minas tienen -1 (convención)
        | otherwise   = cell { adjMines = countAdjacentMines pos }
    
    countAdjacentMines :: Pos -> Int
    countAdjacentMines pos = 
        length $ filter hasMine $ neighbors pos
      where
        hasMine :: Pos -> Bool
        hasMine p = fromMaybe False (isMine <$> M.lookup p b)

{-|
  Selecciona n elementos únicos de una lista de forma aleatoria.
  
  Usamos el algoritmo de Fisher-Yates parcial:
  1. Elegimos un elemento aleatorio
  2. Lo removemos de la lista de candidatos
  3. Repetimos hasta tener n elementos
  
  @param gen  Generador de números aleatorios
  @param n    Cantidad de elementos a seleccionar
  @param xs   Lista de candidatos
  @return     Tupla con los elementos seleccionados y el nuevo generador
-}
chooseUnique :: StdGen -> Int -> [Pos] -> ([Pos], StdGen)
chooseUnique gen n xs = go gen n xs []
  where
    go :: StdGen -> Int -> [Pos] -> [Pos] -> ([Pos], StdGen)
    go g k pool acc
        | k <= 0 || null pool = (acc, g)
        | otherwise =
            let (idx, g') = randomR (0, length pool - 1) g
                picked = pool !! idx
                pool' = take idx pool ++ drop (idx + 1) pool
            in go g' (k - 1) pool' (picked : acc)

-- ============================================================================
-- OPERACIONES SOBRE EL TABLERO
-- ============================================================================

{-|
  Revela una celda en la posición indicada.
  
  Esta es una de las funciones principales del juego. El comportamiento es:
  
  1. Si la posición está fuera del tablero -> no hacer nada
  2. Si la celda tiene bandera -> no hacer nada (protección)
  3. Si la celda ya está revelada -> no hacer nada
  4. Si la celda es una mina -> revelarla y PERDER
  5. Si la celda tiene 0 minas adyacentes -> flood-fill (revelar área)
  6. Si la celda tiene minas adyacentes -> revelar solo esa celda
  
  @param pos   Posición a revelar
  @param world Estado actual del mundo
  @return      Nuevo estado del mundo
-}
revealAt :: Pos -> World -> World
revealAt pos w@World{..}
    -- Verificar que la posición es válida
    | not (inBounds w pos) = w
    | otherwise =
        case M.lookup pos board of
            Nothing -> w  -- No debería pasar, pero por seguridad
            Just cell
                -- No revelar celdas con bandera (protección del jugador)
                | isFlagged cell -> w
                
                -- No revelar celdas ya reveladas
                | isRevealed cell -> w
                
                -- ¡MINA! El jugador pierde
                | isMine cell -> 
                    let newBoard = M.adjust (\c -> c { isRevealed = True }) pos board
                    in w { board = newBoard, phase = Lost }
                
                -- Celda con 0 minas adyacentes: hacer flood-fill
                | adjMines cell == 0 ->
                    let newBoard = floodReveal [pos] board
                    in checkWin w { board = newBoard }
                
                -- Celda con minas adyacentes: revelar solo esta
                | otherwise ->
                    let newBoard = M.adjust (\c -> c { isRevealed = True }) pos board
                    in checkWin w { board = newBoard }

{-|
  Algoritmo de revelado en cascada (flood-fill).
  
  Cuando el jugador revela una celda con 0 minas adyacentes,
  automáticamente se revelan todas las celdas conectadas que también
  tienen 0 minas, junto con sus bordes (que muestran números).
  
  Implementación con cola (BFS) para evitar stack overflow:
  1. Tomamos una posición de la cola
  2. Si está revelada o tiene bandera, la saltamos
  3. La revelamos
  4. Si tiene 0 minas adyacentes, agregamos sus vecinos a la cola
  5. Repetimos hasta que la cola esté vacía
-}
floodReveal :: [Pos] -> Board -> Board
floodReveal queue b = go queue b
  where
    go :: [Pos] -> Board -> Board
    go [] board = board
    go (pos:rest) board =
        case M.lookup pos board of
            Nothing -> go rest board
            Just cell
                -- Saltar celdas ya procesadas o con bandera
                | isRevealed cell || isFlagged cell -> go rest board
                -- No revelar minas (por seguridad)
                | isMine cell -> go rest board
                | otherwise ->
                    let 
                        -- Revelar la celda actual
                        revealed = cell { isRevealed = True }
                        board' = M.insert pos revealed board
                    in 
                        -- Si tiene 0 minas, agregar vecinos a la cola
                        if adjMines revealed == 0
                            then go (neighbors pos ++ rest) board'
                            else go rest board'

{-|
  Alterna la bandera en una celda.
  
  Las banderas sirven para que el jugador marque las celdas que
  cree que contienen minas. Características:
  
  - Solo se pueden poner en celdas no reveladas
  - Al hacer clic derecho se alterna (poner/quitar)
  - Las celdas con bandera no se pueden revelar (protección)
  
  @param pos   Posición donde alternar la bandera
  @param world Estado actual del mundo
  @return      Nuevo estado del mundo
-}
toggleFlag :: Pos -> World -> World
toggleFlag pos w@World{..}
    -- Solo permitir banderas durante el juego
    | phase /= Playing = w
    -- Verificar posición válida
    | not (inBounds w pos) = w
    | otherwise =
        case M.lookup pos board of
            Nothing -> w
            Just cell
                -- No se puede poner bandera en celdas reveladas
                | isRevealed cell -> w
                -- Alternar la bandera
                | otherwise -> 
                    let newBoard = M.adjust (\c -> c { isFlagged = not (isFlagged c) }) pos board
                    in w { board = newBoard }

-- ============================================================================
-- VERIFICACIONES
-- ============================================================================

{-|
  Verifica si el jugador ha ganado.
  
  Condición de victoria: todas las celdas que NO son minas
  han sido reveladas. No es necesario que todas las minas
  tengan banderas.
  
  @param world Estado actual del mundo
  @return      World con phase = Won si se ganó, sin cambios si no
-}
checkWin :: World -> World
checkWin w@World{..} =
    let 
        allCells = M.elems board
        -- Verificar si hay alguna celda segura sin revelar
        hasUnrevealedSafe = any (\c -> not (isMine c) && not (isRevealed c)) allCells
    in 
        if hasUnrevealedSafe 
            then w 
            else w { phase = Won }

{-|
  Verifica si una posición está dentro de los límites del tablero.
  
  @param world Estado del mundo (para obtener dimensiones)
  @param pos   Posición a verificar
  @return      True si la posición es válida
-}
inBounds :: World -> Pos -> Bool
inBounds World{..} (r, c) = 
    r >= 0 && r < rows && c >= 0 && c < cols

-- ============================================================================
-- UTILIDADES
-- ============================================================================

{-|
  Devuelve las 8 posiciones vecinas de una celda.
  
  En el Buscaminas se consideran 8 direcciones:
  - 4 cardinales (arriba, abajo, izquierda, derecha)
  - 4 diagonales
  
  No se verifica si las posiciones están dentro del tablero;
  eso se hace en las funciones que usan neighbors.
-}
neighbors :: Pos -> [Pos]
neighbors (r, c) = 
    [ (r + dr, c + dc) 
    | dr <- [-1..1]
    , dc <- [-1..1]
    , (dr, dc) /= (0, 0)  -- Excluir la celda central
    ]

{-|
  Cuenta el número de banderas colocadas en el tablero.
  Útil para mostrar al jugador cuántas banderas ha usado.
-}
countFlags :: Board -> Int
countFlags = length . filter isFlagged . M.elems

{-|
  Calcula cuántas minas "faltan" por marcar.
  Es simplemente: total_minas - banderas_colocadas
  
  Nota: Puede ser negativo si el jugador puso más banderas que minas.
-}
countRemainingMines :: Int -> Board -> Int
countRemainingMines totalMines b = totalMines - countFlags b
