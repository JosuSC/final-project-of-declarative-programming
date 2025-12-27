# 📋 Reporte Final del Proyecto MATCOMINESWEEPER

## Información del Proyecto

- **Nombre**: MATCOMINESWEEPER
- **Asignatura**: Programación Declarativa
- **Autores**: Josué Javier Senarega Claro (C-311), Ronald Cabrera Martínez (C-311)
- **Fecha**: Diciembre 2025

---

## 1. Resumen de Cambios Realizados

### 1.1 Reorganización de la Estructura del Proyecto

**Antes:**
```
Proyecto Final/
├── main.hs          # Todo el código en un solo archivo (~500 líneas)
├── main.exe         # Ejecutable
└── freeglut.dll     # Dependencia
```

**Después:**
```
Proyecto Final/
├── src/
│   ├── Main.hs      # Punto de entrada (~60 líneas)
│   ├── Types.hs     # Tipos de datos (~130 líneas)
│   ├── Config.hs    # Configuración (~140 líneas)
│   ├── Board.hs     # Lógica del juego (~230 líneas)
│   ├── Render.hs    # Renderizado (~400 líneas)
│   └── Events.hs    # Eventos (~180 líneas)
├── docs/
│   └── REPORTE.md
├── assets/sounds/
├── matcominesweeper.cabal
├── README.md
├── LICENSE
├── run.bat
└── run.sh
```

**¿Por qué este cambio?**
- **Separación de responsabilidades**: Cada módulo tiene una única responsabilidad claramente definida.
- **Mantenibilidad**: Es más fácil encontrar y modificar código específico.
- **Escalabilidad**: Permite añadir nuevas funcionalidades sin afectar otros módulos.
- **Mejor práctica académica**: Demuestra conocimiento de arquitectura de software.

---

### 1.2 Mejoras en la Interfaz Gráfica

#### Pantalla de Splash (NUEVA)
- Añadida pantalla de presentación al inicio del juego
- Muestra el nombre del proyecto, universidad y autores
- Transición automática al menú después de 3 segundos
- Efecto de fade-in animado

#### Menú Rediseñado
- Botones más grandes y legibles
- Información clara de cada dificultad
- Instrucciones de uso visibles
- Paleta de colores moderna y coherente

#### HUD (Head-Up Display)
- Contador de minas restantes con icono
- Contador de banderas colocadas con icono
- Cronómetro en formato MM:SS
- Indicador de dificultad actual

#### Celdas del Tablero
- Efecto 3D en celdas no reveladas
- Iconos mejorados para minas y banderas
- Colores distintos para cada número
- Mejor contraste visual

---

### 1.3 Mejoras en el Código

#### Documentación Exhaustiva
- Cada módulo tiene documentación Haddock
- Cada función importante está comentada
- Explicaciones de por qué existe cada función
- Ejemplos de uso donde corresponde

#### Nombres Semánticos
```haskell
-- Antes:
go :: [Pos] -> Board -> Board

-- Después:
floodReveal :: [Pos] -> Board -> Board
```

#### Funciones Puras
Toda la lógica del juego está en funciones puras. El único código con efectos secundarios está en `Main.main`.

#### Uso Correcto de Tipos
```haskell
-- Tipo para posiciones (claridad semántica)
type Pos = (Int, Int)

-- Tipo para el tablero (eficiencia)
type Board = M.Map Pos Cell

-- ADT para fases del juego (seguridad de tipos)
data Phase = Splash | Menu | Playing | Won | Lost
```

---

### 1.4 Sistema de Construcción con Cabal

**Archivo `matcominesweeper.cabal` creado:**
- Gestión automática de dependencias
- Compilación reproducible
- Metadatos del proyecto
- Opciones de optimización

**Beneficios:**
- `cabal run` compila y ejecuta automáticamente
- Las dependencias se instalan automáticamente
- Portabilidad entre sistemas

---

## 2. Justificación de Decisiones de Diseño

### 2.1 ¿Por qué Data.Map en lugar de listas?

| Operación | Lista [[Cell]] | Data.Map Pos Cell |
|-----------|----------------|-------------------|
| Acceso | O(n) | O(log n) |
| Actualización | O(n) | O(log n) |
| Memoria | Menos overhead | Más overhead |

Para tableros de 16×30 (480 celdas), la diferencia es significativa en operaciones frecuentes como el flood-fill.

### 2.2 ¿Por qué generar minas después del primer clic?

Esta es una característica del Buscaminas original de Windows que mejora la experiencia:
- El primer clic siempre es seguro
- La zona alrededor del primer clic está protegida
- Evita partidas que terminan instantáneamente

### 2.3 ¿Por qué usar Gloss?

| Librería | Pros | Contras |
|----------|------|---------|
| Gloss | API simple, funcional, ideal para juegos 2D | Menos control que OpenGL puro |
| SDL2 | Muy potente, multiplataforma | API imperativa, más compleja |
| Brick | Terminal, muy Haskell | Solo texto |

Gloss es ideal para proyectos educativos: API declarativa que encaja con la filosofía funcional.

### 2.4 Arquitectura TEA (The Elm Architecture)

El patrón Model-View-Update es perfecto para juegos:
- **Model (World)**: Estado inmutable del juego
- **View (renderWorld)**: Función pura que dibuja el estado
- **Update (handleEvent)**: Función pura que produce nuevos estados

Esto hace el código:
- Predecible
- Fácil de debugear
- Fácil de testear

---

## 3. Mejoras Logradas

### 3.1 Rendimiento
- Flood-fill optimizado con BFS (cola) en lugar de recursión
- Uso de Map para acceso O(log n)
- Compilación con `-O2` para optimizaciones

### 3.2 Experiencia de Usuario
- Interfaz más atractiva y moderna
- Feedback visual claro
- Controles intuitivos
- Información útil siempre visible

### 3.3 Calidad del Código
- De 1 archivo de ~500 líneas → 6 módulos organizados
- Documentación completa
- Código idiomático Haskell
- Fácil de entender para profesores

### 3.4 Distribución
- Script `run.bat` para ejecución fácil
- Archivo `.cabal` para gestión de dependencias
- README completo con instrucciones

---

## 4. Guía de Instalación y Ejecución

### Paso 1: Instalar GHCup

GHCup es el instalador recomendado para Haskell. Incluye GHC, Cabal y otras herramientas.

**Windows (PowerShell como Administrador):**
```powershell
Set-ExecutionPolicy Bypass -Scope Process -Force
[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072
Invoke-WebRequest https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1 -OutFile bootstrap-haskell.ps1
.\bootstrap-haskell.ps1
```

Sigue las instrucciones del instalador. Cuando termine, reinicia la terminal.

### Paso 2: Verificar Instalación

```powershell
ghc --version    # Debería mostrar GHC 9.x.x
cabal --version  # Debería mostrar cabal 3.x.x
```

### Paso 3: Descargar el Proyecto

Si aún no tienes el proyecto:
```powershell
git clone <url-del-repositorio>
cd "Proyecto Final"
```

### Paso 4: Actualizar Índice de Paquetes

```powershell
cabal update
```

### Paso 5: Compilar y Ejecutar

**Opción A: Usando el script (más fácil)**
```powershell
.\run.bat
```

**Opción B: Usando Cabal**
```powershell
cabal run matcominesweeper
```

### Paso 6: ¡Jugar!

- Presiona `1`, `2` o `3` para elegir dificultad
- Clic izquierdo para revelar
- Clic derecho para poner bandera
- `R` para reiniciar, `M` para menú

---

## 5. Solución de Errores Comunes

### Error: "unknown GLUT entry glutInit"

**Causa**: Falta la DLL de FreeGLUT.

**Solución**:
1. Verifica que `freeglut.dll` esté en la carpeta del proyecto
2. Si no está, descárgala de: https://www.transmissionzero.co.uk/software/freeglut-devel/
3. Copia `freeglut.dll` a la carpeta del proyecto

### Error: "Could not find module 'Graphics.Gloss'"

**Causa**: Las dependencias no están instaladas.

**Solución**:
```powershell
cabal update
cabal build
```

### Error: "ghc: command not found"

**Causa**: GHC no está en el PATH.

**Solución**:
1. Reinstala GHCup
2. O añade manualmente al PATH: `C:\ghcup\bin`
3. Reinicia la terminal

### El juego se ve muy pequeño/grande

**Solución**: Ajusta `cellSize` en `src/Config.hs`:
```haskell
cellSize :: Float
cellSize = 36  -- Aumentar para celdas más grandes
```

### Los clics no funcionan bien

**Causa**: Puede ser un problema de resolución de pantalla.

**Solución**: Verifica que el DPI de Windows esté al 100%, o ajusta las constantes de tamaño en `Config.hs`.

---

## 6. Posibles Mejoras Futuras

1. **Sonido**: Añadir efectos de sonido usando una librería como SDL2-mixer
2. **Highscores**: Guardar mejores tiempos en un archivo
3. **Temas**: Permitir cambiar la paleta de colores
4. **Tablero personalizado**: Permitir dimensiones custom
5. **Animaciones**: Añadir animaciones de victoria/derrota
6. **Ayuda en juego**: Tutorial interactivo

---

## 7. Conclusiones

Este proyecto demuestra la aplicación práctica de los conceptos de Programación Declarativa:

- **Inmutabilidad**: El estado del juego nunca se modifica, se crean nuevas versiones
- **Funciones puras**: Toda la lógica es predecible y testeable
- **Tipos fuertes**: Los ADTs previenen errores en tiempo de compilación
- **Composición**: Funciones pequeñas se combinan para crear comportamientos complejos
- **Separación de responsabilidades**: Cada módulo tiene un propósito claro

El resultado es un juego funcional, bien documentado y fácil de mantener, que cumple con los estándares esperados de un proyecto final universitario.

---

*Documento generado como parte del Proyecto Final de Programación Declarativa, MATCOM, Universidad de La Habana, 2025.*
