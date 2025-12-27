# 🎮 MATCOMINESWEEPER

<div align="center">

![Haskell](https://img.shields.io/badge/Haskell-5D4F85?style=for-the-badge&logo=haskell&logoColor=white)
![Gloss](https://img.shields.io/badge/Gloss-Graphics-blue?style=for-the-badge)
![License](https://img.shields.io/badge/License-MIT-green?style=for-the-badge)
![Universidad](https://img.shields.io/badge/MATCOM-UH-red?style=for-the-badge)

**Proyecto Final de Programación Declarativa**

*Facultad de Matemática y Computación (MATCOM)*  
*Universidad de La Habana*

</div>

---

## 📋 Tabla de Contenidos

- [Descripción](#-descripción)
- [Objetivos Académicos](#-objetivos-académicos)
- [El Juego](#-el-juego)
- [Estructura del Proyecto](#-estructura-del-proyecto)
- [Tecnologías Utilizadas](#-tecnologías-utilizadas)
- [Arquitectura y Diseño](#-arquitectura-y-diseño)
- [Instalación](#-instalación)
- [Ejecución](#-ejecución)
- [Controles](#-controles)
- [Capturas de Pantalla](#-capturas-de-pantalla)
- [Autores](#-autores)

---

## 📖 Descripción

**MATCOMINESWEEPER** es una implementación completa del clásico juego **Buscaminas** (Minesweeper) desarrollada enteramente en **Haskell**. Este proyecto fue creado como trabajo final de la asignatura *Programación Declarativa* del tercer año de la carrera *Ciencia de la Computación*.

El juego recrea fielmente la experiencia del Buscaminas clásico de Windows, pero con una interfaz visual moderna y atractiva, manteniendo todas las mecánicas originales que hicieron famoso a este juego de lógica y deducción.

---

## 🎯 Objetivos Académicos

Este proyecto tiene como objetivos demostrar el dominio de:

### Conceptos de Programación Funcional
- **Inmutabilidad**: El estado del juego nunca se modifica directamente; cada acción produce un nuevo estado.
- **Funciones puras**: La mayoría de las funciones son puras, facilitando el razonamiento y las pruebas.
- **Composición de funciones**: Uso extensivo de composición para construir funcionalidades complejas.
- **Tipos algebraicos**: Modelado del dominio usando ADTs (Algebraic Data Types).

### Programación Declarativa
- **Descripción vs. Instrucción**: El código describe QUÉ queremos lograr, no CÓMO hacerlo paso a paso.
- **Pattern Matching**: Uso extensivo para manejar diferentes casos de forma elegante.
- **Guards y Where**: Organización clara de la lógica condicional.

### Arquitectura de Software
- **Separación de responsabilidades**: Código organizado en módulos con propósitos específicos.
- **The Elm Architecture (TEA)**: Patrón Model-View-Update implementado con Gloss.
- **Código mantenible**: Documentación exhaustiva y nombres semánticos.

---

## 🎮 El Juego

### ¿Qué es el Buscaminas?

El Buscaminas es un juego de lógica donde el objetivo es descubrir todas las celdas que **NO** contienen minas, sin detonar ninguna mina en el proceso.

### Mecánicas del Juego

1. **Revelado de celdas**: Al hacer clic izquierdo en una celda:
   - Si es una **mina**: pierdes el juego
   - Si tiene **minas adyacentes**: muestra un número (1-8)
   - Si está **vacía**: se revela automáticamente junto con todas las celdas vacías conectadas

2. **Sistema de banderas**: Clic derecho para marcar/desmarcar celdas sospechosas de contener minas.

3. **Primer clic seguro**: El primer clic siempre es seguro; las minas se generan después de este.

4. **Victoria**: Se gana al revelar todas las celdas que no son minas.

### Niveles de Dificultad

| Nivel | Tablero | Minas | Densidad |
|-------|---------|-------|----------|
| 🟢 Fácil | 9 × 9 | 10 | 12.3% |
| 🟡 Medio | 16 × 16 | 40 | 15.6% |
| 🔴 Difícil | 16 × 30 | 99 | 20.6% |

---

## 📁 Estructura del Proyecto

```
MATCOMINESWEEPER/
├── 📄 matcominesweeper.cabal   # Configuración del proyecto
├── 📄 LICENSE                   # Licencia MIT
├── 📄 README.md                 # Este archivo
├── 📄 run.bat                   # Script de ejecución (Windows)
├── 📄 run.sh                    # Script de ejecución (Linux/Mac)
├── 📄 freeglut.dll              # Librería gráfica (Windows)
│
├── 📂 src/                      # Código fuente
│   ├── 📄 Main.hs               # Punto de entrada
│   ├── 📄 Types.hs              # Definiciones de tipos
│   ├── 📄 Config.hs             # Configuración visual
│   ├── 📄 Board.hs              # Lógica del tablero
│   ├── 📄 Render.hs             # Renderizado gráfico
│   └── 📄 Events.hs             # Manejo de eventos
│
├── 📂 docs/                     # Documentación adicional
│   └── 📄 REPORTE.md            # Reporte del proyecto
│
└── 📂 assets/                   # Recursos (futuro)
    └── 📂 sounds/               # Efectos de sonido
```

### Descripción de Módulos

| Módulo | Responsabilidad |
|--------|-----------------|
| `Main.hs` | Inicialización del juego y loop principal |
| `Types.hs` | Definición de todos los tipos de datos (Cell, Board, World, etc.) |
| `Config.hs` | Constantes de configuración: colores, tamaños, tiempos |
| `Board.hs` | Lógica del juego: generación de minas, revelado, victoria |
| `Render.hs` | Todo el renderizado gráfico con Gloss |
| `Events.hs` | Manejo de eventos de teclado y mouse |

---

## 🛠 Tecnologías Utilizadas

### Lenguaje
- **Haskell** (GHC 9.x) - Lenguaje funcional puro

### Librerías
| Librería | Versión | Propósito |
|----------|---------|-----------|
| `base` | ≥ 4.14 | Biblioteca estándar de Haskell |
| `gloss` | ≥ 1.13 | Gráficos 2D de alto nivel |
| `random` | ≥ 1.2 | Generación de números aleatorios |
| `containers` | ≥ 0.6 | Estructuras de datos (Data.Map) |

### Herramientas
- **Cabal** - Sistema de construcción y gestión de dependencias
- **GHCup** - Instalador de herramientas Haskell

---

## 🏗 Arquitectura y Diseño

### The Elm Architecture (TEA)

El juego implementa el patrón TEA, muy popular en programación funcional:

```
┌─────────────┐     ┌─────────────┐     ┌─────────────┐
│    Model    │────▶│    View     │────▶│   Picture   │
│   (World)   │     │(renderWorld)│     │  (Pantalla) │
└─────────────┘     └─────────────┘     └─────────────┘
       ▲                                       │
       │                                       │
       │           ┌─────────────┐             │
       └───────────│   Update    │◀────────────┘
                   │(handleEvent)│   (Eventos)
                   └─────────────┘
```

### Decisiones de Diseño

1. **Data.Map para el tablero**: Elegimos `Map` sobre listas o arrays por su eficiencia O(log n) en búsquedas y su naturaleza inmutable.

2. **Generación de minas diferida**: Las minas se generan después del primer clic para garantizar que siempre sea seguro.

3. **Flood-fill con BFS**: El revelado en cascada usa una cola para evitar stack overflow en tableros grandes.

4. **Separación estricta UI/Lógica**: La lógica del juego no conoce nada sobre gráficos, facilitando testing y mantenimiento.

---

## 📥 Instalación

### Prerrequisitos

1. **GHCup** (recomendado) - Instala desde: https://www.haskell.org/ghcup/

   ```powershell
   # Windows (PowerShell como Administrador)
   Set-ExecutionPolicy Bypass -Scope Process -Force
   [System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072
   Invoke-WebRequest https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1 -OutFile bootstrap-haskell.ps1
   .\bootstrap-haskell.ps1
   ```

2. **FreeGLUT** (Windows) - La DLL `freeglut.dll` ya está incluida en el proyecto.

### Instalación de Dependencias

```bash
# Actualizar índice de paquetes
cabal update

# Las dependencias se instalarán automáticamente al compilar
```

---

## 🚀 Ejecución

### Método Rápido (Recomendado)

**Windows:**
```cmd
run.bat
```

**Linux/Mac:**
```bash
chmod +x run.sh
./run.sh
```

### Con Cabal

```bash
# Compilar y ejecutar
cabal run matcominesweeper

# Solo compilar
cabal build

# Compilar con optimizaciones
cabal build --enable-optimization=2
```

### Compilación Manual (GHC)

```bash
cd src
ghc -O2 -o ../matcominesweeper Main.hs
cd ..
./matcominesweeper
```

---

## 🎹 Controles

| Acción | Control |
|--------|---------|
| Revelar celda | Clic izquierdo |
| Poner/quitar bandera | Clic derecho |
| Seleccionar Fácil | Tecla `1` |
| Seleccionar Medio | Tecla `2` |
| Seleccionar Difícil | Tecla `3` |
| Reiniciar partida | Tecla `R` |
| Volver al menú | Tecla `M` |

---

## 📸 Capturas de Pantalla

### Pantalla de Inicio
```
╔══════════════════════════════════════╗
║         MATCOMINESWEEPER             ║
║                                      ║
║   Proyecto Final de Programación     ║
║         Declarativa                  ║
║                                      ║
║      MATCOM — Universidad de         ║
║           La Habana                  ║
║                                      ║
║           Cargando...                ║
╚══════════════════════════════════════╝
```

### Menú Principal
```
╔══════════════════════════════════════╗
║         MATCOMINESWEEPER             ║
║     Selecciona la dificultad         ║
║                                      ║
║   ┌────────────────────────┐         ║
║   │   FÁCIL  (9×9, 10)     │         ║
║   └────────────────────────┘         ║
║   ┌────────────────────────┐         ║
║   │   MEDIO  (16×16, 40)   │         ║
║   └────────────────────────┘         ║
║   ┌────────────────────────┐         ║
║   │  DIFÍCIL (16×30, 99)   │         ║
║   └────────────────────────┘         ║
║                                      ║
║   Presiona 1, 2 o 3 para comenzar    ║
╚══════════════════════════════════════╝
```

### Durante el Juego
```
╔══════════════════════════════════════╗
║  💣 8   🚩 3   Medio   ⏱ 01:23      ║
╠══════════════════════════════════════╣
║  ▓ ▓ 1 · · · 1 ▓ ▓                  ║
║  ▓ 2 1 · · · 1 2 ▓                  ║
║  1 1 · · · · · 1 1                  ║
║  · · · · · · · · ·                  ║
║  · · · 1 1 1 · · ·                  ║
║  · · · 1 🚩 1 · · ·                  ║
║  ▓ ▓ ▓ ▓ ▓ ▓ ▓ ▓ ▓                  ║
╚══════════════════════════════════════╝
```

---

## 👨‍💻 Autores

<table>
<tr>
<td align="center">
<strong>Josué Javier Senarega Claro</strong><br>
Grupo C-311<br>
3er Año - Ciencia de la Computación<br>
MATCOM, Universidad de La Habana
</td>
<td align="center">
<strong>Ronald Cabrera Martínez</strong><br>
Grupo C-311<br>
3er Año - Ciencia de la Computación<br>
MATCOM, Universidad de La Habana
</td>
</tr>
</table>

---

## 🏛 Información Académica

- **Asignatura**: Programación Declarativa
- **Carrera**: Ciencia de la Computación
- **Año**: 3er año (2024-2025)
- **Facultad**: Matemática y Computación (MATCOM)
- **Universidad**: Universidad de La Habana

---

## 📄 Licencia

Este proyecto está licenciado bajo la Licencia MIT. Ver el archivo [LICENSE](LICENSE) para más detalles.

---

<div align="center">

**Hecho con ❤️ y Haskell en La Habana, Cuba**

*"Lo que no es funcional, no funciona"*

</div>
