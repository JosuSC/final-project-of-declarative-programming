# Buscaminas (Haskell + Gloss)

Juego de Buscaminas hecho en Haskell usando **Gloss**.

## Requisitos

- Windows
- GHC (probado con **9.6.x**)
- (Opcional) VS Code

> Nota: Gloss depende de OpenGL en Windows.

## Compilar

En PowerShell, dentro de la carpeta del proyecto:

```powershell
ghc -O2 main.hs -o main.exe
```

## Ejecutar

```powershell
.\main.exe
```

## Controles

- `1` / `2` / `3`: seleccionar dificultad
- Click izquierdo: revelar
- Click derecho: bandera
- `R`: reiniciar
- `M`: volver al menú

## VS Code (tareas)

- **Build (GHC)**: compila el proyecto
- **Run (Game)**: ejecuta el juego

Se corren desde `Ctrl+Shift+P` → `Tasks: Run Task`.
