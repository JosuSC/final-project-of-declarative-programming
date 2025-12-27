@echo off
REM ============================================================================
REM MATCOMINESWEEPER - Script de ejecución rápida
REM Proyecto Final de Programación Declarativa
REM MATCOM - Universidad de La Habana
REM ============================================================================

echo.
echo  ╔══════════════════════════════════════════════════════════════╗
echo  ║                    MATCOMINESWEEPER                          ║
echo  ║        Proyecto Final - Programación Declarativa             ║
echo  ╚══════════════════════════════════════════════════════════════╝
echo.

REM Verificar si existe el ejecutable compilado
if exist "dist-newstyle\build\*\matcominesweeper-*\x\matcominesweeper\build\matcominesweeper\matcominesweeper.exe" (
    echo [INFO] Ejecutando juego compilado...
    echo.
    for /f "delims=" %%i in ('dir /s /b "dist-newstyle\build\*\matcominesweeper.exe" 2^>nul') do (
        "%%i"
        goto :end
    )
)

REM Si no existe, intentar compilar y ejecutar
echo [INFO] Compilando y ejecutando el proyecto...
echo.

REM Verificar si cabal está instalado
where cabal >nul 2>nul
if %ERRORLEVEL% NEQ 0 (
    echo [ERROR] Cabal no está instalado o no está en el PATH.
    echo         Por favor, instala GHCup desde: https://www.haskell.org/ghcup/
    echo.
    pause
    exit /b 1
)

REM Ejecutar con cabal
cabal run matcominesweeper

:end
