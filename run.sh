#!/bin/bash
# ============================================================================
# MATCOMINESWEEPER - Script de ejecución rápida (Linux/Mac)
# Proyecto Final de Programación Declarativa
# MATCOM - Universidad de La Habana
# ============================================================================

echo ""
echo "╔══════════════════════════════════════════════════════════════╗"
echo "║                    MATCOMINESWEEPER                          ║"
echo "║        Proyecto Final - Programación Declarativa             ║"
echo "╚══════════════════════════════════════════════════════════════╝"
echo ""

# Verificar si cabal está instalado
if ! command -v cabal &> /dev/null; then
    echo "[ERROR] Cabal no está instalado."
    echo "        Por favor, instala GHCup desde: https://www.haskell.org/ghcup/"
    exit 1
fi

# Ejecutar con cabal
echo "[INFO] Compilando y ejecutando el proyecto..."
echo ""
cabal run matcominesweeper
