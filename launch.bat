@echo off
setlocal

echo === Zenodo BibTeX Importer ===
echo.

:: Check for Python
python --version >nul 2>&1
if errorlevel 1 (
    echo ERROR: Python not found. Please install Python 3.10+ from https://python.org
    pause
    exit /b 1
)

:: Create venv if it doesn't exist
if not exist ".venv" (
    echo Creating virtual environment...
    python -m venv .venv
)

:: Activate venv
call .venv\Scripts\activate.bat

:: Install / upgrade dependencies
echo Installing dependencies...
pip install -q -r requirements.txt

:: Launch the app
echo.
echo Starting Zenodo Importer in your browser...
echo (Press Ctrl+C in this window to stop the app)
echo.
streamlit run zenodo_importer.py --server.headless false

endlocal
