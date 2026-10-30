# -*- mode: python ; coding: utf-8 -*-
"""
PyInstaller spec for Zenodo BibTeX Importer (CustomTkinter edition).

Build command:
    pyinstaller zenodo_importer.spec
"""

from pathlib import Path
import customtkinter

# CustomTkinter bundles its own theme/asset files
CTK_DIR = Path(customtkinter.__file__).parent

block_cipher = None

a = Analysis(
    ["run.py"],
    pathex=[],
    binaries=[],
    datas=[
        # CustomTkinter assets (themes, images)
        (str(CTK_DIR / "assets"), "customtkinter/assets"),
    ],
    hiddenimports=[
        "customtkinter",
        "bibtexparser",
        "bibtexparser.model",
        "bibtexparser.splitter",
        "bibtexparser.middlewares",
        "pandas",
        "openpyxl",
        "requests",
        "PIL",
        "PIL.Image",
        "PIL.ImageTk",
    ],
    hookspath=[],
    runtime_hooks=[],
    excludes=[
        # Explicitly exclude Streamlit and its heavy dependencies
        "streamlit", "altair", "pydeck", "uvicorn", "starlette",
    ],
    win_no_prefer_redirects=False,
    win_private_assemblies=False,
    cipher=block_cipher,
    noarchive=False,
)

pyz = PYZ(a.pure, a.zipped_data, cipher=block_cipher)

exe = EXE(
    pyz,
    a.scripts,
    [],
    exclude_binaries=True,
    name="ZenodoImporter",
    debug=False,
    bootloader_ignore_signals=False,
    strip=False,
    upx=True,
    console=False,      # no console window — pure GUI
    icon=None,
)

coll = COLLECT(
    exe,
    a.binaries,
    a.zipfiles,
    a.datas,
    strip=False,
    upx=True,
    upx_exclude=[],
    name="ZenodoImporter",
)
