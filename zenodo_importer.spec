# -*- mode: python ; coding: utf-8 -*-
"""
PyInstaller spec for Zenodo BibTeX Importer.

Build command:
    pyinstaller zenodo_importer.spec
"""

import sys
import os
from pathlib import Path

# Locate the streamlit package so we can bundle its static assets
import streamlit
import importlib.metadata
STREAMLIT_DIR  = Path(streamlit.__file__).parent
STREAMLIT_META = Path(importlib.metadata.distribution("streamlit")._path)

block_cipher = None

a = Analysis(
    ["run.py"],
    pathex=[],
    binaries=[],
    datas=[
        # Bundle the Streamlit web assets (JS, CSS, fonts, …)
        (str(STREAMLIT_DIR / "static"),  "streamlit/static"),
        (str(STREAMLIT_DIR / "runtime"), "streamlit/runtime"),
        # Bundle streamlit package metadata (needed for importlib.metadata.version())
        (str(STREAMLIT_META), str(STREAMLIT_META.name)),
        # Bundle the app script itself so run.py can find it at _MEIPASS
        ("zenodo_importer.py", "."),
    ],
    hiddenimports=[
        # Streamlit internals
        "streamlit.web.bootstrap",
        "streamlit.web.server",
        "streamlit.runtime",
        "streamlit.runtime.scriptrunner",
        "streamlit.components.v1",
        # bibtexparser v2
        "bibtexparser",
        "bibtexparser.model",
        "bibtexparser.splitter",
        "bibtexparser.middlewares",
        "bibtexparser.writer",
        # Standard data stack
        "pandas",
        "openpyxl",
        "requests",
        # Streamlit-required extras
        "altair",
        "pydeck",
        "pyarrow",
        "click",
        "uvicorn",
        "starlette",
    ],
    hookspath=[],
    hooksconfig={},
    runtime_hooks=[],
    excludes=[],
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
    console=True,   # keep console for log output; set False for silent window
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
