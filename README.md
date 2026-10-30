# Zenodo BibTeX Importer

A Python/Streamlit web app that streamlines uploading academic publications to Zenodo. Load a BibTeX file and an optional PDF, and the tool handles metadata formatting and the Zenodo API upload — including automatic duplicate detection.

> The original R/Shiny implementation is preserved as `zenodo-importr.R`.

## Features

- **BibTeX parsing** – supports all common entry types (article, inproceedings, book, thesis, techreport, …)
- **Multi-entry selection** – when a `.bib` file has multiple entries, pick the one to upload
- **Automatic duplicate detection** – checks Zenodo by DOI before upload and blocks duplicates
- **PDF attachment** – attach the full-text PDF to the deposition
- **ORCID auto-matching** – load an Excel file with author names and ORCIDs; they are attached automatically
- **EC Grant support** – add European Commission grant numbers
- **Metadata preview** – inspect the exact JSON sent to Zenodo before committing
- **Sandbox mode** – test uploads against `sandbox.zenodo.org` without touching production
- **Persistent settings** – token, default access type, and ORCID table are stored locally (`~/.zenodo_importer_config.json`)

## Quick Start (Windows)

**Option A — Double-click the pre-built exe** (no Python needed):
```
dist\ZenodoImporter.exe
```

**Option B — Run from source** (requires Python 3.10+):
1. Install Python from <https://python.org> (tick "Add Python to PATH")
2. Double-click **`launch.bat`**

**Rebuild the exe after code changes:**
```bash
pip install pyinstaller
pyinstaller zenodo_importer.spec
```
The single-file exe appears at `dist\ZenodoImporter.exe`.

## Usage

### Upload tab

| Step | Action |
|------|--------|
| 1 | Upload a `.bib` file. If it contains multiple entries, select the one to upload. The DOI is checked against Zenodo automatically. |
| 2 | Upload a PDF (optional). |
| 3 | Set the access token, access type, and any EC grant numbers. |
| 4 | Click **Upload to Zenodo**. |

### Settings tab

- **Zenodo Access Token** – saved to disk; not required on every launch.
  [Generate a token ↗](https://zenodo.org/account/settings/applications/tokens/new/) (needs `deposit:write` scope)
- **Default Access Type** – open or closed access
- **Sandbox mode** – use `sandbox.zenodo.org` for testing (requires a separate sandbox token)
- **Author ORCID Mapping** – upload an Excel file with **Name** and **ORCID** columns

## Supported BibTeX Fields

| BibTeX field | Zenodo field |
|---|---|
| title | title |
| author | creators |
| abstract | description |
| year | publication_date |
| journal | journal_title |
| volume | journal_volume |
| number | journal_issue |
| pages | journal_pages |
| doi | doi / related_identifiers |
| keywords | keywords |
| issn / isbn | issn / isbn |

## Requirements

```
Python 3.10+
customtkinter
bibtexparser >= 2.0
requests
pandas
openpyxl
```

## Troubleshooting

| Error | Likely cause |
|---|---|
| HTTP 403 | Token missing `deposit:write` scope or wrong environment (sandbox vs. production) |
| HTTP 400 | Invalid metadata (e.g. a grant ID not in Zenodo's database) |
| PDF upload fails | File too large or network timeout; try again |
| BibTeX parse errors | Non-standard encoding or malformed file; open in JabRef and re-export |

## Technical Notes

- Uses the [Zenodo REST API v1](https://developers.zenodo.org/)
- Deposition flow: create → upload file → (optionally) publish
- All uploads start as **drafts** unless "Auto-publish" is checked
- Config is stored at `~/.zenodo_importer_config.json` (your home directory)

---

**Warning:** uploads without sandbox mode go to production Zenodo. Published records cannot be deleted — only new versions can be added.
