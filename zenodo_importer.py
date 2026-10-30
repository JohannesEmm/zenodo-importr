#!/usr/bin/env python3
"""
Zenodo BibTeX Importer
A Streamlit web app to upload academic publications to Zenodo from BibTeX files.

Run with:  streamlit run zenodo_importer.py
"""

import json
import re
from datetime import date
from pathlib import Path

import bibtexparser
from bibtexparser.model import Entry as BibEntry
import pandas as pd
import requests
import streamlit as st

# ─── Config persistence ──────────────────────────────────────────────────────

CONFIG_FILE = Path.home() / ".zenodo_importer_config.json"


def load_config() -> dict:
    if CONFIG_FILE.exists():
        try:
            return json.loads(CONFIG_FILE.read_text(encoding="utf-8"))
        except Exception:
            pass
    return {"token": "", "access_type": "open", "orcid_data": [], "sandbox": False}


def save_config(config: dict) -> None:
    CONFIG_FILE.write_text(json.dumps(config, indent=2), encoding="utf-8")


# ─── Zenodo API ───────────────────────────────────────────────────────────────

ZENODO_PROD    = "https://zenodo.org/api"
ZENODO_SANDBOX = "https://sandbox.zenodo.org/api"


def api_base(sandbox: bool) -> str:
    return ZENODO_SANDBOX if sandbox else ZENODO_PROD


def check_doi_on_zenodo(doi: str, sandbox: bool = False) -> dict:
    """Public search – no auth needed. Returns a status dict."""
    if not doi:
        return {"status": "no_doi"}

    clean = re.sub(r"^(doi:|DOI:)?\s*", "", doi.strip())
    try:
        r = requests.get(
            f"{api_base(sandbox)}/records",
            params={"q": f'doi:"{clean}"', "size": 5},
            timeout=15,
        )
        r.raise_for_status()
        data = r.json()
        total = data["hits"]["total"]
        if isinstance(total, dict):          # Elasticsearch 7 format
            total = total.get("value", 0)
        if total > 0:
            hit = data["hits"]["hits"][0]
            rid = hit.get("id") or hit.get("recid")
            return {
                "status":     "duplicate",
                "record_id":  rid,
                "record_url": f"https://{'sandbox.' if sandbox else ''}zenodo.org/record/{rid}",
                "title":      hit["metadata"].get("title", ""),
            }
        return {"status": "safe"}
    except Exception as exc:
        return {"status": "error", "message": str(exc)}


# ─── BibTeX → Zenodo metadata ─────────────────────────────────────────────────

ENTRY_TYPE_MAP = {
    "article":       ("publication", "article"),
    "inproceedings": ("publication", "conferencepaper"),
    "proceedings":   ("publication", "conferencepaper"),
    "conference":    ("publication", "conferencepaper"),
    "book":          ("publication", "book"),
    "incollection":  ("publication", "section"),
    "techreport":    ("publication", "technicalnote"),
    "report":        ("publication", "technicalnote"),
    "thesis":        ("publication", "thesis"),
    "phdthesis":     ("publication", "thesis"),
    "mastersthesis": ("publication", "thesis"),
    "misc":          ("publication", "other"),
    "unpublished":   ("publication", "preprint"),
    "software":      ("software",    None),
    "dataset":       ("dataset",     None),
}


def _strip_braces(s: str) -> str:
    return re.sub(r"[{}]", "", s).strip()


def _field(entry: BibEntry, key: str, default: str = "") -> str:
    """Get a field value from a bibtexparser v2 Entry, stripping LaTeX braces."""
    f = entry.get(key)
    return _strip_braces(str(f.value)) if f is not None else default


def _names_match(a: str, b: str) -> bool:
    norm = lambda s: re.sub(r"[^a-z]", "", s.lower())
    return norm(a) == norm(b)


def bibtex_to_zenodo(
    entry: BibEntry,
    access_type: str = "open",
    grants: list | None = None,
    orcid_map: dict | None = None,
) -> dict:
    """Build Zenodo metadata payload from a bibtexparser v2 Entry."""
    bib_type = entry.entry_type.lower()
    upload_type, pub_type = ENTRY_TYPE_MAP.get(bib_type, ("publication", "article"))

    title = _field(entry, "title", "Untitled")

    meta: dict = {
        "title":        title,
        "upload_type":  upload_type,
        "access_right": access_type,
        "language":     "eng",
    }

    if pub_type:
        meta["publication_type"] = pub_type

    if access_type == "open":
        meta["license"] = "CC-BY-4.0"

    # Description / abstract
    abstract = _field(entry, "abstract")
    meta["description"] = abstract if abstract else title

    # Publication date
    year = _field(entry, "year")
    if year.isdigit() and 1900 < int(year) <= 2100:
        meta["publication_date"] = f"{year}-01-01"
    else:
        meta["publication_date"] = f"{date.today().year}-01-01"

    # Creators / authors (with optional ORCID lookup)
    creators: list[dict] = []
    raw_authors = _field(entry, "author")
    if raw_authors:
        for auth in re.split(r"\s+and\s+", raw_authors, flags=re.IGNORECASE):
            auth = auth.strip()
            if not auth:
                continue
            creator: dict = {"name": auth}
            if orcid_map:
                for name_key, orcid in orcid_map.items():
                    if _names_match(auth, name_key):
                        creator["orcid"] = orcid
                        break
            creators.append(creator)
    meta["creators"] = creators or [{"name": "Unknown Author"}]

    # Journal / publication metadata
    bib_to_zen = {
        "journal": "journal_title",
        "volume":  "journal_volume",
        "number":  "journal_issue",
        "pages":   "journal_pages",
        "doi":     "doi",
        "issn":    "issn",
        "isbn":    "isbn",
    }
    for bib_key, zen_key in bib_to_zen.items():
        val = _field(entry, bib_key)
        if val:
            meta[zen_key] = val

    # Keywords
    kw = _field(entry, "keywords")
    if kw:
        meta["keywords"] = [k.strip() for k in re.split(r"[,;]", kw) if k.strip()]

    # EC Grants
    if grants:
        meta["grants"] = [{"id": g.strip()} for g in grants if g.strip()]

    return {"metadata": meta}


# ─── Zenodo upload ────────────────────────────────────────────────────────────


def upload_to_zenodo(
    token: str,
    metadata: dict,
    pdf_bytes: bytes | None = None,
    pdf_name: str | None = None,
    publish: bool = False,
    sandbox: bool = False,
) -> dict:
    """Create a deposition, optionally attach a file, optionally publish."""
    auth = {"Authorization": f"Bearer {token}"}
    base = api_base(sandbox)

    # 1. Create deposition
    r = requests.post(
        f"{base}/deposit/depositions",
        json=metadata,
        headers={**auth, "Content-Type": "application/json"},
        timeout=30,
    )
    if r.status_code != 201:
        raise RuntimeError(
            f"Deposition creation failed (HTTP {r.status_code}):\n{r.text}"
        )

    dep = r.json()
    dep_id    = dep["id"]
    bucket_url = dep["links"]["bucket"]

    # 2. Upload PDF
    if pdf_bytes and pdf_name:
        safe_name = re.sub(r"[^A-Za-z0-9._-]", "_", pdf_name)
        r2 = requests.put(
            f"{bucket_url}/{safe_name}",
            data=pdf_bytes,
            headers={**auth, "Content-Type": "application/octet-stream"},
            timeout=300,
        )
        if r2.status_code not in (200, 201):
            raise RuntimeError(
                f"File upload failed (HTTP {r2.status_code}):\n{r2.text}"
            )

    # 3. Publish
    published = False
    if publish:
        r3 = requests.post(
            f"{base}/deposit/depositions/{dep_id}/actions/publish",
            headers=auth,
            timeout=30,
        )
        published = r3.status_code == 202

    prefix = "sandbox." if sandbox else ""
    return {
        "deposition_id": dep_id,
        "deposit_url":   f"https://{prefix}zenodo.org/deposit/{dep_id}",
        "record_url":    f"https://{prefix}zenodo.org/record/{dep_id}",
        "published":     published,
    }


# ─── BibTeX parsing ───────────────────────────────────────────────────────────


def parse_bibtex(content: str) -> list[BibEntry]:
    """Parse BibTeX string with bibtexparser v2."""
    lib = bibtexparser.parse_string(content)
    return lib.entries


def entry_label(e: BibEntry) -> str:
    """Short human-readable label for selectbox."""
    raw_author = _field(e, "author", "Unknown")
    first_author = re.split(r"\s+and\s+", raw_author, flags=re.IGNORECASE)[0].strip()
    year  = _field(e, "year")
    title = _field(e, "title")[:70]
    return f"{first_author} ({year}) — {title}"


# ─── Streamlit UI ─────────────────────────────────────────────────────────────


def main() -> None:
    st.set_page_config(
        page_title="Zenodo BibTeX Importer",
        page_icon="📤",
        layout="wide",
    )
    st.title("📤 Zenodo BibTeX Importer")

    # Load persistent config into session state once
    if "config" not in st.session_state:
        st.session_state.config = load_config()
    config: dict = st.session_state.config

    tab_upload, tab_settings = st.tabs(["📂 Upload", "⚙️ Settings"])

    # ══════════════════════════════════════════════════════════════════════════
    # SETTINGS TAB
    # ══════════════════════════════════════════════════════════════════════════
    with tab_settings:
        st.header("Settings")
        st.caption(f"Settings are stored at `{CONFIG_FILE}`")

        new_token = st.text_input(
            "Zenodo Access Token",
            value=config.get("token", ""),
            type="password",
            help="Needs the `deposit:write` scope.",
        )
        st.markdown(
            "[Generate a new token ↗](https://zenodo.org/account/settings/applications/tokens/new/)"
        )

        new_access = st.selectbox(
            "Default Access Type",
            options=["open", "closed"],
            index=0 if config.get("access_type", "open") == "open" else 1,
        )

        new_sandbox = st.checkbox(
            "Use Sandbox (sandbox.zenodo.org) for testing",
            value=config.get("sandbox", False),
            help="Uploads go to the Zenodo sandbox instead of production. "
                 "Use a sandbox token from sandbox.zenodo.org.",
        )

        st.divider()
        st.subheader("Author ORCID Mapping")
        st.caption(
            "Upload an Excel file with **Name** and **ORCID** columns. "
            "The importer will match author names and attach ORCID IDs automatically."
        )

        orcid_file = st.file_uploader(
            "Excel file (Name + ORCID columns)", type=["xlsx", "xls"], key="orcid_uploader"
        )
        if orcid_file:
            try:
                orcid_df = pd.read_excel(orcid_file)
                if {"Name", "ORCID"}.issubset(orcid_df.columns):
                    config["orcid_data"] = (
                        orcid_df[["Name", "ORCID"]].fillna("").to_dict("records")
                    )
                    st.success(f"Loaded ORCID data for {len(config['orcid_data'])} authors.")
                    st.dataframe(orcid_df[["Name", "ORCID"]], use_container_width=True)
                else:
                    st.error("Excel file must contain 'Name' and 'ORCID' columns.")
            except Exception as exc:
                st.error(f"Error reading Excel: {exc}")
        elif config.get("orcid_data"):
            st.caption(f"Loaded from previous session: {len(config['orcid_data'])} authors.")
            st.dataframe(pd.DataFrame(config["orcid_data"]), use_container_width=True)

        if st.button("💾 Save Settings", type="primary"):
            config["token"]       = new_token
            config["access_type"] = new_access
            config["sandbox"]     = new_sandbox
            save_config(config)
            st.success("Settings saved.")

    # ══════════════════════════════════════════════════════════════════════════
    # UPLOAD TAB
    # ══════════════════════════════════════════════════════════════════════════
    with tab_upload:
        sandbox_mode: bool = config.get("sandbox", False)
        if sandbox_mode:
            st.warning("**Sandbox mode active** – uploads go to sandbox.zenodo.org")

        # ── Step 1: BibTeX ─────────────────────────────────────────────────
        st.header("Step 1: Upload BibTeX File")
        bib_file = st.file_uploader("Choose a .bib file", type=["bib"])

        selected_entry: dict | None = None
        doi_status: dict | None     = None

        if bib_file:
            raw = bib_file.read().decode("utf-8", errors="replace")
            entries = parse_bibtex(raw)

            if not entries:
                st.error("No valid BibTeX entries found in this file.")
            else:
                if len(entries) == 1:
                    selected_entry = entries[0]
                    st.success("Loaded 1 entry.")
                else:
                    st.info(f"Found **{len(entries)}** entries – select one to upload:")
                    labels  = [entry_label(e) for e in entries]
                    choice  = st.selectbox("Entry to upload", labels)
                    selected_entry = entries[labels.index(choice)]

                if selected_entry:
                    with st.expander("Entry details", expanded=True):
                        display = {
                            "BibTeX key": selected_entry.key,
                            "Type":       selected_entry.entry_type,
                            "Title":      _field(selected_entry, "title"),
                            "Authors":    _field(selected_entry, "author"),
                            "Year":       _field(selected_entry, "year"),
                            "Journal":    _field(selected_entry, "journal"),
                            "DOI":        _field(selected_entry, "doi"),
                            "Keywords":   _field(selected_entry, "keywords"),
                        }
                        df = pd.DataFrame.from_dict(display, orient="index", columns=["Value"])
                        st.dataframe(df, use_container_width=True)

                    # Auto DOI duplicate check
                    doi = _field(selected_entry, "doi")
                    if doi:
                        with st.spinner("Checking Zenodo for duplicates…"):
                            doi_status = check_doi_on_zenodo(doi, sandbox=sandbox_mode)

                        if doi_status["status"] == "duplicate":
                            st.error(
                                f"**Duplicate found on Zenodo!**\n\n"
                                f"Title: _{doi_status.get('title', '')}_\n\n"
                                f"[View existing record ↗]({doi_status['record_url']})"
                            )
                        elif doi_status["status"] == "safe":
                            st.success("No duplicate found – safe to upload.")
                        elif doi_status["status"] == "error":
                            st.warning(
                                f"DOI check returned an error: {doi_status.get('message', '')}. "
                                "You can still proceed."
                            )
                    else:
                        doi_status = {"status": "no_doi"}
                        st.warning("No DOI in this entry – duplicate check skipped.")

        st.divider()

        # ── Step 2: PDF ────────────────────────────────────────────────────
        st.header("Step 2: Upload PDF (optional)")
        pdf_file = st.file_uploader("Choose a PDF file", type=["pdf"])
        if pdf_file:
            size_mb = len(pdf_file.getvalue()) / 1_048_576
            st.success(f"PDF ready: **{pdf_file.name}** ({size_mb:.2f} MB)")

        st.divider()

        # ── Step 3: Upload options ─────────────────────────────────────────
        st.header("Step 3: Upload Options")

        col_left, col_right = st.columns(2)
        with col_left:
            token_input = st.text_input(
                "Zenodo Access Token",
                value=config.get("token", ""),
                type="password",
                help="Save your token permanently in the Settings tab.",
            )
            access_type = st.selectbox(
                "Access Type",
                ["open", "closed"],
                index=0 if config.get("access_type", "open") == "open" else 1,
            )
            auto_publish = st.checkbox("Auto-publish after upload", value=False)

        with col_right:
            grants_text = st.text_area(
                "EC Grant Numbers (one per line, optional)",
                placeholder="e.g.\n101022622\n956811",
                help="European Commission grant numbers – must exist in Zenodo's grants database.",
                height=120,
            )

        # ── Metadata preview ───────────────────────────────────────────────
        if selected_entry:
            with st.expander("Preview Zenodo metadata JSON"):
                grants = [g.strip() for g in grants_text.splitlines() if g.strip()]
                orcid_map = {
                    r["Name"]: r["ORCID"]
                    for r in config.get("orcid_data", [])
                    if r.get("ORCID")
                }
                preview_meta = bibtex_to_zenodo(
                    selected_entry,
                    access_type=access_type,
                    grants=grants or None,
                    orcid_map=orcid_map,
                )
                st.json(preview_meta)

        st.divider()

        # ── Step 4: Upload ─────────────────────────────────────────────────
        st.header("Step 4: Upload")

        upload_blocked = (
            doi_status is not None and doi_status.get("status") == "duplicate"
        )

        if upload_blocked:
            st.button("Upload Blocked – Duplicate Found", disabled=True)
        else:
            if st.button(
                "🚀 Upload to Zenodo",
                type="primary",
                disabled=(selected_entry is None),
            ):
                if not token_input:
                    st.error("Please enter a Zenodo access token.")
                else:
                    grants = [g.strip() for g in grants_text.splitlines() if g.strip()]
                    orcid_map = {
                        r["Name"]: r["ORCID"]
                        for r in config.get("orcid_data", [])
                        if r.get("ORCID")
                    }

                    zenodo_meta = bibtex_to_zenodo(
                        selected_entry,
                        access_type=access_type,
                        grants=grants or None,
                        orcid_map=orcid_map,
                    )

                    pdf_bytes = pdf_file.getvalue() if pdf_file else None
                    pdf_name  = pdf_file.name      if pdf_file else None

                    with st.spinner("Uploading to Zenodo…"):
                        try:
                            result = upload_to_zenodo(
                                token_input,
                                zenodo_meta,
                                pdf_bytes=pdf_bytes,
                                pdf_name=pdf_name,
                                publish=auto_publish,
                                sandbox=sandbox_mode,
                            )

                            status_label = "published" if result["published"] else "saved as draft"
                            st.success(
                                f"Upload successful! Deposition ID: **{result['deposition_id']}** ({status_label})"
                            )
                            st.markdown(f"[Open on Zenodo ↗]({result['deposit_url']})")
                            st.markdown(
                                "[View all my uploads ↗](https://zenodo.org/me/uploads)"
                            )

                            if not result["published"]:
                                st.info(
                                    "The record is saved as a **draft**. "
                                    "Log in to Zenodo to review and publish it."
                                )

                        except Exception as exc:
                            st.error(f"Upload failed:\n\n{exc}")


if __name__ == "__main__":
    main()
