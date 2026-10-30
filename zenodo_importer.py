#!/usr/bin/env python3
"""
Zenodo BibTeX Importer  –  Desktop GUI (CustomTkinter)
A native desktop app to upload academic publications to Zenodo from BibTeX files.

Run with:  python zenodo_importer.py
Build exe: pyinstaller zenodo_importer.spec
"""

import json
import re
import threading
import webbrowser
from datetime import date
from pathlib import Path
import tkinter as tk
import tkinter.ttk as ttk
from tkinter import filedialog, messagebox

import bibtexparser
from bibtexparser.model import Entry as BibEntry
import customtkinter as ctk
import pandas as pd
import requests

# ─── Appearance ───────────────────────────────────────────────────────────────
ctk.set_appearance_mode("System")   # follows OS dark/light mode
ctk.set_default_color_theme("blue")

# ─── Config persistence ───────────────────────────────────────────────────────
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


def _api(sandbox: bool) -> str:
    return ZENODO_SANDBOX if sandbox else ZENODO_PROD


def check_doi_on_zenodo(doi: str, sandbox: bool = False) -> dict:
    if not doi:
        return {"status": "no_doi"}
    clean = re.sub(r"^(doi:|DOI:)?\s*", "", doi.strip())
    try:
        r = requests.get(
            f"{_api(sandbox)}/records",
            params={"q": f'doi:"{clean}"', "size": 5},
            timeout=15,
        )
        r.raise_for_status()
        data = r.json()
        total = data["hits"]["total"]
        if isinstance(total, dict):
            total = total.get("value", 0)
        if total > 0:
            hit = data["hits"]["hits"][0]
            rid = hit.get("id") or hit.get("recid")
            prefix = "sandbox." if sandbox else ""
            return {
                "status":     "duplicate",
                "record_id":  rid,
                "record_url": f"https://{prefix}zenodo.org/record/{rid}",
                "title":      hit["metadata"].get("title", ""),
            }
        return {"status": "safe"}
    except Exception as exc:
        return {"status": "error", "message": str(exc)}


def upload_to_zenodo(
    token: str,
    metadata: dict,
    pdf_bytes: bytes | None = None,
    pdf_name: str | None = None,
    publish: bool = False,
    sandbox: bool = False,
) -> dict:
    auth = {"Authorization": f"Bearer {token}"}
    base = _api(sandbox)

    r = requests.post(
        f"{base}/deposit/depositions",
        json=metadata,
        headers={**auth, "Content-Type": "application/json"},
        timeout=30,
    )
    if r.status_code != 201:
        raise RuntimeError(f"Deposition failed (HTTP {r.status_code}):\n{r.text}")

    dep = r.json()
    dep_id     = dep["id"]
    bucket_url = dep["links"]["bucket"]

    if pdf_bytes and pdf_name:
        safe = re.sub(r"[^A-Za-z0-9._-]", "_", pdf_name)
        r2 = requests.put(
            f"{bucket_url}/{safe}",
            data=pdf_bytes,
            headers={**auth, "Content-Type": "application/octet-stream"},
            timeout=300,
        )
        if r2.status_code not in (200, 201):
            raise RuntimeError(f"File upload failed (HTTP {r2.status_code}):\n{r2.text}")

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
        "published":     published,
    }


# ─── BibTeX helpers ───────────────────────────────────────────────────────────
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


def _strip(s: str) -> str:
    return re.sub(r"[{}]", "", s).strip()


def _field(entry: BibEntry, key: str, default: str = "") -> str:
    f = entry.get(key)
    return _strip(str(f.value)) if f is not None else default


def _names_match(a: str, b: str) -> bool:
    norm = lambda s: re.sub(r"[^a-z]", "", s.lower())
    return norm(a) == norm(b)


def bibtex_to_zenodo(
    entry: BibEntry,
    access_type: str = "open",
    grants: list | None = None,
    orcid_map: dict | None = None,
) -> dict:
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

    abstract = _field(entry, "abstract")
    meta["description"] = abstract if abstract else title

    year = _field(entry, "year")
    if year.isdigit() and 1900 < int(year) <= 2100:
        meta["publication_date"] = f"{year}-01-01"
    else:
        meta["publication_date"] = f"{date.today().year}-01-01"

    creators: list[dict] = []
    for auth in re.split(r"\s+and\s+", _field(entry, "author"), flags=re.IGNORECASE):
        auth = auth.strip()
        if not auth:
            continue
        creator: dict = {"name": auth}
        if orcid_map:
            for k, v in orcid_map.items():
                if _names_match(auth, k):
                    creator["orcid"] = v
                    break
        creators.append(creator)
    meta["creators"] = creators or [{"name": "Unknown Author"}]

    for bib_key, zen_key in [
        ("journal", "journal_title"), ("volume", "journal_volume"),
        ("number",  "journal_issue"), ("pages",  "journal_pages"),
        ("doi",     "doi"),           ("issn",   "issn"), ("isbn", "isbn"),
    ]:
        val = _field(entry, bib_key)
        if val:
            meta[zen_key] = val

    kw = _field(entry, "keywords")
    if kw:
        meta["keywords"] = [k.strip() for k in re.split(r"[,;]", kw) if k.strip()]

    if grants:
        meta["grants"] = [{"id": g} for g in grants if g]

    return {"metadata": meta}


def _entry_label(e: BibEntry) -> str:
    first = re.split(r"\s+and\s+", _field(e, "author", "Unknown"), flags=re.IGNORECASE)[0].strip()
    return f"{first} ({_field(e, 'year')}) — {_field(e, 'title')[:65]}"


# ─── GUI ──────────────────────────────────────────────────────────────────────

class ZenodoApp(ctk.CTk):

    def __init__(self):
        super().__init__()
        self.title("Zenodo BibTeX Importer")
        self.geometry("800x860")
        self.minsize(720, 720)

        self.config_data  = load_config()
        self.bib_entries:  list[BibEntry] = []
        self.selected_entry: BibEntry | None = None
        self.doi_status:   dict | None = None
        self.pdf_path:     str | None = None

        self._build_ui()
        self._sync_from_config()

    # ── UI construction ───────────────────────────────────────────────────────

    def _build_ui(self):
        self.tabs = ctk.CTkTabview(self)
        self.tabs.pack(fill="both", expand=True, padx=12, pady=12)
        self._build_upload_tab(self.tabs.add("  Upload  "))
        self._build_settings_tab(self.tabs.add("  Settings  "))

    def _build_upload_tab(self, parent):
        scroll = ctk.CTkScrollableFrame(parent)
        scroll.pack(fill="both", expand=True)

        # ── BibTeX section ────────────────────────────────────────────────────
        s1 = self._section(scroll, "Step 1 — BibTeX File")

        row = self._hframe(s1)
        self.bib_label = ctk.CTkLabel(row, text="No file selected", anchor="w",
                                      text_color="gray")
        self.bib_label.pack(side="left", fill="x", expand=True)
        ctk.CTkButton(row, text="Browse .bib", width=120,
                      command=self._browse_bib).pack(side="right")

        # Entry selector (shown only for multi-entry files)
        self.entry_selector = self._hframe(s1)
        ctk.CTkLabel(self.entry_selector, text="Entry:", width=50).pack(side="left")
        self.entry_var = tk.StringVar()
        self.entry_menu = ctk.CTkOptionMenu(
            self.entry_selector, variable=self.entry_var,
            values=["—"], command=self._on_entry_changed, width=580)
        self.entry_menu.pack(side="left", fill="x", expand=True, padx=(4, 0))
        self.entry_selector.pack_forget()

        # Entry details grid
        self.details_box = ctk.CTkFrame(s1)
        self.detail_vars: dict[str, tk.StringVar] = {}
        rows = [("Title", "title"), ("Authors", "authors"), ("Year / Journal", "yearjournal"),
                ("DOI", "doi"), ("Type", "type")]
        for i, (lbl, key) in enumerate(rows):
            ctk.CTkLabel(self.details_box, text=f"{lbl}:", anchor="e",
                         width=110, text_color="gray").grid(
                row=i, column=0, sticky="e", padx=(8, 4), pady=3)
            var = tk.StringVar(value="—")
            self.detail_vars[key] = var
            ctk.CTkLabel(self.details_box, textvariable=var, anchor="w",
                         wraplength=560, justify="left").grid(
                row=i, column=1, sticky="w", padx=(0, 8), pady=3)
        self.details_box.pack_forget()

        # DOI status
        self.doi_var = tk.StringVar(value="")
        self.doi_lbl = ctk.CTkLabel(s1, textvariable=self.doi_var, anchor="w")
        self.doi_lbl.pack(fill="x", padx=10, pady=(2, 6))

        # ── PDF section ───────────────────────────────────────────────────────
        s2 = self._section(scroll, "Step 2 — PDF File (optional)")
        row2 = self._hframe(s2)
        self.pdf_label = ctk.CTkLabel(row2, text="No file selected", anchor="w",
                                      text_color="gray")
        self.pdf_label.pack(side="left", fill="x", expand=True)
        ctk.CTkButton(row2, text="Browse .pdf", width=120,
                      command=self._browse_pdf).pack(side="right")

        # ── Options section ───────────────────────────────────────────────────
        s3 = self._section(scroll, "Step 3 — Upload Options")

        r_tok = self._hframe(s3)
        ctk.CTkLabel(r_tok, text="Token:", width=80, anchor="e").pack(side="left")
        self.token_entry = ctk.CTkEntry(r_tok, show="•",
                                        placeholder_text="Zenodo access token")
        self.token_entry.pack(side="left", fill="x", expand=True, padx=(5, 0))

        r_opts = self._hframe(s3)
        ctk.CTkLabel(r_opts, text="Access:", width=80, anchor="e").pack(side="left")
        self.access_var = tk.StringVar(value="open")
        ctk.CTkOptionMenu(r_opts, variable=self.access_var,
                          values=["open", "closed"], width=100).pack(side="left", padx=(5, 20))
        self.publish_var = tk.BooleanVar(value=False)
        ctk.CTkCheckBox(r_opts, text="Auto-publish", variable=self.publish_var).pack(side="left")
        self.sandbox_var = tk.BooleanVar(value=False)
        ctk.CTkCheckBox(r_opts, text="Sandbox mode",
                        variable=self.sandbox_var).pack(side="left", padx=(20, 0))

        ctk.CTkLabel(s3, text="EC Grant Numbers (one per line, optional):",
                     anchor="w").pack(fill="x", padx=10, pady=(8, 2))
        self.grants_box = ctk.CTkTextbox(s3, height=68)
        self.grants_box.pack(fill="x", padx=10, pady=(0, 10))

        # ── Upload button ─────────────────────────────────────────────────────
        self.upload_btn = ctk.CTkButton(
            scroll, text="Upload to Zenodo",
            height=42, font=ctk.CTkFont(size=14, weight="bold"),
            command=self._do_upload)
        self.upload_btn.pack(fill="x", padx=12, pady=10)

        # ── Result area ───────────────────────────────────────────────────────
        s4 = self._section(scroll, "Result")
        self.result_box = ctk.CTkTextbox(s4, height=90, state="disabled",
                                         font=ctk.CTkFont(family="Courier", size=12))
        self.result_box.pack(fill="x", padx=10, pady=(0, 8))

    def _build_settings_tab(self, parent):
        # Token
        s1 = self._section(parent, "Zenodo Token")
        self.settings_token = ctk.CTkEntry(s1, show="•",
                                           placeholder_text="Zenodo access token")
        self.settings_token.pack(fill="x", padx=10, pady=(0, 6))
        ctk.CTkButton(s1, text="Generate new token  ↗", width=200, anchor="w",
                      command=lambda: webbrowser.open(
                          "https://zenodo.org/account/settings/applications/tokens/new/"
                      )).pack(anchor="w", padx=10, pady=(0, 8))

        # Defaults
        s2 = self._section(parent, "Defaults")
        r = self._hframe(s2)
        ctk.CTkLabel(r, text="Default Access:", width=120, anchor="e").pack(side="left")
        self.settings_access = tk.StringVar(value="open")
        ctk.CTkOptionMenu(r, variable=self.settings_access,
                          values=["open", "closed"], width=100).pack(side="left", padx=5)
        self.settings_sandbox = tk.BooleanVar(value=False)
        ctk.CTkCheckBox(s2, text="Use Sandbox (sandbox.zenodo.org)",
                        variable=self.settings_sandbox).pack(anchor="w", padx=10, pady=(4, 8))

        # ORCID
        s3 = self._section(parent, "Author ORCID Mapping")
        ctk.CTkLabel(s3, text="Excel file with 'Name' and 'ORCID' columns.",
                     anchor="w", text_color="gray").pack(fill="x", padx=10, pady=(0, 4))
        r2 = self._hframe(s3)
        self.orcid_label = ctk.CTkLabel(r2, text="No file loaded",
                                        anchor="w", text_color="gray")
        self.orcid_label.pack(side="left", fill="x", expand=True)
        ctk.CTkButton(r2, text="Browse Excel", width=120,
                      command=self._browse_orcid).pack(side="right")

        # ORCID table using ttk.Treeview (no CTk table widget exists)
        tree_wrap = tk.Frame(s3, bg=self._bg())
        tree_wrap.pack(fill="x", padx=10, pady=(6, 10))
        style = ttk.Style()
        style.configure("Zenodo.Treeview", rowheight=22, font=("Segoe UI", 10))
        style.configure("Zenodo.Treeview.Heading", font=("Segoe UI", 10, "bold"))
        self.orcid_tree = ttk.Treeview(tree_wrap, columns=("Name", "ORCID"),
                                       show="headings", height=8, style="Zenodo.Treeview")
        self.orcid_tree.heading("Name",  text="Name")
        self.orcid_tree.heading("ORCID", text="ORCID")
        self.orcid_tree.column("Name",  width=280, minwidth=150)
        self.orcid_tree.column("ORCID", width=200, minwidth=120)
        vsb = ttk.Scrollbar(tree_wrap, orient="vertical",
                            command=self.orcid_tree.yview)
        self.orcid_tree.configure(yscrollcommand=vsb.set)
        self.orcid_tree.pack(side="left", fill="x", expand=True)
        vsb.pack(side="right", fill="y")

        # Save
        ctk.CTkButton(parent, text="Save Settings", height=38,
                      command=self._save_settings).pack(fill="x", padx=12, pady=10)

    # ── Layout helpers ────────────────────────────────────────────────────────

    def _section(self, parent, title: str) -> ctk.CTkFrame:
        frame = ctk.CTkFrame(parent)
        frame.pack(fill="x", padx=10, pady=(8, 0))
        ctk.CTkLabel(frame, text=title,
                     font=ctk.CTkFont(size=13, weight="bold")).pack(
            anchor="w", padx=10, pady=(8, 4))
        return frame

    def _hframe(self, parent) -> ctk.CTkFrame:
        f = ctk.CTkFrame(parent, fg_color="transparent")
        f.pack(fill="x", padx=10, pady=(0, 6))
        return f

    def _bg(self) -> str:
        return ctk.ThemeManager.theme["CTkFrame"]["fg_color"][
            0 if ctk.get_appearance_mode() == "Light" else 1]

    # ── Config sync ───────────────────────────────────────────────────────────

    def _sync_from_config(self):
        cfg = self.config_data
        tok = cfg.get("token", "")
        self.token_entry.insert(0, tok)
        self.settings_token.insert(0, tok)
        self.access_var.set(cfg.get("access_type", "open"))
        self.settings_access.set(cfg.get("access_type", "open"))
        self.sandbox_var.set(cfg.get("sandbox", False))
        self.settings_sandbox.set(cfg.get("sandbox", False))
        if cfg.get("orcid_data"):
            self._fill_orcid_tree(cfg["orcid_data"])
            self.orcid_label.configure(
                text=f"Loaded {len(cfg['orcid_data'])} authors from previous session",
                text_color="gray")

    # ── BibTeX ────────────────────────────────────────────────────────────────

    def _browse_bib(self):
        path = filedialog.askopenfilename(
            title="Select BibTeX file",
            filetypes=[("BibTeX", "*.bib"), ("All files", "*.*")])
        if not path:
            return
        self.bib_label.configure(text=Path(path).name, text_color="white")
        try:
            content = Path(path).read_text(encoding="utf-8", errors="replace")
            lib = bibtexparser.parse_string(content)
            self.bib_entries = lib.entries
            if not self.bib_entries:
                messagebox.showerror("Error", "No valid BibTeX entries found.")
                return
            if len(self.bib_entries) == 1:
                self.entry_selector.pack_forget()
                self._select_entry(self.bib_entries[0])
            else:
                labels = [_entry_label(e) for e in self.bib_entries]
                self.entry_menu.configure(values=labels)
                self.entry_var.set(labels[0])
                self.entry_selector.pack(fill="x", padx=10, pady=(0, 6))
                self._select_entry(self.bib_entries[0])
        except Exception as exc:
            messagebox.showerror("Error", f"Failed to load BibTeX:\n{exc}")

    def _on_entry_changed(self, choice: str):
        labels = [_entry_label(e) for e in self.bib_entries]
        if choice in labels:
            self._select_entry(self.bib_entries[labels.index(choice)])

    def _select_entry(self, entry: BibEntry):
        self.selected_entry = entry
        self.detail_vars["title"].set(_field(entry, "title", "—"))
        self.detail_vars["authors"].set(_field(entry, "author", "—"))
        year = _field(entry, "year")
        journal = _field(entry, "journal")
        self.detail_vars["yearjournal"].set(
            f"{year}{'  |  ' + journal if journal else ''}")
        self.detail_vars["doi"].set(_field(entry, "doi") or "—")
        self.detail_vars["type"].set(entry.entry_type)
        self.details_box.pack(fill="x", padx=10, pady=(0, 4))
        # DOI check
        self._start_doi_check(entry)

    def _start_doi_check(self, entry: BibEntry):
        doi = _field(entry, "doi")
        self.doi_var.set("🔍  Checking for duplicates…")
        self.doi_status = None
        sandbox = self.sandbox_var.get()

        def run():
            result = check_doi_on_zenodo(doi, sandbox=sandbox)
            self.doi_status = result
            if result["status"] == "duplicate":
                url = result["record_url"]
                msg = f"❌  Duplicate on Zenodo — {result.get('title','')[:55]}\n    {url}"
                self.after(0, lambda: self.doi_var.set(msg))
                self.after(0, lambda: self.upload_btn.configure(
                    state="disabled",
                    text="Upload Blocked — Duplicate Found",
                    fg_color="darkred"))
            elif result["status"] == "safe":
                self.after(0, lambda: self.doi_var.set("✅  No duplicate — safe to upload"))
                self.after(0, self._reset_upload_btn)
            elif result["status"] == "no_doi":
                self.after(0, lambda: self.doi_var.set("⚠️  No DOI — duplicate check skipped"))
                self.after(0, self._reset_upload_btn)
            else:
                self.after(0, lambda: self.doi_var.set(
                    f"⚠️  DOI check error: {result.get('message','')}"))
                self.after(0, self._reset_upload_btn)

        threading.Thread(target=run, daemon=True).start()

    def _reset_upload_btn(self):
        self.upload_btn.configure(state="normal",
                                  text="Upload to Zenodo",
                                  fg_color=ctk.ThemeManager.theme["CTkButton"]["fg_color"])

    # ── PDF ───────────────────────────────────────────────────────────────────

    def _browse_pdf(self):
        path = filedialog.askopenfilename(
            title="Select PDF file",
            filetypes=[("PDF", "*.pdf"), ("All files", "*.*")])
        if not path:
            return
        self.pdf_path = path
        size = Path(path).stat().st_size / 1_048_576
        self.pdf_label.configure(
            text=f"{Path(path).name}  ({size:.1f} MB)", text_color="white")

    # ── Upload ────────────────────────────────────────────────────────────────

    def _do_upload(self):
        if not self.selected_entry:
            messagebox.showerror("Error", "Load a BibTeX file first.")
            return
        token = self.token_entry.get().strip()
        if not token:
            messagebox.showerror("Error", "Enter a Zenodo access token.")
            return
        if self.doi_status and self.doi_status.get("status") == "duplicate":
            messagebox.showerror("Blocked", "Upload blocked: duplicate DOI on Zenodo.")
            return

        grants_raw = self.grants_box.get("1.0", "end").strip()
        grants = [g.strip() for g in grants_raw.splitlines() if g.strip()] or None
        orcid_map = {r["Name"]: r["ORCID"]
                     for r in self.config_data.get("orcid_data", [])
                     if r.get("ORCID")}

        metadata = bibtex_to_zenodo(
            self.selected_entry,
            access_type=self.access_var.get(),
            grants=grants,
            orcid_map=orcid_map,
        )

        pdf_bytes = Path(self.pdf_path).read_bytes() if self.pdf_path else None
        pdf_name  = Path(self.pdf_path).name         if self.pdf_path else None

        self.upload_btn.configure(state="disabled", text="Uploading…")
        self._set_result("Uploading to Zenodo…")

        sandbox = self.sandbox_var.get()
        publish = self.publish_var.get()

        def run():
            try:
                result = upload_to_zenodo(
                    token, metadata,
                    pdf_bytes=pdf_bytes, pdf_name=pdf_name,
                    publish=publish, sandbox=sandbox)
                status = "published" if result["published"] else "saved as draft"
                msg = (f"✅  Upload successful!  ({status})\n"
                       f"    Deposition ID: {result['deposition_id']}\n"
                       f"    {result['deposit_url']}")
                self.after(0, lambda: self._set_result(msg))
                self.after(0, self._reset_upload_btn)
            except Exception as exc:
                self.after(0, lambda: self._set_result(f"❌  Upload failed:\n    {exc}"))
                self.after(0, self._reset_upload_btn)

        threading.Thread(target=run, daemon=True).start()

    def _set_result(self, text: str):
        self.result_box.configure(state="normal")
        self.result_box.delete("1.0", "end")
        self.result_box.insert("1.0", text)
        self.result_box.configure(state="disabled")

    # ── Settings ──────────────────────────────────────────────────────────────

    def _browse_orcid(self):
        path = filedialog.askopenfilename(
            title="Select ORCID Excel file",
            filetypes=[("Excel", "*.xlsx *.xls"), ("All files", "*.*")])
        if not path:
            return
        try:
            df = pd.read_excel(path)
            if not {"Name", "ORCID"}.issubset(df.columns):
                messagebox.showerror("Error", "Excel must have 'Name' and 'ORCID' columns.")
                return
            records = df[["Name", "ORCID"]].fillna("").to_dict("records")
            self.config_data["orcid_data"] = records
            self._fill_orcid_tree(records)
            self.orcid_label.configure(
                text=f"{Path(path).name}  ({len(records)} authors)",
                text_color="white")
        except Exception as exc:
            messagebox.showerror("Error", f"Failed to load Excel:\n{exc}")

    def _fill_orcid_tree(self, records: list[dict]):
        for row in self.orcid_tree.get_children():
            self.orcid_tree.delete(row)
        for r in records:
            self.orcid_tree.insert("", "end",
                                   values=(r.get("Name", ""), r.get("ORCID", "")))

    def _save_settings(self):
        self.config_data["token"]       = self.settings_token.get().strip()
        self.config_data["access_type"] = self.settings_access.get()
        self.config_data["sandbox"]     = self.settings_sandbox.get()
        save_config(self.config_data)
        # Sync to upload tab
        self.token_entry.delete(0, "end")
        self.token_entry.insert(0, self.config_data["token"])
        self.access_var.set(self.config_data["access_type"])
        self.sandbox_var.set(self.config_data["sandbox"])
        messagebox.showinfo("Saved", "Settings saved.")


# ─── Entry point ──────────────────────────────────────────────────────────────

def main():
    app = ZenodoApp()
    app.mainloop()


if __name__ == "__main__":
    main()
