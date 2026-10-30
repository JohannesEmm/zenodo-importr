"""
Entry point used by PyInstaller to launch the Streamlit app.
When running as a frozen exe, Streamlit's web server is started
programmatically instead of via the CLI.
"""
import sys
import os


def resource(relative: str) -> str:
    """Return absolute path, works both from source and inside a frozen bundle."""
    base = getattr(sys, "_MEIPASS", os.path.dirname(os.path.abspath(__file__)))
    return os.path.join(base, relative)


if __name__ == "__main__":
    # Must set argv before importing streamlit so its argument parser is happy
    app_file = resource("zenodo_importer.py")
    sys.argv = ["streamlit", "run", app_file,
                "--server.headless=false",
                "--server.enableCORS=false",
                "--server.enableXsrfProtection=false"]

    from streamlit.web import bootstrap
    bootstrap.run(app_file, False, [], {})
