#!/usr/bin/env python3
"""Turn an SVG into the `$header-logo:` line for dracula-header.scss.

    python3 svg2datauri.py my-logo.svg

Prints a data-URI declaration you can paste into the theme's scss:defaults (or
into your own .qmd theme file, where it will override the placeholder). A data
URI avoids the relative-path problem: Quarto compiles the theme SCSS into
<doc>_files/libs/revealjs/dist/theme/, so `url("logo.svg")` would be looked up
there rather than next to your .qmd.

PNG and JPEG work too (they are base64-encoded instead).
"""

import base64
import pathlib
import re
import sys


def svg_to_uri(text: str) -> str:
    # Drop the XML prolog, comments and <title>; they only bloat the URI.
    text = re.sub(r"<\?xml.*?\?>", "", text, flags=re.S)
    text = re.sub(r"<!--.*?-->", "", text, flags=re.S)
    text = re.sub(r"<title>.*?</title>", "", text, flags=re.S)
    text = re.sub(r">\s+<", "><", text).strip()
    # Single quotes inside so the whole URI can sit in a double-quoted url().
    text = text.replace('"', "'")
    for a, b in (("%", "%25"), ("#", "%23"), ("<", "%3C"), (">", "%3E")):
        text = text.replace(a, b)
    text = text.replace("\n", "").replace("\r", "")
    return "data:image/svg+xml," + text


def main() -> int:
    if len(sys.argv) != 2:
        print(__doc__.strip(), file=sys.stderr)
        return 2

    path = pathlib.Path(sys.argv[1])
    if not path.is_file():
        print(f"no such file: {path}", file=sys.stderr)
        return 1

    suffix = path.suffix.lower()
    notes = []

    if suffix == ".svg":
        uri = svg_to_uri(path.read_text(encoding="utf-8"))
    elif suffix in {".png", ".jpg", ".jpeg", ".webp", ".gif"}:
        mime = {"jpg": "jpeg", "svg": "svg+xml"}.get(suffix[1:], suffix[1:])
        uri = f"data:image/{mime};base64," + base64.b64encode(path.read_bytes()).decode()
        notes += png_notes(path)
    else:
        print(f"unsupported file type: {suffix}", file=sys.stderr)
        return 1

    print(f'$header-logo: url("{uri}") !default;')

    print(f"\n// {path.name}: data URI is {len(uri) / 1024:.1f} KB", file=sys.stderr)
    for n in notes:
        print(f"// {n}", file=sys.stderr)
    return 0


def png_notes(path: pathlib.Path) -> list:
    """Size / aspect advice for raster logos. Best effort — no hard dependency."""
    size = raster_size(path)
    if size is None:
        return ["could not read the pixel size; aim for 256 px on the long edge"]

    w, h = size
    out = [f"{w}x{h} px"]

    # The logo box is ~1.15em of a 40px root, and reveal.js scales the whole
    # deck up on large displays, so ~3x the CSS size keeps it crisp.
    if max(w, h) < 128:
        out.append("small for a HiDPI display — 256 px on the long edge is a safe target")

    ratio = w / h
    if ratio > 1.2:
        out.append(f"wide logo — set  $header-logo-width: {ratio * 1.15:.2f}em;")
    elif ratio < 0.83:
        out.append("tall logo — consider reducing $header-logo-size")

    return out


def raster_size(path: pathlib.Path):
    data = path.read_bytes()
    # PNG: IHDR is always the first chunk.
    if data[:8] == b"\x89PNG\r\n\x1a\n" and data[12:16] == b"IHDR":
        return (
            int.from_bytes(data[16:20], "big"),
            int.from_bytes(data[20:24], "big"),
        )
    try:
        from PIL import Image  # optional

        with Image.open(path) as im:
            return im.size
    except Exception:
        return None


if __name__ == "__main__":
    raise SystemExit(main())
