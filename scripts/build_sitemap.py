#!/usr/bin/env python3
"""Regenerate sitemap.xml from data/schools_index.json.

Runs in the Refresh IPEDS Full Dataset workflow (3x/year) because the
school roster only meaningfully changes when IPEDS releases new data.
"""

import json
from pathlib import Path

BASE = "https://financialtracker.hechingerreport.org"
PAGES = ["", "school.html", "accreditation.html", "cuts.html", "research.html", "methodology.html"]


def main():
    index = json.loads(Path("data/schools_index.json").read_text(encoding="utf-8"))
    schools = index if isinstance(index, list) else index.get("schools", [])

    urls = [f"{BASE}/{page}" for page in PAGES]
    urls += [
        f"{BASE}/school.html?unitid={school['unitid']}"
        for school in schools
        if school.get("unitid")
    ]

    lines = [
        '<?xml version="1.0" encoding="UTF-8"?>',
        '<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">',
    ]
    lines += [f"  <url><loc>{url.replace('&', '&amp;')}</loc></url>" for url in urls]
    lines.append("</urlset>")

    Path("sitemap.xml").write_bytes(("\n".join(lines) + "\n").encode("utf-8"))
    print(f"sitemap.xml written: {len(urls)} URLs")


if __name__ == "__main__":
    main()
