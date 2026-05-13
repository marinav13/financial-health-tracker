#!/usr/bin/env python3
"""
check_file_integrity.py - Detect Cowork-on-Windows file corruption.

Walks the working tree and flags any source/data file with:
  - trailing null bytes (classic Cowork Edit-tool truncation fingerprint)
  - missing trailing newline (truncation fingerprint)
  - JSON parse failure (catches mid-string truncation)

Designed to be run manually before and after any Claude editing session:
  python3 scripts/check_file_integrity.py
  python3 scripts/check_file_integrity.py --staged   # only staged files
  python3 scripts/check_file_integrity.py --restore  # offer git checkout for committed files

Exit code 0 = clean, 1 = problems found.

Why this exists: Claude's Edit tool, when used in Cowork mode against a
Windows-mounted filesystem, silently truncates files. The "read" tool can
also serve cached content, so Claude-side verify-after-write is unreliable.
This script reads raw bytes via Python and is not fooled by either layer.
"""
from __future__ import annotations
import argparse
import json
import os
import subprocess
import sys
from pathlib import Path

EXT = {".R", ".r", ".js", ".mjs", ".cjs", ".json", ".yml", ".yaml",
       ".html", ".css", ".md", ".py", ".toml", ".sh"}
SKIP_DIRS = {".git", "node_modules", "renv", "playwright-cache",
             "dist", "build", "cache", ".next"}


def staged_files() -> list[str]:
    r = subprocess.run(
        ["git", "diff", "--cached", "--name-only", "--diff-filter=ACM"],
        capture_output=True, text=True, check=False,
    )
    return [line for line in r.stdout.splitlines() if line.strip()]


def walk_tree(root: str) -> list[str]:
    out = []
    for dirpath, dirs, files in os.walk(root):
        dirs[:] = [d for d in dirs if d not in SKIP_DIRS]
        for fn in files:
            ext = os.path.splitext(fn)[1].lower()
            if ext in EXT:
                out.append(os.path.join(dirpath, fn))
    return out


def check_file(path: str) -> list[tuple[str, str]]:
    issues = []
    try:
        with open(path, "rb") as f:
            data = f.read()
    except Exception as e:
        return [("read_error", str(e))]
    if b"\x00" in data:
        n = data.count(b"\x00")
        trail = len(data) - len(data.rstrip(b"\x00"))
        issues.append(("null_bytes", f"count={n} trailing={trail} size={len(data)}"))
    elif data and not data.endswith(b"\n"):
        issues.append(("no_trailing_newline", f"size={len(data)}"))
    if path.lower().endswith(".json") and data:
        try:
            json.loads(data.decode("utf-8", "replace"))
        except Exception as e:
            issues.append(("json_parse", str(e)[:200]))
    return issues


def file_in_head(path: str) -> bool:
    r = subprocess.run(["git", "cat-file", "-e", f"HEAD:{path}"],
                       capture_output=True, check=False)
    return r.returncode == 0


def head_clean(path: str) -> bool:
    r = subprocess.run(["git", "show", f"HEAD:{path}"], capture_output=True, check=False)
    if r.returncode != 0:
        return False
    data = r.stdout
    if b"\x00" in data:
        return False
    if data and not data.endswith(b"\n"):
        return False
    return True


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[1] if __doc__ else "")
    ap.add_argument("--staged", action="store_true",
                    help="Only check files in the git staging area")
    ap.add_argument("--restore", action="store_true",
                    help="Offer to git-checkout files corrupted in working tree but clean in HEAD")
    ap.add_argument("--quiet", action="store_true", help="Suppress per-file output")
    args = ap.parse_args()

    files = staged_files() if args.staged else walk_tree(".")
    problems = []
    for path in files:
        if not os.path.isfile(path):
            continue
        for kind, detail in check_file(path):
            problems.append((path, kind, detail))

    if not problems:
        print(f"check_file_integrity: OK ({len(files)} files checked)")
        return 0

    by_kind = {}
    for p, k, d in problems:
        by_kind.setdefault(k, []).append((p, d))

    print(f"check_file_integrity: FOUND {len(problems)} issues across {len({p for p,_,_ in problems})} files")
    for kind, rows in sorted(by_kind.items()):
        print(f"\n== {kind.upper()} ({len(rows)} files) ==")
        if not args.quiet:
            for p, d in rows[:20]:
                print(f"  {p}  {d}")
            if len(rows) > 20:
                print(f"  ... and {len(rows) - 20} more")

    if args.restore:
        print("\n-- RESTORE PASS --")
        recoverable = []
        for p, _, _ in problems:
            if file_in_head(p) and head_clean(p):
                recoverable.append(p)
        print(f"Files corrupted in working tree but clean in HEAD: {len(recoverable)}")
        if recoverable:
            print("Run to restore (review first):")
            for p in recoverable[:30]:
                print(f"  git checkout -- '{p}'")
            if len(recoverable) > 30:
                print(f"  ... and {len(recoverable) - 30} more")
    return 1


if __name__ == "__main__":
    sys.exit(main())
