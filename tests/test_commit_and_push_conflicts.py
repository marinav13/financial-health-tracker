"""Simulation tests for scripts/ci/commit_and_push.sh conflict handling.

Reproduces the 2026-07-15 production failure: the weekly refresh's data
commit rebasing over backfill commits that touched the append-only
discovery leads log. Asserts that (1) the rebase completes without an
editor (CI has none), (2) append-only files union-merge instead of
dropping the other writer's rows, and (3) snapshot-style conflict paths
still resolve to the runner's version.

Run directly: python tests/test_commit_and_push_conflicts.py
"""

import csv
import os
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[1]
SCRIPT = REPO_ROOT / "scripts" / "ci" / "commit_and_push.sh"

LEADS_REL = "data_pipelines/college_cuts/discovery/leads.csv"
SNAP_REL = "data_pipelines/college_cuts/discovered_cut_candidates.csv"
LEADS_HEADER = "lead_id,first_seen,tier,url\n"
SNAP_HEADER = "cut_id,unitid,institution_name\n"

passed = 0
failed = 0


def run(name, fn):
    global passed, failed
    try:
        fn()
        print(f"  PASS: {name}")
        passed += 1
    except Exception as exc:  # noqa: BLE001
        print(f"  FAIL: {name}: {exc}")
        failed += 1


def git(cwd, *args, env=None):
    merged = {**os.environ, **(env or {})}
    result = subprocess.run(
        ["git", *args], cwd=cwd, capture_output=True, text=True, env=merged
    )
    if result.returncode != 0:
        raise AssertionError(f"git {' '.join(args)} failed: {result.stderr.strip()}")
    return result.stdout


def write(base, rel, text):
    path = Path(base) / rel
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text, encoding="utf-8", newline="\n")


def build_scenario(tmp):
    """Bare origin + runner clone at base; a second writer advances origin."""
    origin = Path(tmp) / "origin.git"
    subprocess.run(["git", "init", "--bare", "-q", str(origin)], check=True)

    seed = Path(tmp) / "seed"
    subprocess.run(["git", "clone", "-q", str(origin), str(seed)], check=True)
    git(seed, "config", "user.email", "test@example.org")
    git(seed, "config", "user.name", "test")
    git(seed, "checkout", "-q", "-b", "main")
    write(seed, LEADS_REL, LEADS_HEADER + "base-1,2026-01-01,google_news,https://a\n")
    write(seed, SNAP_REL, SNAP_HEADER + "base-cut,111,Base University\n")
    git(seed, "add", "-A")
    git(seed, "commit", "-qm", "base")
    git(seed, "push", "-q", "origin", "main")
    git(origin, "symbolic-ref", "HEAD", "refs/heads/main")

    runner = Path(tmp) / "runner"
    subprocess.run(["git", "clone", "-q", str(origin), str(runner)], check=True)
    git(runner, "config", "user.email", "runner@example.org")
    git(runner, "config", "user.name", "runner")
    git(runner, "checkout", "-q", "main")

    # Backfill writer advances origin AFTER the runner cloned.
    ladder = Path(tmp) / "ladder"
    subprocess.run(["git", "clone", "-q", str(origin), str(ladder)], check=True)
    git(ladder, "config", "user.email", "ladder@example.org")
    git(ladder, "config", "user.name", "ladder")
    git(ladder, "checkout", "-q", "main")
    write(
        ladder,
        LEADS_REL,
        LEADS_HEADER
        + "base-1,2026-01-01,google_news,https://a\n"
        + "ladder-1,2026-07-14,google_news,https://b\n"
        + "ladder-2,2026-07-14,google_news,https://c\n",
    )
    write(ladder, SNAP_REL, SNAP_HEADER + "base-cut,111,Base University\n" + "ladder-cut,222,Ladder College\n")
    git(ladder, "add", "-A")
    git(ladder, "commit", "-qm", "ladder chunk")
    git(ladder, "push", "-q", "origin", "main")

    # Runner regenerates both files on the stale base (the conflict source).
    write(
        runner,
        LEADS_REL,
        LEADS_HEADER
        + "base-1,2026-01-01,google_news,https://a\n"
        + "weekly-1,2026-07-15,trade_feed,https://d\n",
    )
    write(runner, SNAP_REL, SNAP_HEADER + "base-cut,111,Base University\n" + "weekly-cut,333,Weekly University\n")
    return runner


def run_commit_script(runner):
    env = {
        **os.environ,
        "GITHUB_REF_NAME": "main",
        "GITHUB_HEAD_REF": "",
        # CI has no editor; the script must not need one.
        "GIT_EDITOR": "",
        "EDITOR": "",
        "TERM": "dumb",
    }
    return subprocess.run(
        [
            "bash",
            str(SCRIPT),
            "--commit-message",
            "chore: refresh data [skip ci]",
            "--add",
            LEADS_REL,
            "--add",
            SNAP_REL,
            "--conflict-path",
            LEADS_REL,
            "--conflict-path",
            SNAP_REL,
        ],
        cwd=runner,
        capture_output=True,
        text=True,
        env=env,
    )


def read_csv_ids(base, rel):
    with open(Path(base) / rel, encoding="utf-8", newline="") as fh:
        return [row[0] for row in csv.reader(fh)][1:]


def test_concurrent_writers():
    tmp = tempfile.mkdtemp(prefix="cap-test-")
    try:
        runner = build_scenario(tmp)
        result = run_commit_script(runner)
        assert result.returncode == 0, (
            f"script failed rc={result.returncode}\nstdout:{result.stdout[-800:]}\nstderr:{result.stderr[-800:]}"
        )
        assert "editor" not in result.stderr.lower(), f"editor error leaked: {result.stderr[-300:]}"

        verify = Path(tmp) / "verify"
        subprocess.run(
            ["git", "clone", "-q", str(Path(tmp) / "origin.git"), str(verify)], check=True
        )
        git(verify, "checkout", "-q", "main")
        lead_ids = read_csv_ids(verify, LEADS_REL)
        assert sorted(lead_ids) == ["base-1", "ladder-1", "ladder-2", "weekly-1"], (
            f"append-only union lost rows: {sorted(lead_ids)}"
        )
        snap_ids = read_csv_ids(verify, SNAP_REL)
        assert "weekly-cut" in snap_ids and "base-cut" in snap_ids, (
            f"snapshot should be the runner's version: {snap_ids}"
        )
        assert "ladder-cut" not in snap_ids, (
            f"snapshot conflict must resolve to the runner (theirs), got: {snap_ids}"
        )
    finally:
        shutil.rmtree(tmp, ignore_errors=True)


def test_clean_fast_forward_still_works():
    tmp = tempfile.mkdtemp(prefix="cap-ff-")
    try:
        runner = build_scenario(tmp)
        # Make the runner identical to origin first (no conflict), then add one row.
        git(runner, "fetch", "-q", "origin", "main")
        git(runner, "reset", "-q", "--hard", "origin/main")
        with open(Path(runner) / LEADS_REL, "a", encoding="utf-8", newline="\n") as fh:
            fh.write("weekly-9,2026-07-15,trade_feed,https://e\n")
        result = run_commit_script(runner)
        assert result.returncode == 0, f"clean push failed: {result.stderr[-400:]}"
        verify = Path(tmp) / "verify2"
        subprocess.run(
            ["git", "clone", "-q", str(Path(tmp) / "origin.git"), str(verify)], check=True
        )
        git(verify, "checkout", "-q", "main")
        assert "weekly-9" in read_csv_ids(verify, LEADS_REL)
    finally:
        shutil.rmtree(tmp, ignore_errors=True)


print("\n=== commit_and_push conflict-handling tests ===\n")
run("concurrent writers: union-merge leads, runner wins snapshots, no editor", test_concurrent_writers)
run("clean fast-forward path unaffected", test_clean_fast_forward_still_works)
print(f"\ncommit_and_push conflict tests: {passed} passed, {failed} failed.")
if failed:
    sys.exit(1)
