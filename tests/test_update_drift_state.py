"""Standalone tests for scripts/ci/update_drift_state.py (run directly)."""

import json
import subprocess
import sys
import tempfile
from pathlib import Path

SCRIPT = Path(__file__).resolve().parents[1] / "scripts" / "ci" / "update_drift_state.py"


def run(args):
    return subprocess.run(
        [sys.executable, str(SCRIPT)] + args,
        capture_output=True,
        text=True,
    )


def main():
    with tempfile.TemporaryDirectory() as tmp:
        state = Path(tmp) / "drift_state.json"

        # Fresh state: one drifted source starts at 1, others 0.
        result = run(["--state", str(state), "--drifted", "dapip"])
        assert result.returncode == 0, result.stderr
        data = json.loads(state.read_text())
        assert data["dapip"] == 1, data
        assert data["cuts_api"] == 0, data
        assert data["cuts_discovery"] == 0, data

        # Consecutive drift increments; clean sources stay 0.
        run(["--state", str(state), "--drifted", "dapip"])
        data = json.loads(state.read_text())
        assert data["dapip"] == 2, data

        # Clean week resets.
        run(["--state", str(state), "--drifted", ""])
        data = json.loads(state.read_text())
        assert data["dapip"] == 0, data

        # Threshold breach is reported in update mode output.
        for _ in range(3):
            result = run(["--state", str(state), "--drifted", "grant_witness"])
        assert "CHRONIC DRIFT: grant_witness" in result.stdout, result.stdout

        # check-only exits 1 at threshold and does not modify state.
        before = state.read_text()
        result = run(["--state", str(state), "--threshold", "3", "--check-only"])
        assert result.returncode == 1, (result.returncode, result.stdout)
        assert state.read_text() == before

        # check-only exits 0 below threshold.
        run(["--state", str(state), "--drifted", ""])
        result = run(["--state", str(state), "--threshold", "3", "--check-only"])
        assert result.returncode == 0, (result.returncode, result.stdout)

        # Unknown source names are ignored, not stored.
        run(["--state", str(state), "--drifted", "bogus_source,dapip"])
        data = json.loads(state.read_text())
        assert "bogus_source" not in data, data
        assert data["dapip"] == 1, data

        # Corrupt state file starts fresh instead of crashing.
        state.write_text("not json")
        result = run(["--state", str(state), "--drifted", "dapip"])
        assert result.returncode == 0, result.stderr
        data = json.loads(state.read_text())
        assert data["dapip"] == 1, data

    print("update_drift_state tests: all passed.")


if __name__ == "__main__":
    main()
