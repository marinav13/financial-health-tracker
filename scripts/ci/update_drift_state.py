#!/usr/bin/env python3
"""Track consecutive drift weeks per pipeline source.

Used by the weekly refresh workflow's drift-report step. The state file
maps source name -> consecutive drift-week count. A drifted source
increments; a clean known source resets to 0. --check-only reads the
committed state and exits 1 when any source has reached the threshold,
turning chronic ignored drift into a red run without blocking the data
push (the check runs as the workflow's final step).
"""

import argparse
import json
import sys
from pathlib import Path

KNOWN_SOURCES = [
    "accreditation_scrapers",
    "dapip",
    "cuts_api",
    "cuts_discovery",
    "grant_witness",
    "review_decision_quarantine",
    "review_gate_anomaly",
]


def load_state(path):
    if not path.exists():
        return {}
    try:
        data = json.loads(path.read_text(encoding="utf-8"))
    except (json.JSONDecodeError, OSError):
        print(f"drift-state: could not parse {path}; starting fresh", file=sys.stderr)
        return {}
    if not isinstance(data, dict):
        return {}
    return {str(k): int(v) for k, v in data.items() if isinstance(v, (int, float))}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--state", required=True, help="path to drift state JSON")
    parser.add_argument("--threshold", type=int, default=3)
    parser.add_argument(
        "--drifted",
        default="",
        help="comma-separated source names that drifted this run",
    )
    parser.add_argument(
        "--check-only",
        action="store_true",
        help="do not modify state; exit 1 if any source is at/over threshold",
    )
    args = parser.parse_args()

    state_path = Path(args.state)
    state = load_state(state_path)

    if args.check_only:
        breached = {s: c for s, c in state.items() if c >= args.threshold}
        if breached:
            for source, count in sorted(breached.items()):
                print(
                    f"CHRONIC DRIFT: {source} has drifted {count} consecutive "
                    f"week(s) (threshold {args.threshold})"
                )
            return 1
        print("drift-state: no source at chronic threshold")
        return 0

    drifted = {s.strip() for s in args.drifted.split(",") if s.strip()}
    unknown = drifted - set(KNOWN_SOURCES)
    if unknown:
        print(
            f"drift-state: ignoring unknown source name(s): {', '.join(sorted(unknown))}",
            file=sys.stderr,
        )
        drifted &= set(KNOWN_SOURCES)

    new_state = {}
    for source in KNOWN_SOURCES:
        previous = state.get(source, 0)
        new_state[source] = previous + 1 if source in drifted else 0

    state_path.parent.mkdir(parents=True, exist_ok=True)
    state_path.write_bytes(
        (json.dumps(new_state, indent=2, sort_keys=True) + "\n").encode("utf-8")
    )

    for source, count in sorted(new_state.items()):
        if count >= args.threshold:
            print(
                f"CHRONIC DRIFT: {source} has drifted {count} consecutive "
                f"week(s) (threshold {args.threshold})"
            )
        elif count:
            print(f"drift-state: {source} at {count} consecutive drift week(s)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
