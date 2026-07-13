"""Standalone tests for discovery ID and URL normalization helpers."""

import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(REPO_ROOT / "scripts"))

from cuts_discovery.common import discovered_cut_id, lead_id_for, normalize_url  # noqa: E402


def main():
    url_a = "https://example.org/story?utm_source=newsletter&x=1"
    url_b = "https://example.org/story?x=1&utm_campaign=weekly"
    assert normalize_url(url_a) == "https://example.org/story?x=1"
    assert normalize_url(url_b) == "https://example.org/story?x=1"
    assert lead_id_for(url_a) == lead_id_for(url_b)

    cut_a = discovered_cut_id("123456", "staff_layoff", "2026-07-09", url_a)
    cut_b = discovered_cut_id("123456", "staff_layoff", "2026-07-09", url_b)
    cut_c = discovered_cut_id("123456", "program_suspension", "2026-07-09", url_b)
    assert cut_a == cut_b
    assert cut_a.startswith("discovered-")
    assert cut_c != cut_a

    print("cuts discovery id tests: all passed.")


if __name__ == "__main__":
    main()
