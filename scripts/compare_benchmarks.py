#!/usr/bin/env python3
"""
Compare two CatNF benchmark outputs (from `lake exe bench`).

Expects lines containing:
  CATNF_BENCH <name> p50_ms=<n> p95_ms=<n> avg_ms=<x>

Exit code 1 if any shared benchmark's p50 regresses by more than the
allowed ratio (default 1.10) compared to the baseline file.
"""

from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path
from typing import Dict, Tuple


LINE_RE = re.compile(
    r"^CATNF_BENCH\s+(\S+)\s+p50_ms=(\d+)\s+p95_ms=(\d+)\s+avg_ms=([0-9.]+)\s*$"
)


def parse_bench_file(path: Path) -> Dict[str, Tuple[int, int, float]]:
    out: Dict[str, Tuple[int, int, float]] = {}
    text = path.read_text(encoding="utf-8", errors="replace")
    for line in text.splitlines():
        m = LINE_RE.match(line.strip())
        if not m:
            continue
        name, p50, p95, avg = m.group(1), int(m.group(2)), int(m.group(3)), float(m.group(4))
        out[name] = (p50, p95, avg)
    return out


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("current", type=Path, help="New benchmark output")
    ap.add_argument("baseline", type=Path, help="Baseline benchmark output")
    ap.add_argument(
        "--ratio",
        type=float,
        default=1.10,
        help="Fail if current p50 > baseline p50 * ratio (default: 1.10)",
    )
    args = ap.parse_args()

    cur = parse_bench_file(args.current)
    base = parse_bench_file(args.baseline)
    if not cur:
        print("error: no CATNF_BENCH lines in current file", file=sys.stderr)
        return 2
    if not base:
        print("error: no CATNF_BENCH lines in baseline file", file=sys.stderr)
        return 2

    names = sorted(set(cur) & set(base))
    if not names:
        print("error: no benchmark names in common between files", file=sys.stderr)
        return 2

    failed = False
    for name in names:
        c50, _, _ = cur[name]
        b50, _, _ = base[name]
        limit = b50 * args.ratio
        if c50 > limit:
            print(
                f"regression: {name} p50_ms={c50} > baseline {b50} * {args.ratio} = {limit:.1f}",
                file=sys.stderr,
            )
            failed = True
        else:
            print(f"ok: {name} p50_ms={c50} (baseline p50_ms={b50})")

    return 1 if failed else 0


if __name__ == "__main__":
    raise SystemExit(main())
