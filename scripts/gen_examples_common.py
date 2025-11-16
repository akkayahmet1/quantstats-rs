#!/usr/bin/env python3
"""
Generate examples/common.rs from the local `data` file.

Usage (from repo root):

    python3 scripts/gen_examples_common.py

This reads the `time`, `returns`, and `benchmark` arrays from the `data` file
and emits a Rust module `examples/common.rs` exposing:

    pub fn demo_strategy() -> ReturnSeries
    pub fn demo_benchmark() -> ReturnSeries

The generated code is used by the example binaries.
"""

import ast
import re
from pathlib import Path


def main() -> None:
    root = Path(__file__).resolve().parent.parent
    data_path = root / "data"
    out_path = root / "examples" / "common.rs"

    text = data_path.read_text(encoding="utf-8")

    # Extract raw content between [...] for each field
    m_time = re.search(r"time:\s*\[(.*?)\]", text, re.S)
    m_ret = re.search(r"returns:\s*\[(.*?)\]", text, re.S)
    m_bench = re.search(r"benchmark:\s*\[(.*?)\]", text, re.S)

    if not (m_time and m_ret and m_bench):
        raise SystemExit("expected `time`, `returns`, and `benchmark` arrays in `data`")

    # Parse dates from Timestamp('YYYY-MM-DD ...') items
    dates_part = m_time.group(1)
    dates = re.findall(r"Timestamp\('([0-9]{4})-([0-9]{2})-([0-9]{2})", dates_part)
    if not dates:
        raise SystemExit("failed to parse any dates from `time` field")

    # Safely parse numeric lists using literal_eval
    returns_list = ast.literal_eval("[" + m_ret.group(1) + "]")
    bench_list = ast.literal_eval("[" + m_bench.group(1) + "]")

    # Ensure all arrays have the same length by trimming to the minimum
    n = min(len(dates), len(returns_list), len(bench_list))
    dates = dates[:n]
    returns_list = returns_list[:n]
    bench_list = bench_list[:n]

    out_lines: list[str] = []
    out_lines.append("// Auto-generated from `data` by scripts/gen_examples_common.py; do not edit by hand.")
    out_lines.append("use chrono::NaiveDate;")
    out_lines.append("use quantstats_rs::ReturnSeries;")
    out_lines.append("")

    # Strategy series
    out_lines.append("pub fn demo_strategy() -> ReturnSeries {")
    out_lines.append("    let dates = vec![")
    for y, m, d in dates:
        out_lines.append(
            f"        NaiveDate::from_ymd_opt({int(y)}, {int(m)}, {int(d)}).unwrap(),"
        )
    out_lines.append("    ];")
    out_lines.append("")
    out_lines.append("    let returns = vec![")
    for v in returns_list:
        out_lines.append(f"        {v},")
    out_lines.append("    ];")
    out_lines.append("")
    out_lines.append(
        '    ReturnSeries::new(dates, returns, Some("Demo Strategy".to_string())).unwrap()'
    )
    out_lines.append("}")
    out_lines.append("")

    # Benchmark series
    out_lines.append("pub fn demo_benchmark() -> ReturnSeries {")
    out_lines.append("    let dates = vec![")
    for y, m, d in dates:
        out_lines.append(
            f"        NaiveDate::from_ymd_opt({int(y)}, {int(m)}, {int(d)}).unwrap(),"
        )
    out_lines.append("    ];")
    out_lines.append("")
    out_lines.append("    let returns = vec![")
    for v in bench_list:
        out_lines.append(f"        {v},")
    out_lines.append("    ];")
    out_lines.append("")
    out_lines.append(
        '    ReturnSeries::new(dates, returns, Some("Benchmark".to_string())).unwrap()'
    )
    out_lines.append("}")
    out_lines.append("")

    out_path.write_text("\n".join(out_lines), encoding="utf-8")
    print(f"wrote {out_path} with {n} points")


if __name__ == "__main__":
    main()

