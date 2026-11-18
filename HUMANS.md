# HUMANS.md — android-usage.el

A guide for humans navigating and developing this codebase. Written
2026-11-18 per the org-wide rule that every agent-developed repo
carries one.

## What this is

A single-file Emacs package (`android-usage.el`, ~1140 lines) that
pulls Android's Usage Access data (the same per-app screen time the
system shows StayFree) into a plain Org file: one table per day, a
StayFree-style top-apps report, and a `#+BEGIN: android-usage`
dynamic block with SVG charts. Everything stays on the device.
STATUS 2026-11-18: DEPRECATED - the CSV/JSON import lane now lives
in the successor repo `org-activity`, which also carries the native
patched-Emacs lane; this repo is kept for history.

The bridge, stated honestly in the README: stock Emacs on Android has
no Java interop, so Elisp cannot call UsageStatsManager. One export
step is needed — a usage app that holds the permission exports CSV or
JSON (StayFree's own export works today); this package imports it.
The native no-export lane lives in the `org-activity` repo (a patched
Emacs primitive), not here.

## Mental model

A pipeline with a swappable front:

1. **Pull** — `android-usage-pull-function` (default
   `android-usage--pull-latest-file`) grabs the newest matching export
   from `android-usage-import-dir`. This is the seam the native lane
   replaces.
2. **Parse** — `android-usage-parse` dispatches on file suffix to the
   CSV or JSON reader; both normalize rows to
   `(DAY PACKAGE SECS)`.
3. **Write** — `android-usage--write-day` merges rows into the day's
   Org table, idempotently (re-importing the same export does not
   double-count).
4. **Read back** — `android-usage--read-day-tables` parses the Org
   tables back out; reports and dynamic blocks are computed from the
   Org file, not from cached parse state.

## Code map (android-usage.el)

| Symbol | Role |
|---|---|
| `android-usage-org-file` / `android-usage-import-dir` | where the Org journal lives; where exports are looked for. |
| `android-usage-pull-function` | the swappable import seam. |
| `android-usage--split-csv` / `android-usage--parse-csv` | RFC-4180-ish CSV reader with quoted-field handling. |
| `android-usage--detect-columns` | header matching against `android-usage--header-candidates` (StayFree and friends spell columns differently). |
| `android-usage--sniff-duration-unit` + `android-usage-duration-unit` | StayFree exports unitless seconds/minutes depending on version; the sniffer decides before any math. |
| `android-usage--parse-hms` / `android-usage--duration-secs` | "1:23:45" and unitless durations to seconds. |
| `android-usage--parse-json` | the JSON lane (package / usage fields). |
| `android-usage--aggregate` | rows to per-package seconds per day. |
| `android-usage--write-day` / `android-usage--day-table` | idempotent day-table writer under a date heading. |
| `android-usage--read-day-tables` / `android-usage--range-table` | Org-file readers for reports. |
| `android-usage--svg-hbars` / `android-usage--svg-vbars` | pure-Elisp SVG chart renderers (no external tools). |
| `org-dblock-write:android-usage` / `org-dblock-write:android-usage-day` | the dynamic blocks. |
| `android-usage-import` / `android-usage-refresh` / `android-usage-report` | user commands. |

## Core algorithms, in prose

**Column detection.** Export headers are normalized (case, spacing)
and matched against per-field candidate lists, so StayFree's
"Package Name"/"Usage" and other apps' variants resolve to the same
logical columns; unknown headers fall back to position heuristics
recorded in `android-usage-column-map` for the next run.

**Unit sniffing.** Some exports carry durations with no unit (seconds
in one version, minutes in another). The sniffer looks at the
magnitude distribution of the parsed column to pick a unit once per
file; `android-usage-duration-unit` pins it by hand when :auto gets it
wrong.

**Idempotent merge.** A day's table is rebuilt from the union of what
the Org file already has and what the import adds, keyed by package;
importing overlapping exports converges instead of accumulating.

## Invariants and gotchas

- The Org file is the database: reports read the Org tables back, so
  hand-edits to the tables are respected, not clobbered. Keep it that
  way.
- Package names, not app labels, are the key; `android-usage-ignore-packages`
  filters launcher/keyboard noise before aggregation.
- `android-usage.elc` and the `bin/` scratch dir are artifacts, not
  sources.
- Do the unit sniffing before any arithmetic — a wrong unit silently
  scales every number.

## Developing

```sh
emacs -Q --batch -L . -f batch-byte-compile android-usage.el   # 0 warnings
emacs -Q --batch -L . -l test.el -f ert-run-tests-batch-and-exit
```

Branch law: land on `ai/main`.
