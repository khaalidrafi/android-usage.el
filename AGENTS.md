# AGENTS.md — khaalidrafi/android-usage.el

Local law for agents working in this repo. Global owner rules apply on
top (Guix-first machine, simple code, tests before implementation,
commit early/often, never procrastinate).

## Identity & commits (HARD LAW)

- Author every commit as your model identity: `MODEL <model@provider>`
  (e.g. `GLM-5.3-Flash <glm-5.3-flash@z.ai>`).
- End every commit message with BOTH trailers:

        Co-authored-by: <agent/harness> <agent@harness>
        Co-authored-by: Khalid Rafi <khaalidrafi@gmail.com>

  Canonical owner email is `khaalidrafi@gmail.com` — no variants.
- One model per commit. Never mix agents' work in one commit.
- Never commit directly to `main`. Branch `ai/main` is the working
  default; feature work on `ai/<topic>`.
- Descriptive subjects; put what comments can't hold in the body.

## This package

- Target: GNU Emacs 29.1+, including the Android port (emacs 30,
  feature/android). No external processes, no gnuplot/ImageMagick —
  charts are pure-Elisp SVG strings.
- The data bridge is an export file (StayFree CSV / canonical JSON);
  Emacs cannot call UsageStatsManager (no Java interop). Keep that
  honesty in README and code comments — do not promise direct pulls.
- Gate before any push: `make compile` (byte-compile-error-on-warn t,
  zero warnings) and `make test` (ERT, all green) on emacs 30.1.
- Tests live in `test.el`; add tests BEFORE the fix/feature they cover.
- Keep code simple and short; comment the WHY, not the what.
- Record anything the owner should personally review in AI-REVIEW.org.
