# CLAUDE.md

`upstartr` is the utility package behind [`startr`](https://github.com/globeandmail/startr),
The Globe and Mail's data journalism project template, and it's **published on CRAN** —
that last fact governs most of the constraints below.

## Structure

One file per concern in `R/`: `init.R` (`initialize_startr()`), `run.R` (`run_config()`,
`run_process()`, `run_analyze()`, etc., plus the internal `maybe_beep()` helper), `dir.R`
(path helpers), `read.R`/`write.R` (file I/O), `graphics.R` (ggplot scale helpers),
`helpers.R` (string/data cleaning utilities). Tests in `tests/testthat/` roughly mirror
this, one `test-*.R` per source file.

## Versioning and NEWS

Bump `DESCRIPTION`'s `Version` and add a `NEWS.md` entry for every user-facing change,
following R's loose semver convention: patch for pure bug fixes, minor for new
backward-compatible features or a loosened dependency contract (e.g. Imports → Suggests),
major reserved for an actual breaking API change (hasn't happened yet — package is pre-1.0).
If you're bumping the version across several small, separately-reviewed PRs before any of
them have been released, don't sweat picking the exact right number for each - once they're
about to ship together as a single CRAN release, collapse them into one clean bump rather
than stacking patch-on-minor-on-patch.

## Testing gotchas (both cost real time to find)

- **Any test that calls `initialize_startr()` (or anything else touching `options()`) must
  scope and restore every option it might set.** A plain `old <- options(); on.exit(options(old))`
  snapshot does *not* work — `options()` can't "unset" a key that didn't exist before the
  test, so a newly-created option (e.g. `startr.should_timestamp_output_files`) leaks into
  later tests in the same run. This actually broke an unrelated `write_excel()` test the
  first time. Use `withr::local_options()` with each option's pre-test value fetched via
  `getOption()` explicitly — withr handles the "didn't exist before" case correctly.
- **Declare every package used via `::` in a test file in `DESCRIPTION`'s `Suggests`,
  including test-only ones like `withr`.** `R CMD check` flags this as a WARNING
  ("checking for unstated dependencies in tests"), and CI runs with `error-on: "warning"`,
  so this fails the build, not just a lint nag.
- For anything gated on a Suggested heavy dependency (`sf`, `tgamtheme`, `beepr`), guard both
  the implementation (`requireNamespace()`) and the test (`skip_if_not_installed()`).

## Dependencies

- Keep heavy or optional dependencies in `Suggests`, not `Imports`, guarded by
  `requireNamespace()` at the call site with a clear error (or silent no-op, for
  conveniences like beeps). `sf`, `tgamtheme`, and `beepr` already went through this.
- `initialize_startr()` defaults `options(repos)` to Posit Package Manager's binary mirror
  specifically because CRAN doesn't reliably have binaries for heavy compiled packages on
  every platform (especially Linux, which CRAN never provides binaries for at all) — without
  it, installing something like `arrow` can silently trigger a 20+ minute source compile.
  Don't remove this without understanding why it's there.
- To regenerate the Linux system-library list used by CI's apt-cache step:
  `pak::pkg_sysreqs(".", sysreqs_platform = "ubuntu-22.04", dependencies = TRUE)`.

## CI

`.github/workflows/R-CMD-check.yaml` runs the full 5-job matrix (macOS/Windows/Ubuntu ×
devel/release/oldrel-1) **on purpose** — because this package is on CRAN, and CRAN's own
check farm exercises this same spread. Don't trim it for "simplicity"; that would just
reduce the local signal for exactly the kind of breakage CRAN would otherwise catch after
submission. R package caching is already handled by `setup-r-dependencies@v2` (built in,
keyed on OS/R-version/lockfile hash) - if CI feels slow, the apt-installed system library
step is the more likely place to look, not the R package cache.

## CRAN submission

Actually submitting (`devtools::submit_cran()` or https://cran.r-project.org/submit.html)
needs the maintainer to do it - it emails a confirmation link to whoever's listed in
`DESCRIPTION`. Before handing off:
- Refresh `cran-comments.md`: pull the *actual* R versions from the current CI matrix's job
  logs (they drift over time - don't reuse a previous submission's numbers), and summarize
  what's changed since the last real CRAN release (check `Date/Publication` in
  `https://crandb.r-pkg.org/upstartr` if unsure what's actually live).
- Run `R CMD build` then `R CMD check --as-cran` on the resulting tarball - this is the most
  authoritative local check, more thorough than a plain `devtools::check()`.
- If PDF manual generation fails locally with "pdflatex is not available" (check
  `<pkg>.Rcheck/Rdlatex.log`), that's a missing local TeX install, not a real Rd defect -
  confirm the R CMD check `HTML`/other checks are clean and don't block on this; CI's
  `check-r-package@v2` passes `--no-manual` and skips it anyway, and CRAN's own
  infrastructure always has a working TeX setup.

## Related repos

- [`startr`](https://github.com/globeandmail/startr) - the project template that consumes
  this package via `install.packages('upstartr')`. Changes here don't reach `startr` users
  until a new version is actually released to CRAN.
- [`startr-cli`](https://github.com/globeandmail/startr-cli) - scaffolds new `startr`
  projects by cloning that repo; doesn't touch `upstartr` directly.
