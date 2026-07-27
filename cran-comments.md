## Test environments
* local OS X install, R 4.3.2
* macos-latest (release), on GitHub, R 4.6.1
* ubuntu-latest (devel), on GitHub, R 4.7.0
* ubuntu-latest (oldrel-1), on GitHub, R 4.5.3
* ubuntu-latest (release), on GitHub, R 4.6.1
* windows-latest (release), on GitHub, R 4.6.1

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE on the local OS X check only ("unable to verify current time"), which is a
sandbox networking artifact (no NTP access) and does not appear on any of the GitHub Actions
test environments above, which all pass with 0 errors, 0 warnings, 0 notes.

## Change notes
This release (0.2.0) bundles three sets of changes since the last CRAN release (0.1.2):

- Fixes `write_plot()` so `useDingbats = FALSE` is actually applied for PDF output. The
  previous code compared the literal string `'format'` against `'pdf'` instead of checking
  the `format` argument, so the fix never took effect.
- `initialize_startr()` now defaults `options(repos)` to Posit Package Manager's rolling
  binary snapshot instead of leaving it at CRAN's default, since CRAN doesn't always have a
  binary available for every platform/R-version combination for heavy compiled packages
  (`arrow`, `sf`, etc.), which otherwise silently falls back to a slow source compile. This
  is configurable via a new `repos` argument, including opting out entirely with `repos = NULL`.
- Moves `sf`, `tgamtheme` and `beepr` from `Imports` to `Suggests`, since they're only needed
  by a subset of users (heavy system dependencies for `sf`; `tgamtheme` and beep notifications
  are optional conveniences). Call sites now check for the package first via `requireNamespace()`
  and fail with a clear message (or silently skip, for beeps) instead of hard-requiring
  installation.

See NEWS.md for full details.
