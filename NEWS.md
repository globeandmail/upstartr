Version 0.2.0
====================

- `initialize_startr()` now defaults `options(repos)` to Posit Package Manager's rolling binary
  snapshot instead of leaving it at CRAN's default. CRAN doesn't always have a binary available
  for every platform/R-version combination for heavy compiled packages (`arrow`, `sf`, etc.),
  which silently falls back to a source compile that can take many minutes, or fail outright on
  a machine without build tools. Set `initialize_startr(repos = NULL)` to leave your existing
  `repos` option untouched (e.g. if your organization already runs its own mirror), or pass a
  different URL to use a different one.

Version 0.1.3
====================

Small bug fix:
- Fixes `write_plot()` so `useDingbats = FALSE` is actually applied for PDF output (closes #3). The
  previous code compared the literal string `'format'` against `'pdf'` instead of checking the
  `format` argument, so the fix never took effect.
- Adds test coverage for `write_plot()` and `write_excel()`.

Version 0.1.2
====================

Small bug fix:
- Fixes an error where `initialize_startr()` breaks if `cancensus` is among listed packages due to an errant comma.

Version 0.1.1
====================

Small bug fixes:
- Adds an extra newline to the `initialize_startr()` log messages so that it stays on its own line.
- Fixes an issue with `write_plot()` where the `format` parameter was inadvertently being passed to `ggsave()`, which would then throw an error.

Version 0.1.0
====================

Inaugural release! 🎉
