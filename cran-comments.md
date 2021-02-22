## Test environments
* local OS X install, R 3.5.1
* ubuntu 20.04 (on GitHub), R 4.0.3
* win 20.04 (on GitHub), R 4.0.3
* win-builder (devel and release)

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs.

## Change notes
Two small bug fixes:
- Adds an extra newline to the `initialize_startr()` log messages so that it stays on its own line.
- Fixes an issue with `write_plot()` where the `format` parameter was inadvertently being passed to `ggsave()`, which would then throw an error.
