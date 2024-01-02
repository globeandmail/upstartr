## Test environments
* local OS X install, R 3.5.1
* ubuntu 20.04 (on GitHub), R 4.0.3
* win 20.04 (on GitHub), R 4.0.3
* win-builder (devel and release)

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs.

## Change notes
Small bug fix:
- Fixes an error where `initialize_startr()` breaks if `cancensus` is among listed packages due to an errant comma.
