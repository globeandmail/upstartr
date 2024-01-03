## Test environments
* local OS X install, R 4.3.2
* macos-latest (release), on GitHub, R 4.3.2
* ubuntu-latest (devel), on GitHub, R 2023-12-31 r85754
* ubuntu-latest (oldrel-1), on GitHub, R 4.2.3
* ubuntu-latest (release), on GitHub, R 4.3.2
* windows-latest (release), on GitHub, R 4.3.2

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs.

## Change notes
Small bug fix:
- Fixes an error where `initialize_startr()` breaks if `cancensus` is among listed packages due to an errant comma.
