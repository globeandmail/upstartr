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
