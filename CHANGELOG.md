# Changelog

All notable features of this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.1.0] - 2025-07-15

### Added
- Initial project structure for publishing on PyPI.
- Modularization of the reading (`readers.py`) and writing (`writers.py`) functions.
- Implementation of `read()` for fixed- and free-format reading (Fortran-like and Python-like).
- Implementation of `write()` for fixed- and free-format writing (Fortran-like and Python-like).

- Cache handling for `FortranRecordReader` and `FortranRecordWriter`.
- Type inference for free-format reading.
- Tab expansion to sanitize input lines (`line_sanity`).
- `EndOfFile` exception to indicate the end of a read stream.
- `pyproject.toml` file for package configuration.
- `README.md`, `LICENSE` (MIT), and `_version.py` files.
- `tests/basic_tests.py` basic test script.

### Fixed
- Fixed handling of quoted strings when reading free-format (`split_fortran_free_format`).
- Improved error messages for `InvalidFormat` when reading and writing.