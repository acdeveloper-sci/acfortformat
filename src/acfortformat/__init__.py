"""
acfortformat - A package for reading and writing data in Fortran and Python.

This package provides utilities for handling fixed and free-format data I/O,
emulating Fortran's FORMAT functionality and also support native Python-style
reading and writing.
"""

from .exceptions import EndOfFile
from .readers import read
from .writers import write

# Import the package version
from ._version import __version__

__all__ = ["read", "write", "EndOfFile"]
