"""
Module for data reading functions, emulating Fortran I/O.

Includes functions for reading fixed and free formats, and handling data types.
"""

import re
from fortranformat import FortranRecordReader, InvalidFormat
from .exceptions import EndOfFile

# Global caches
_reader_cache = {}


def get_cached_reader(fmt: str) -> FortranRecordReader:
    """
    Returns a FortranRecordReader for the given format, using caching.

    Args:
    fmt (str): The Fortran format string.

    Returns:
    FortranRecordReader: An instance of FortranRecordReader.
    """
    if fmt not in _reader_cache:
        _reader_cache[fmt] = FortranRecordReader(fmt)
    return _reader_cache[fmt]


def split_fortran_free_format(line):
    """
    Emulates Fortran-style token separation using READ(*,*) using stateful parsing.
    Recognizes quoted strings (single or double) as a single field,
    handling double quote escapes (e.g., '' inside 'string').
    """
    # Regex to handle single or double quotes and quotation marks
    # escapes (double quote for a literal quote inside)
    tokens = []
    current_token = []
    in_single_quote = False
    in_double_quote = False

    i = 0
    n = len(line)

    while i < n:
        char = line[i]

        if in_single_quote:
            if char == "'":
                # Check for escaped single quote ''
                if i + 1 < n and line[i + 1] == "'":
                    current_token.append("'")  # Add a single quote
                    i += 1  # Consume the second quote of the escape
                else:
                    # End of single-quoted string
                    in_single_quote = False
                    tokens.append("".join(current_token))
                    current_token = []
            else:
                current_token.append(char)
        elif in_double_quote:
            if char == '"':
                # Check for escaped double quote ""
                if i + 1 < n and line[i + 1] == '"':
                    current_token.append('"')  # Add a double quote
                    i += 1  # Consume the second quote of the escape
                else:
                    # End of double-quoted string
                    in_double_quote = False
                    tokens.append("".join(current_token))
                    current_token = []
            else:
                current_token.append(char)
        else:  # Not in a quote (parsing for new token or delimiter)
            if char == "'":
                in_single_quote = True
            elif char == '"':
                in_double_quote = True
            elif char.isspace() or char == ",":  # Delimiters outside quotes
                if current_token:  # If we have a token accumulated, add it
                    tokens.append("".join(current_token))
                    current_token = []
                # Consume all consecutive delimiters (spaces or commas)
                while i < n and (line[i].isspace() or line[i] == ","):
                    i += 1
                i -= 1  # Adjust i back as it will be incremented at end of loop for the current char
            else:
                current_token.append(char)
        i += 1

    # Add any remaining token after the loop finishes (e.g., the last token if no trailing delimiter)
    if current_token:
        tokens.append("".join(current_token))

    return tokens


def infer_type(s: str):
    """
    Infers the data type (int, float, or str) from a string.

    Attempts to convert the string to int, then to float. If both fail,
    returns the original string without leading or trailing whitespace.
    Handles scientific notation with 'D' (Fortran) by converting it to 'E' (Python).

    Args:
    s (str): The string from which to infer the type.

    Returns:
    int or float or str or None: The converted value or the original string.
    """
    if not s:
        return None

    try:
        return int(s)
    except ValueError:
        try:
            # Replace 'D' with 'E' to handle Fortran scientific notation
            return float(s.replace("D", "E"))
        except ValueError:
            return s.strip()


def line_sanity(line: str, tabsize: int = 8) -> str:
    """
    Expands tabs to spaces to preserve column alignment, which is
    required for parsing fixed formats in Fortran.

    Args:
    line (str): Line of text to be sanitized.
    tabsize (int): How many spaces a tab represents. Default is 8.

    Returns:
    str: Line ready to be read with fortranformat.
    """
    return line.expandtabs(tabsize)


def read(fp, fmt: str = None):
    """
    Reads a line from a file pointer or a string with or without Fortran formatting
    and returns the parsed values.

    Args:
    fp (file-like object or str): Pointer to a file (with readline method) or a string.
    fmt (str, optional): The Fortran format string to apply.
    If None or '*', free format is used.
    If '(A)', the entire line is read as a string.
    Defaults to None.

    Returns:
    list or any: The parsed values. Can be a list if there are multiple values, a single
                 value if there is only one, or `EndOfFile` if the end of the file is reached.

    Raises:
    TypeError: If `fp` is not a valid file pointer or string.
    InvalidFormat: If the format is invalid for reading.
    """
    if fp is None or (not isinstance(fp, str) and not hasattr(fp, "readline")):
        raise TypeError(f"`fp` must be a valid file pointer or a string, no {type(fp).__name__}")

    line = fp if isinstance(fp, str) else fp.readline()

    if not line:
        return EndOfFile  # Returns the EndOfFile object to indicate the end of the file

    line = line_sanity(line)

    if fmt is None or fmt == "*":
        # Free-form reading, separating by spaces or commas and handling quotes
        raw_tokens = split_fortran_free_format(line)
        return [infer_type(val) for val in raw_tokens]

    if fmt == "(A)":
        # Read the entire line as a string, removing only the newline
        return line.strip("\n")

    try:
        reader = get_cached_reader(fmt)
        values = reader.read(line)
        # If fortranformat returns a single-element list, return the element directly
        return values[0] if len(values) == 1 else values
    except InvalidFormat as e:
        # Re-throw the exception with a clearer message for the user
        raise InvalidFormat(f"'{fmt}' edit descriptor not permitted on input") from e
