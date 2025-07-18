"""
Module for data writing functions, emulating Fortran I/O.

Includes functions for writing to fixed or free formats, and output to console or file.
"""

from fortranformat import FortranRecordWriter, InvalidFormat

# Global caches
_writer_cache = {}


def get_cached_writer(fmt: str) -> FortranRecordWriter:
    """
    Returns a FortranRecordWriter for the given format, using caching.

    Args:
        fmt (str): The Fortran format string.

    Returns:
        FortranRecordWriter: An instance of FortranRecordWriter.
    """
    if fmt not in _writer_cache:
        _writer_cache[fmt] = FortranRecordWriter(fmt)
    return _writer_cache[fmt]


def write(data, fp=None, con: bool = False, fmt: str = None) -> str:
    """
    Writes Fortran or unformatted data to a file or the console.

    Args:
        data (any): The data to write. Can be a single value or a list/tuple.
        fp (file-like object, optional): Pointer to a file (with write method).
                                         If None, output goes to the console if `con` is True.
                                         Defaults to None.
        con (bool, optional): If True, output is also printed to the console.
                              If `fp` is None, `con` is forced to True. Defaults to False.
        fmt (str, optional): The Fortran format string to apply.
                             If None or '*', free formatting is used. Defaults to None.

    Returns:
        str: The text string that was written.

    Raises:
        TypeError: If `fp` is not a valid file pointer.
        InvalidFormat: If the Fortran format is invalid for writing.
        RuntimeError: If an error occurs while writing to the file.
    """
    if fp and not hasattr(fp, "write"):
        raise TypeError(f"`fp` must be a valid file pointer, no {type(fp).__name__}")

    # If no file pointer is provided, force output to console
    if fp is None:
        con = True

    if not isinstance(data, (list, tuple)):
        data = [data]  # Ensures that data is always a list/tuple

    msg_str = ""
    if fmt is None or fmt == "*":
        # Free format
        # Wrap strings with spaces/commas in single quotes to
        # to be read as a single entity by READ(*,*)
        formatted_elements = []
        for item in data:
            if isinstance(item, str):
                # If the string contains spaces or commas, we wrap it in single quotes.
                # Logic to determine if quoting is needed and to handle internal escapes ('', "")
                needs_quoting = " " in item or "," in item or not item or "'" in item or '"' in item

                if needs_quoting:
                    use_double_quotes_outer = False
                    # Decide which outer quote to use based on internal quotes for better Fortran compatibility
                    if "'" in item and '"' not in item:
                        # If string has single quotes but no double quotes, use double quotes for outer delimiter
                        use_double_quotes_outer = True
                    elif '"' in item and "'" not in item:
                        # If string has double quotes but no single quotes, use single quotes for outer delimiter
                        use_double_quotes_outer = False  # Default to single
                    else:
                        # If string has both types of quotes, or neither, default to single quotes for outer delimiter
                        # and escape single quotes internally. This prioritizes '' escaping.
                        use_double_quotes_outer = False

                    if use_double_quotes_outer:
                        # Escape internal double quotes by doubling them when outer is double quotes
                        escaped_content = item.replace('"', '""')
                        formatted_elements.append(f'"{escaped_content}"')
                    else:
                        # Escape internal single quotes by doubling them when outer is single quotes
                        escaped_content = item.replace("'", "''")
                        formatted_elements.append(f"'{escaped_content}'")

            else:
                formatted_elements.append(str(item))  # Convert non-string items to string
        msg_str = " ".join(formatted_elements)

    else:
        # Fortran fixed format
        try:
            writer = get_cached_writer(fmt)
            msg_str = writer.write(data)
        except InvalidFormat:
            # Re-throw the exception with a clearer message
            raise InvalidFormat(f"Invalid format for writing: '{fmt}'")
        except Exception:
            # If fortranformat can't handle it, fall back to a string representation
            msg_str = str(data)

    # Write to file if a file pointer is provided
    if fp:
        try:
            print(msg_str, file=fp)  # We use print with file=fp to ensure the line break
        except Exception as e:
            raise RuntimeError(f"{type(e).__name__}: {e}") from e

    # Write to console if 'con' is True (this check will correctly reflect the 'fp is None' assignment
    # or if 'con' was explicitly passed as True)
    if con:
        print(msg_str)

    return msg_str
