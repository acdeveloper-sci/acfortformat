# ACFortFormat

Fortran-style reading and writing utility in Python.
Support for fixed formats, free reading, type inference, and formatted output.<br>

A powerful and flexible Python library for handling data input/output (I/O) operations, inspired by Fortran's formatting syntax and also supporting native Python-style I/O operations.

`acfortformat` is ideal for engineers, scientists, and developers working with data generated or consumed by Fortran applications, or for those who simply need precise control over data formatting in their Python applications.

## Features

* **Fortran-Style Reading and Writing:** Full support for the Fortran formatting language (FORMAT), allowing you to specify field width, data type (integer, real, string), and position control (T, X, etc.).
* **Fixed and Free Formatting:** Handles both fixed formats (predefined columns) and free formats (space- or comma-separated, with handling of quoted strings).
* **Type Inferring:** Automatically detects the data type (integer, float, string) when reading in free format.
* **Easy Integration:** Designed to be easy to integrate into any Python project.
* **Format Cache:** Performance optimization by reusing pre-compiled format descriptors.
* **Versatile Output:** Writes data to the console or any file-like object.

---

## Installation

You can automatically install ACFortFormat by running:

```bash
pip install acfortformat
```
> Requires an internet connection, `pip` available, and Python ≥ 3.7.

---
## Fortran-like I/O Utility – Key Features

### READING

`read(fp, fmt=None)` emulates the Fortran `READ` statement:

- `fmt=None` → behavior like `READ(*,*)` (free reading):
- Supports integers, floats (including `D` notation), and strings.
- Strings enclosed in double or single quotes are treated as a single value.
- Automatic type inference: `int`, `float`, `str`.

- `fmt="*"` → explicit equivalent of `READ(*,*)`.

- `fmt="(A)"` → reads the entire line as a single string.

- `fmt="(Fortran format)"` → Fixed-format reading (via `fortranformat`):
- Supports `I`, `F`, `A`, `X`, `Tn`, repetitions, etc.
- Supports fixed alignments, spacing, and columns.
- Uses *reader* caching for greater efficiency.

---

### WRITING

`write(data, fp=None, fmt=None, con=False)` emulates the Fortran `WRITE` instruction:

- `fmt=None` or `fmt="*"` → behavior like `WRITE(*,*)`:
- Elements of `data` (list or tuple) are printed separated by spaces.
- If `data` is a string or a simple number, it is printed directly.

- `fmt="(Fortran format)"` → fixed-format writing:
- Supports numeric precision (`F10.4`, `I4`, etc.), alignment, and embedded text.
- Supports complex formats.
- Uses *writers* cache for optimization.

---

\### ADDITIONAL UTILITIES

- `infer_type(s)` → converts string to `int`, `float`, or leaves it as `str`.
- `line_sanity(line)` → expands tabs to spaces for fixed reads.
- `EndOfFile` → custom end-of-file exception.
- Transparent file (`fp`) and console support (`con=True` in `write()`).

---

## Included Test Coverage

- Writing and reading with complex fixed formats.
- Reading/writing to files.
- Free format support (`*`), including text with spaces.
- Read-write symmetry validation.

---

## Usage

### Reading and Writing with Fortran Format

```python
from acfortformat import read, write, EndOfFile

# Define a Fortran format
fortran_fmt = "(I5,F10.3,A15)"
data_to_write = [12345, 67.8901, "Hello World"]

# Write to the console with Fortran format
print("--- Writing with Fortran format ---")
written_line = write(data_to_write, fmt=fortran_fmt)
print(f"Line written: '{written_line}'")

# Simulate reading from a string (which could be a file line)
print("\n--- Reading with Fortran format ---")
read_values = read(written_line, fmt=fortran_fmt)
print(f"Values read: {read_values}")

# Example of reading from a file
# Assuming 'data.txt' contains a line like: " 123 45.678 This is text"
# with the format "(I5, F7.3, A12)"

# with open("data.txt", "r") as f:
#     values_from_file = read(f, fmt="(I5, F7.3, A12)")
#     print(f"Values read from file: {values_from_file}")
```

### Free Format Reading and Writing (Python-like)

```python
from acfortformat import read, write, EndOfFile

# Free Format Data to Write
free_data = [10, 3.14, "a string with spaces", -50, "another 'string'"]

# Write to the console in free format
print("\n--- Free-Format Writing (*) ---")
free_written_line = write(free_data, fmt="*")
print(f"Line written: '{free_written_line}'")

# Reading from a free-form string
print("\n--- Free-Format Reading (*) ---")
free_read_values = read(free_written_line, fmt="*")
print(f"Read Values: {free_read_values}")

# Reading a quoted string for strings with spaces
quoted_line = '123 "string with spaces" 3.14 "another string with \'quotes\' and ""plus""" \'simple quote\''
print(f"\n--- Reading a quoted string in free-form ---")
parsed_quoted = read(quoted_line, fmt="*")
print(f"Parsed Values: {parsed_quoted}")

# Handling end of file
from io import StringIO
# Simulate a file with one line and then EOF
test_fp = StringIO("First line\n")
print("\n--- End of file handling ---")
print(f"Read: {read(test_fp, fmt='(A)')}")
# The second read will return the EndOfFile object
eof_result = read(test_fp, fmt='(A)')
if eof_result is EndOfFile:
    print("End of file reached!")
```

---

## More Examples

For a broader range of practical use cases and detailed demonstrations of `acfortformat`'s features, explore the `examples/` directory in the project's GitHub repository. You'll find standalone scripts illustrating various scenarios like:

* **Simple Read/Write:** Basic Fortran-style and free-format I/O.
* **File I/O:** Reading and writing data directly to/from files.
* **Error Handling:** Demonstrating how to gracefully manage conditions like EndOfFile.

These examples are designed to be easily runnable and serve as a quick start for integrating `acfortformat` into your projects.

---

## Contributions

Contributions are welcome! If you find a bug or have an improvement, please open an issue or submit a pull request in the GitHub repository.

## License

This project is licensed under the MIT License. See the [LICENSE](https://github.com/acdeveloper-sci/acfortformat/blob/main/LICENSE) file for more details.

## Contact

If you have any questions or suggestions, please feel free to contact me at [ajcs.developer@gmail.com](mailto:ajcs.developer@gmail.com).

## Autor

**Adolph Cardozo**  
📧 [ajcs.developer@gmail.com](mailto:ajcs.developer@gmail.com)
🔗 [GitHub](https://github.com/acdeveloper-sci)
