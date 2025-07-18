"""Demonstrates basic fixed and free format read/write operations."""

import sys
import os

# This is to allow running the example directly from the 'examples/' folder.

from acfortformat import read, write

print("--- Simple Fortran-style Read and Write Example ---")

# Define a Fortran format
fortran_fmt = "(I5,F10.3,A15)"
data_to_write = [12345, 67.8901, "Hello World!"]

print("\n--- Fixed Format ---")
print(f"Original Data: {data_to_write}")
print(f"Format String: '{fortran_fmt}'")

# Write to a string
written_line = write(data_to_write, fmt=fortran_fmt)
print(f"Written Line: '{written_line}'")

# Read from the string
read_values = read(written_line, fmt=fortran_fmt)
print(f"Read Values: {read_values}")

# Simple verification (optional, for self-check of the example)
if (
    read_values[0] == data_to_write[0]
    and abs(read_values[1] - data_to_write[1]) < 1e-4
    and read_values[2].strip() == data_to_write[2]
):
    print("Fixed format read/write verification: SUCCESS")
else:
    print("Fixed format read/write verification: FAILED")


print("\n--- Free Format (*) ---")
free_data = [10, 3.14, "a string with spaces", -50, "another 'string with \"quotes\"'"]
print(f"Original Data: {free_data}")

# Write to a string in free format
free_written_line = write(free_data, fmt="*")
print(f"Written Line: '{free_written_line}'")

# Read from a free-form string
free_read_values = read(free_written_line, fmt="*")
print(f"Read Values: {free_read_values}")

# Simple verification (optional)
if free_read_values == free_data:
    print("Free format read/write verification: SUCCESS")
else:
    print("Free format read/write verification: FAILED")

print("\nExample finished.")
