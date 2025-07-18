"""Demonstrates reading and writing data to/from files."""

import sys
import os

from acfortformat import read, write

print("--- File I/O Example with acfortformat ---")

test_filename = "sample_data.dat"
file_fmt = "(I5,F10.2,A20)"
data_for_file = [9876, 123.45, "Example Entry One"]

# --- Writing to a file ---
print(f"\nWriting data to '{test_filename}' with format '{file_fmt}'")
print(f"Data to write: {data_for_file}")
try:
    with open(test_filename, "w") as f:
        write(data_for_file, fp=f, fmt=file_fmt, con=False)  # con=False is for file output
    print("Write successful.")
except Exception as e:
    print(f"Error writing to file: {e}")
    sys.exit(1)


# --- Reading from a file ---
print(f"\nReading data from '{test_filename}'")
try:
    with open(test_filename, "r") as f:
        read_from_file = read(f, fmt=file_fmt)
    print(f"Data read: {read_from_file}")

    # Simple verification (optional)
    if (
        read_from_file[0] == data_for_file[0]
        and abs(read_from_file[1] - data_for_file[1]) < 1e-4
        and read_from_file[2].strip() == data_for_file[2]
    ):
        print("File read/write verification: SUCCESS")
    else:
        print("File read/write verification: FAILED")

except Exception as e:
    print(f"Error reading from file: {e}")
    sys.exit(1)
finally:
    # Clean up the test file
    if os.path.exists(test_filename):
        os.remove(test_filename)
        print(f"Cleaned up '{test_filename}'.")

print("\nFile I/O example finished.")
