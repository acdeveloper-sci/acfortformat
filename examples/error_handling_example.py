"""Demonstrates how to handle the EndOfFile exception."""

import sys
import os
from io import StringIO


from acfortformat import read, EndOfFile

print("--- EndOfFile Handling Example ---")

# Simulate a file-like object with some lines and then EOF
test_content = "Line 1 of data\nLine 2 of data\n"
fp_simulated = StringIO(test_content)

try:
    print("\nReading first line...")
    line1 = read(fp_simulated, fmt="(A)")
    print(f"Read: '{line1}'")
    assert line1 == "Line 1 of data"

    print("\nReading second line...")
    line2 = read(fp_simulated, fmt="(A)")
    print(f"Read: '{line2}'")
    assert line2 == "Line 2 of data"

    print("\nAttempting to read beyond EndOfFile...")
    # The next read should trigger EndOfFile
    eof_result = read(fp_simulated, fmt="(A)")

    if eof_result is EndOfFile:
        print("Successfully detected EndOfFile object!")
    else:
        print(f"Expected EndOfFile, but got: {eof_result}")
        sys.exit(1)  # Exit with error

    print("\n--- Example using a context manager for EndOfFile ---")
    fp_simulated_context = StringIO("Single line.\n")
    try:
        read_data = read(fp_simulated_context, fmt="(A)")
        print(f"First read: '{read_data}'")
        read_data = read(fp_simulated_context, fmt="(A)")  # This will raise EndOfFile
        print(f"Second read (should not reach here): '{read_data}'")
    except EndOfFile:
        print("Caught EndOfFile exception as expected for exhausted stream!")
    except Exception as e:
        print(f"Unexpected error: {e}")
        sys.exit(1)

except Exception as e:
    print(f"An unexpected error occurred during the example: {e}")
    sys.exit(1)

print("\nEndOfFile example finished.")
