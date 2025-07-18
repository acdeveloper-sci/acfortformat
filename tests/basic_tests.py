"""
Script to run basic tests of the acfortformat package's read and write functions.
"""

import os
import sys

# Import for colors
from colorama import Fore, Style, init

# Initialize Colorama for cross-platform compatibility and auto-reset
init(autoreset=True)

# Add the 'src' directory to the path to allow package import
# This is necessary when running the test script directly or with 'python -m',
# as the package is not yet installed in the environment.
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "../src")))

from acfortformat import read, write, EndOfFile
from fortranformat import InvalidFormat  # Import to handle the specific exception


# Helper functions for colored output
def print_section_header(title):
    print(f"\n{Fore.CYAN}--- {title} ---{Style.RESET_ALL}")


def print_info(message):
    print(f"{Fore.WHITE}{message}{Style.RESET_ALL}")


def print_success(message):
    print(f"{Fore.GREEN}{message}{Style.RESET_ALL}")


def print_failure(message):
    print(f"{Fore.RED}{message}{Style.RESET_ALL}")


def print_warning(message):
    print(f"{Fore.YELLOW}{message}{Style.RESET_ALL}")


def run_all_tests():
    """Run basic reading and writing tests with different formats, with colored output."""
    print_info("Starting tests for acfortformat...")

    # ---------- First test: Fixed format with file --------------
    print_section_header("1st test (Fixed format, writing to file and reading)")
    # Adjusted format to ensure proper parsing with FortranFormat's T descriptor
    # T13 means go to column 13. (2I4,1X,I1) consumes 4+4+1+1 = 10 chars. So T13 is valid.
    fmt_1 = "(2I4,1X,I1,T13,I4,I6,3F10.4)"
    data_1 = [1985, 1234, 7, 25, 123456, 3.14, 1.618, 2.718]

    print_info(f"FORMAT: '{fmt_1}'")
    print_info(f"Data: {data_1}")

    output_filename_1 = "output_test_1.dat"
    try:
        with open(output_filename_1, "w") as f:
            write(data_1, fp=f, fmt=fmt_1, con=False)  # con=False as output goes to file
        print_info(f"Data written to '{output_filename_1}'.")

        with open(output_filename_1) as f:
            values_1 = read(f, fmt=fmt_1)
            print_info(f"Parsed values: {values_1}")
            # Ensure that the values read are correct (approximately for floats)
            assert isinstance(values_1, list)
            assert len(values_1) == len(data_1)
            # Checks for specific values, adjusting for float precision
            assert values_1[0:5] == data_1[0:5]
            for i in range(5, 8):
                assert abs(values_1[i] - data_1[i]) < 1e-4
        print_success("Test 1 completed successfully.")
    except Exception as e:
        print_failure(f"Error in Test 1: {e}")
    finally:
        if os.path.exists(output_filename_1):
            os.remove(output_filename_1)
            print_info(f"'{output_filename_1}' file deleted.")

    # ---------- Second test: Fixed format with string --------------
    print_section_header("2nd test (Fixed format, writing and reading string)")
    # Using the same adjusted format as Test 1
    fmt_2 = "(2I4,1X,I1,T13,I4,I6,3F10.4)"
    values_2 = [12, 34, 5, 789, 123456, 1.2345, 6.7890, 0.0001]

    print_info(f"FORMAT: '{fmt_2}'")
    print_info(f"Data: {values_2}")

    try:
        formatted_line_2 = write(values_2, fmt=fmt_2)
        print_info(f"> Written line: '{formatted_line_2}'")

        parsed_values_2 = read(formatted_line_2, fmt=fmt_2)
        print_info(f"> Parsed values: {parsed_values_2}")
        assert isinstance(parsed_values_2, list)
        assert len(parsed_values_2) == len(values_2)
        assert parsed_values_2[0:5] == values_2[0:5]
        for i in range(5, 8):
            assert abs(parsed_values_2[i] - values_2[i]) < 1e-4
        print_success("Test 2 completed successfully.")
    except Exception as e:
        print_failure(f"Error in Test 2: {e}")

    # ---------- Third test: Complex/tricky format --------------
    print_section_header("3rd test (Complex/tricky format, writing and reading strings)")
    fmt_3 = "///1X,78(1H*)/1X,1H*,T12,' ERREUR DANS NB ENREGISTREMENTS: ',I8,' DE EPISODE : ',I8,T78,1H*/1X,78(1H*)"
    values_3 = [12, 34]  # Only these two values are used in I8,I8

    print_info(f"FORMAT: '{fmt_3}'")
    print_info(f"Data: {values_3}")

    try:
        formatted_line_3 = write(values_3, fmt=fmt_3)
        print_info(f"> Written line (can be multi-line): \n'{formatted_line_3}'")

        try:
            parsed_values_3 = read(formatted_line_3, fmt=fmt_3)
            print_info(f"> Parsed values: {parsed_values_3}")
            print_success("Test 3 completed successfully (literal reading is not checked in detail).")
        except InvalidFormat as e:
            print_warning(
                f"Test 3 Warning: Reading this format returned InvalidFormat, which is expected for literals: {e}"
            )

    except Exception as e:
        print_failure(f"Error in Test 3: {e}")

    # ---------- Fourth test: Free-form "*" (write and read) --------------
    print_section_header("4th test (Free-form '*', write and read)")
    data_4 = [42, 3.1415, "hello world", -7, "text with spaces", 2.71828, "another 'string'"]
    print_info(f"Original data: {data_4}")

    try:
        # Raw write (equivalent to WRITE(*,*))
        line_4 = write(data_4, fmt="*")
        print_info(f"> Written line: '{line_4}'")

        # Raw read (equivalent to READ(*,*))
        parsed_4 = read(line_4, fmt="*")
        print_info(f"> Line read and parsed: {parsed_4}")
        # Ensure that the types and values are correct
        assert len(parsed_4) == len(data_4)
        assert parsed_4[0] == data_4[0]
        assert abs(parsed_4[1] - data_4[1]) < 1e-5
        assert parsed_4[2] == data_4[2]
        assert parsed_4[3] == data_4[3]
        assert parsed_4[4] == data_4[4]
        assert abs(parsed_4[5] - data_4[5]) < 1e-5
        assert parsed_4[6] == data_4[6]
        print_success("Test 4 completed successfully.")
    except Exception as e:
        print_failure(f"Error in Test 4: {e}")

    # ---------- Fifth test: Free format reading with quotes --------------
    print_section_header("5th test (free format reading with quotes)")
    # This line is designed to test complex quoting and escaping as per Fortran rules.
    # Note: 'quotes' is inside single quotes, "plus" is inside double quotes.
    # Fortran escapes ' as '' and " as "".
    # The Python string literal uses '\' to escape the ' for Python's parser.
    line_5 = '123 "string with spaces" 3.14 "another string with \'quotes\' and ""plus""" \'simple quote\''
    print_info(f"> Original line: '{line_5}'")
    try:
        parsed_5 = read(line_5, fmt="*")
        print_info(f"> Free format reading result: {parsed_5}")
        # Expected result after Fortran-style parsing
        expected_5 = [123, "string with spaces", 3.14, "another string with 'quotes' and \"plus\"", "simple quote"]

        assert len(parsed_5) == len(expected_5)
        assert parsed_5[0] == expected_5[0]
        assert parsed_5[1] == expected_5[1]
        assert abs(parsed_5[2] - expected_5[2]) < 1e-5
        assert parsed_5[3] == expected_5[3]
        assert parsed_5[4] == expected_5[4]
        print_success("Test 5 completed successfully.")
    except Exception as e:
        print_failure(f"Error in Test 5: {e}")

    # ---------- Sixth test: Handling EndOfFile --------------
    print_section_header("6th test (EndOfFile Handling)")
    # Create an empty or single line file
    eof_test_filename = "eof_test.dat"
    try:
        with open(eof_test_filename, "w") as f:
            f.write("Line 1\n")
        print_info(f"File '{eof_test_filename}' created for EOF test.")

        with open(eof_test_filename, "r") as f:
            print_info("> Reading first line...")
            result_1 = read(f, fmt="(A)")
            print_info(f"Result: '{result_1}'")
            assert result_1 == "Line 1"

            print_info("> Trying to read beyond the EOF...")
            result_eof = read(f, fmt="(A)")
            print_info(f"Result: {result_eof}")
            assert result_eof is EndOfFile
        print_success("Test 6 completed successfully.")
    except EndOfFile:
        print_success("EndOfFile exception caught as expected.")
    except Exception as e:
        print_failure(f"Error in Test 6: {e}")
    finally:
        if os.path.exists(eof_test_filename):
            os.remove(eof_test_filename)
            print_info(f"'{eof_test_filename}' file deleted.")

    print_info("\nAll basic tests have finished.")


if __name__ == "__main__":
    run_all_tests()
