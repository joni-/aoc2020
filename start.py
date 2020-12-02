#!/usr/bin/env python3
import os
import sys

PLACEHOLDER = '####'

SOLUTION_FILE_TEMPLATE = f"""
module {PLACEHOLDER} where

solveA :: String -> String
solveA s = s

solveB :: String -> String
solveB s = s
""".strip()

TESTS_FILE_TEMPLATE = f"""
module {PLACEHOLDER}Test (tests) where

import {PLACEHOLDER}
import Test.Tasty
import Test.Tasty.HUnit (testCase, (@?=))

tests =
  testGroup
    "{PLACEHOLDER}"
    [ testCase "solveA" $ solveA "hello" @?= "hello",
      testCase "solveB" $ solveB "hello" @?= "hello"
    ]
""".strip()

base_dir = os.path.dirname(__file__)
SOLUTION_DIR = os.path.join(base_dir, "src")
INPUTS_DIR = os.path.join(base_dir, "inputs")
TESTS_DIR = os.path.join(base_dir, "test")
MAIN_FILE = os.path.join(base_dir, "app", "Main.hs")
MAIN_TEST_FILE = os.path.join(TESTS_DIR, "Spec.hs")

def create_file(file, template):
    filename = os.path.basename(file)

    print(f"Create {file}")
    with open(file, "w") as f:
        f.write(template)


def create_solution_file(puzzle_number):
    puzzle_name = f"Day{puzzle_number}"
    filename = f"{puzzle_name}.hs"
    puzzle_file = os.path.join(SOLUTION_DIR, filename)
    create_file(
        file=puzzle_file,
        template=SOLUTION_FILE_TEMPLATE.replace(PLACEHOLDER, puzzle_name)
    )

    SOLVER_IMPORT_PLACEHOLDER = "-- start.py import placeholder\n"
    SOLVER_PLACEHOLDER = "-- start.py placeholder\n"

    with open(MAIN_FILE, 'r+') as f:
      lines = f.read()
      f.seek(0)
      f.write(lines
        .replace(SOLVER_IMPORT_PLACEHOLDER, f"import qualified {puzzle_name}\n{SOLVER_IMPORT_PLACEHOLDER}")
        .replace(SOLVER_PLACEHOLDER, f', (("{puzzle_number}", "A"), {puzzle_name}.solveA)\n    , (("{puzzle_number}", "B"), {puzzle_name}.solveB)\n    {SOLVER_PLACEHOLDER}'))


def create_test_file(puzzle_number):
    puzzle_name = f"Day{puzzle_number}"
    filename = f"{puzzle_name}Test.hs"
    test_file = os.path.join(TESTS_DIR, filename)
    create_file(
        file=test_file,
        template=TESTS_FILE_TEMPLATE.replace(PLACEHOLDER, puzzle_name)
    )

    TEST_IMPORT_PLACEHOLDER = "-- start.py import placeholder\n"
    TEST_PLACEHOLDER = "-- start.py test placeholder\n"

    with open(MAIN_TEST_FILE, 'r+') as f:
      lines = f.read()
      f.seek(0)
      f.write(lines
        .replace(TEST_IMPORT_PLACEHOLDER, f"import qualified {puzzle_name}Test\n{TEST_IMPORT_PLACEHOLDER}")
        .replace(TEST_PLACEHOLDER, f", {puzzle_name}Test.tests\n    {TEST_PLACEHOLDER}"))

def create_input_file(puzzle_number):
    filename = os.path.join(INPUTS_DIR, f"Day{puzzle_number}.input")
    print(f"Create {filename}")
    with open(filename, "a") as f:
        f.write("")


if __name__ == "__main__":
    if len(sys.argv) != 2:
        print(f"Usage: {sys.argv[0]} <puzzle#>")
        exit

    puzzle_number = sys.argv[1]
    create_solution_file(puzzle_number)
    create_test_file(puzzle_number)
    create_input_file(puzzle_number)