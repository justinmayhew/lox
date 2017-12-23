#!/usr/bin/env python3
import sys
from craftinginterpreters.util import test

NAME = 'rlox'
TESTS = test.INTERPRETERS['chap12_classes'].tests.items()

test.INTERPRETERS[NAME] = test.Interpreter(
    name=NAME,
    language='java',
    args=['target/release/lox'],
    tests={f'craftinginterpreters/{path}': state for path, state in TESTS},
)

if len(sys.argv) > 2:
    print('Usage: test.py [filter]', file=sys.stderr)
    sys.exit(1)

if len(sys.argv) == 2:
    test.filter_path = sys.argv[1]

if not test.run_suite(NAME):
    sys.exit(1)
