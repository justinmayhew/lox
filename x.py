#!/usr/bin/env python3

import glob
import os
import subprocess
import sys

def err(msg):
    print(f"\033[1;31m{msg}\033[0;0m", file=sys.stderr)

def exec(path):
    out_path = path.replace('.lox', '.out')
    with open(out_path) as f:
        expected = f.read()

    output = subprocess.check_output(['target/release/lox', path]).decode('utf-8')

    if output != expected:
        print(f"Output: '{output}'")
        print(f"Expected: '{expected}'")
        raise AssertionError

subprocess.run(['cargo', 'build', '--release'])

for path in glob.glob('t/*.lox'):
    print(f"Checking {path}...")
    try:
        exec(path)
    except FileNotFoundError:
        err(f"Skipping {path} because it doesn't have a .out file")
