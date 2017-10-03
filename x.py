#!/usr/bin/env python3

import glob
import os
import subprocess

def exec(path):
    out_path = path.replace('.lox', '.out')
    with open(out_path) as f:
        expected = f.read()

    output = subprocess.check_output(['target/release/lox', path]).decode('utf-8')
    assert output == expected

subprocess.run(['cargo', 'build', '--release'])

for path in glob.glob('t/*.lox'):
    print(f"Checking {path}...")
    exec(path)
