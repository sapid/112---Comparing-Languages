#!/usr/bin/python
import os

for file in os.listdir('tests/'):
    run_file = "tests/" + file
    save_file = file[:6] + ".output"
    cmd = "gprolog <" + run_file + " 2>&1 | tee " + save_file
    os.system(cmd)
