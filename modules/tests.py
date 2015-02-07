#!/usr/bin/env python
# quick & dirty test script for dynamic modules

import os, sys, glob, subprocess, argparse
from pprint import pprint as P

def fail():
    global OK
    OK = False
    if not ARGS.keep_going:
        exit(1)

ROOT_DIR = os.path.abspath(os.path.join(os.path.dirname(os.path.abspath(__file__)), ".."))
EMACS_BIN = os.path.join(ROOT_DIR, "src", "emacs")
MODULES_DIR = []
MAKE_EXTRA_FLAGS = []
OK = True

parser = argparse.ArgumentParser(description='Emacs dynamic-module tester')
parser.add_argument('-e', '--emacs-bin', help='altenative path to the emacs binary')
parser.add_argument('-B', '--rebuild', action='store_true', help='force make to build (-B flag)')
parser.add_argument('-k', '--keep-going', action='store_true', help='keep going after a failure')
parser.add_argument('module', nargs='*', help='explicit list of module to test (default: all modules)')
ARGS = parser.parse_args()

if ARGS.rebuild:
    MAKE_EXTRA_FLAGS.append('-B')
if ARGS.emacs_bin:
    EMACS_BIN = ARGS.emacs_bin

assert os.path.isfile(EMACS_BIN), "No emacs binary found at path %s" % EMACS_BIN

if ARGS.module:
    for mod in ARGS.module:
        path = os.path.join(ROOT_DIR, "modules", mod)
        if os.path.isdir(path):
            MODULES_DIR.append(path)
else:
    for path in glob.glob(os.path.join(ROOT_DIR, "modules", "*")):
        if os.path.isdir(path):
            MODULES_DIR.append(path)

assert len(MODULES_DIR) > 0, "No modules found"

for module_dir in MODULES_DIR:
    base, name = os.path.split(module_dir)
    print ""
    print "[*] testing module %s..." % name
    print "[*] building module..."

    make_call = ["make"] + MAKE_EXTRA_FLAGS
    ret = subprocess.call(make_call, cwd=module_dir)
    if ret != 0:
        print "[!] %s make failed" % name
        fail()

    test_script = os.path.join(module_dir, "test.el")
    if os.path.isfile(test_script):
        print "[*] running test.el..."
        emacs_call = [EMACS_BIN, "-Q", "-l", "ert", "-L", module_dir, "--batch",
                      "--load", test_script, "-f", "ert-run-tests-batch-and-exit"]
        ret = subprocess.call(emacs_call, cwd=module_dir)
        if ret != 0:
            print "[!] %s tests failed" % name
            fail()

if OK:
    print "\n[*] all ok"
else:
    print "\n[!] something failed"
