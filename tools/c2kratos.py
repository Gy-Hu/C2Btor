"""
Translates a C file to Kratos2 IR.

Requires pycparser: https://github.com/eliben/pycparser
"""

import sys

import c2kratos.main

if __name__ == '__main__':
    sys.setrecursionlimit(20000)
    c2kratos.main.main(c2kratos.main.getopts())
