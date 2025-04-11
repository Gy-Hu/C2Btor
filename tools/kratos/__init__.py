"""
Python API for Kratos.
"""

import os, sys, ctypes
_b = os.path.normpath(os.path.abspath(os.path.join(os.path.dirname(__file__))))
try:
    for _d in (_b, os.path.abspath(os.path.join(_b, '..', '..', 'lib'))):
        for _n in (('kratos.dll', 'libkratos.dll') if sys.platform == 'win32'
                   else ('libkratos.so',)):
            _l = os.path.join(_d, _n)
            if os.path.exists(_l):
                try:
                    ctypes.cdll.LoadLibrary(_l)
                    raise StopIteration
                except OSError:
                    pass
except StopIteration:
    pass

from kratos.expr import *
from kratos.exprparse import ExprParser, ProgramParser, ExecutionPathParser
from kratos import progrewrite
from kratos.progrewrite import VarProvider, get_kratos_executable
from kratos.verification import *

def parse_program(src):
    """
    Parse a `Program` from the given file-like object.
    Parameters:
    - src: a file-like object
    """
    data = src.read() if hasattr(src, 'read') else src
    parser = ProgramParser()
    return parser.parse(data)

def parse_expr(src):
    """
    Parse an `Expr` from the given file-like object.
    Parameters:
    - src: a file-like object
    """
    data = src.read() if hasattr(src, 'read') else src
    parser = ExprParser()
    return parser.parse(data)

try:
    import kratos._pykratos
    import kratos.expr

    def parse_program(src):
        data = src.read() if hasattr(src, 'read') else src
        return kratos._pykratos.parse_program(kratos.expr, data)

    def parse_expr(src):
        data = src.read() if hasattr(src, 'read') else src
        return kratos._pykratos.parse_expr(kratos.expr, data)
    
except ImportError:
    pass

def parse_execution_path(src):
    """
    Parse an `ExecutionPath` from the given file-like object.
    Parameters:
    - src: a file-like object
    """
    data = src.read() if hasattr(src, 'read') else src
    parser = ExecutionPathParser()
    return parser.parse(data)
