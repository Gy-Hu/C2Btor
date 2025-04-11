import subprocess
import os, sys
import itertools
from io import StringIO
from .exprparse import ProgramParser
from .expr import *


class VarProvider(object):
    """
    A provider of fresh variables and names.
    Attributes:
    - added: the list of added variables
    """
    def __init__(self, *scopes):
        """
        Constructor.
        Arguments:
        - scopes: sequence of lists of variables already in use
        """
        self.seen = set(v.var for v in itertools.chain(*scopes))
        self.idx = 1
        self.added = []
        self.auxvars = {}

    def mkvar(self, tp, name='$tmp'):
        """
        Generate a unique var of the given type.
        Parameters:
        - tp: the type of the var
        - name: (optional) custom prefix for the variable name
        """
        ret = VarExpr(tp, self.mkname(name))
        self.added.append(ret)
        return ret

    def auxvar(self, tp):
        """
        Generate an auxiliary var of the given type. If an aux var of
        the requested type was already created, it will be reused.
        Parameters:
        - tp: the type of the var
        """
        v = self.auxvars.get(tp)
        if v is None:
            v = self.mkvar(tp, '$aux')
            self.auxvars[tp] = v
        return v

    def mkname(self, name='$tmp'):
        """
        Generate a unique name with the given prefix.
        Parameters:
        - name: (optional) prefix for the name to generate
        """
        base = name
        while name in self.seen:
            name = '%s.%d' % (base, self.idx)
            self.idx += 1
        self.seen.add(name)
        return name

# end of class VarProvider


def get_kratos_executable():
    """
    Returns the full path to the `kratos` executable.
    Raises an `Exception` if the path cannot be determined.
    """
    exe = os.environ.get('KRATOS_EXECUTABLE', None)
    if exe is None:
        mydir = os.path.abspath(os.path.dirname(__file__))
        name = 'kratos'
        if sys.platform == 'win32':
            name += '.exe'
        possible_prefixes = ['.', '../..', '../../bin']
        for prefix in possible_prefixes:
            candidate_exe = os.path.join(mydir, prefix, name)
            if os.path.exists(candidate_exe):
                exe = candidate_exe
                break
    if not os.path.exists(exe):
        raise Exception("impossible to locate the kratos executable")
    return exe
