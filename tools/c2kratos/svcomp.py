#
# Minimal support for SV-COMP specifications and builtins
#

import re
from kratos import *
from . import rewrite


class Spec:
    def add(self, program):
        raise NotImplementedError

# end of class SVCompSpec


class ErrorSpec(Spec):
    def __init__(self, error_func):
        self.error_func = error_func

    def add(self, program):
        body = LabelExpr('ERROR')
        body.annotations = [('error', self.error_func)]
        try:
            f = program.get_function(self.error_func)
            f.body = body
        except KeyError:
            pass
        return program

# end of class ErrorSpec


def get_spec(specfile):
    error_re = re.compile(r'^\s*CHECK\(\s*init\(([a-zA-Z0-9_]+)\s*\(\)\s*\),\s*LTL\(G\s*[!]\s*call\s*\(([a-zA-Z0-9_]+)\s*\(\s*\)\s*\)\s*\)\s*\)\s*$')
    with open(specfile) as f:
        data = f.read()
    m = error_re.match(data)
    if m:
        return m.group(1), ErrorSpec(m.group(2))
    raise NotImplementedError("unsupported spec")


def handle_options(options):
    if options.svcomp_spec or options.svcomp:
        options.builtin_abort = 'abort'
        options.builtin_exit = 'exit'
        options.builtin_malloc = 'malloc'
        options.builtin_memset = 'memset'

        options.builtin_assume = '__VERIFIER_assume'
        options.nondet_prefix = '__VERIFIER_nondet'

        options.abstract_ext_funs = True
        options.no_unreachable = True
        options.precise_math_h = True
        options.use_condjump = True
        options.use_iquote_includes = True
        if options.bitvectors is None:
            options.bitvectors = 32

    if options.svcomp_spec:
        entry, spec = get_spec(options.svcomp_spec)
        options.entry_point = entry
        options.svcomp_spec = spec
    elif options.svcomp and not options.builtin_error:
        options.builtin_error = '__VERIFIER_error'
