#
# Options for the C to Kratos translator
#

import argparse
import os, sys, imp
from . import rewrite, svcomp
from kratos.progrewrite import get_kratos_executable


class Options(object):
    def __init__(self):
        self.cpp = 'cpp'
        self.nondet_prefix = None
        self.builtin_assert = None
        self.builtin_assert_force_cond = False
        self.builtin_assume = None
        self.builtin_havoc = None
        self.builtin_exit = None
        self.builtin_memset = None
        self.builtin_error = None
        self.builtin_floor = None
        self.builtin_ceil = None
        self.builtin_isfinite = None
        self.builtin_isinf = None
        self.builtin_isnan = None
        self.builtin_isnormal = None
        self.builtin_issubnormal = None
        self.builtin_iszero = None
        self.builtin_float_as_bits = None
        self.builtin_bits_as_float = None
        self.builtin_malloc = 'malloc'
        self.init_function = None
        self.bitvectors = None
        self.floats_as_reals = False
        self.entry_point = 'main'
        self.print_ast = False
        self.sldv_mode = False
        self.sldv_input_struct = None
        self.sldv_input_var = []
        self.sldv_initialize = None
        self.sldv_step = None
        self.svcomp = False
        self.svcomp_spec = None
        self.runtime = None
        self.no_unreachable = False
        self.detect_output_params = True
        self.debug = False
        self.verbose = False
        self.blast_static_arrays = None
        self.blast_static_arrays_allow_var_idx = False
        self.output_file = None
        self.cpp_defs = []
        self.cpp_undefs = []
        self.cpp_includes = []
        self.no_pointers = False
        self.rewrites = None
        self.initialize_globals = True
        self.unroll_static_loops = False
        self.rwopts = []
        self.filenames = []
        self.c_stubs_dir = None
        self.no_default_rewrites = False
        self.reuse_temporaries = True
        self.use_condjump = False
        self.annotate_branches = False
        self.precise_math_h = False
        self.annotate_labels = False
        self.custom_int_types = False
        self.abstract_ext_funs = False
        self.use_iquote_includes = False
        self.funs_blacklist = set()
        self.single_error = False

    def parse(self, args=None):
        p = argparse.ArgumentParser()
        def add_flag(name, **kwds):
            dest = name.replace('-', '_')
            p.add_argument('--' + name, dest=dest, action='store_true', **kwds)
            p.add_argument('--no-' + name, dest=dest, action='store_false')
        p.add_argument('filenames', nargs='*', help='input files')
        p.add_argument('-D', dest='cpp_defs', metavar='DEF', action='append',
                       help='pass "-D DEF" to the C preproccessor')
        p.add_argument('-U', dest='cpp_undefs', metavar='DEF', action='append',
                       help='pass "-U DEF" to the C preprocessor')
        p.add_argument('-I', dest='cpp_includes', metavar='DIR',
                       action='append',
                       help='pass "-I DIR" to the C preprocessor')
        p.add_argument('--cpp', help='custom cpp command')
        p.add_argument('--nondet-prefix',
                       help='prefix for non-deterministic variables')
        p.add_argument('--builtin-assert', 
                       help='consider the given function as a built-in assert')
        add_flag('builtin-assert-force-cond', 
                 default=False,
                 help='use cond as an assumption when encoding the '
                 'built-in assert')
        p.add_argument('--builtin-assume', 
                       help='consider the given function as a built-in assume')
        p.add_argument('--builtin-havoc', 
                       help='consider the given function as a built-in havoc')
        p.add_argument('--builtin-exit', 
                       help='consider the given function as a built-in exit')
        p.add_argument('--builtin-memset', 
                       help='consider the given function as a built-in memset')
        p.add_argument('--builtin-error',
                       help='consider the given function as a built-in '
                       'error function')
        p.add_argument('--builtin-floor',
                       help='consider the given function as the built-in '
                       'floor op')
        p.add_argument('--builtin-ceil',
                       help='consider the given function as the built-in '
                       'ceil op')
        p.add_argument('--builtin-isfinite',
                       help='consider the given function as the built-in '
                       'isfinite op')
        p.add_argument('--builtin-isinf',
                       help='consider the given function as the built-in '
                       'isinf op')
        p.add_argument('--builtin-isnan',
                       help='consider the given function as the built-in '
                       'isnan op')
        p.add_argument('--builtin-isnormal',
                       help='consider the given function as the built-in '
                       'isnormal op')
        p.add_argument('--builtin-issubnormal',
                       help='consider the given function as the built-in '
                       'issubnormal op')
        p.add_argument('--builtin-iszero',
                       help='consider the given function as the built-in '
                       'iszero op')
        p.add_argument('--builtin-float-as-bits',
                       help='consider the given function as the built-in '
                       'float to bits bitcast')
        p.add_argument('--builtin-bits-as-float',
                       help='consider the given function as the built-in '
                       'bits to float bitcast')
        p.add_argument('--builtin-malloc', help='consider the given function '
                       'as the built-in memory allocator')
        p.add_argument('--init-function',
                       help='translate the given function as the init '
                       'constraint of the kratos program.')
        p.add_argument('--bitvectors', choices=[16, 32, 64], const=32, type=int,
                       help='use bit-vectors', nargs='?')
        p.add_argument('--no-bitvectors', dest='bitvectors',
                       action='store_const', const=0)
        add_flag('floats-as-reals', 
                 help='translate floats as reals even when --bitvectors is set')
        p.add_argument('--entry-point', default='main',
                       help='custom entry point function (default: main)')
        p.add_argument('--print-ast', action='store_true',
                       help='enable the print of the ast of the parsed file')
        add_flag('sldv-mode', 
                 help='Enable SLDV mode. See also --sldv-input, '
                 '--sldv-initialize and --sldv-step')
        p.add_argument('--sldv-input-struct',
                       help='name of struct that stores SLDV inputs. '
                       'If not given, a struct starting with "ExtU_" '
                       'will be used.')
        p.add_argument('--sldv-input-var',
                       help='add the given var to the list of SLDV inputs. '
                       'Supersedes --sldv-input-struct', action='append')
        p.add_argument('--sldv-initialize',
                       help='name of the SLDV init function. If not given, '
                       'a function ending in "_initialize" will be used.')
        p.add_argument('--sldv-step',
                       help='name of the SLDV init function. If not given, '
                       'a function ending in "_step" will be used.')
        add_flag('svcomp', help='SV-COMP compatibility mode')
        p.add_argument('--svcomp-spec', help='specify a SV-COMP spec file'
                       ' to check. Implies --svcomp')
        p.add_argument('--runtime', help='specify a custom runtime to use')
        p.add_argument('--no-unreachable', action='store_true', default=False,
                       help='do not translate unreachable code')
        add_flag('detect-output-params', 
                 help='detect output parameters when translating '
                 'functions')
        add_flag('debug', help='enable debugging output')
        add_flag('verbose', help='increase verbosity')
        p.add_argument('--blast-static-arrays', type=int,
                       help='blast unaliased static arrays up '
                       'to the given size')
        p.add_argument('--blast-static-arrays-allow-var-idx',
                       action='store_true',
                       help='allow variable indices when blasting '
                       'static arrays')
        p.add_argument('-o', '--output', dest='output_file',
                       help='output file (stdout if not given)')
        p.add_argument('--pointers', dest='no_pointers',
                       action='store_false', help='use pointers')
        p.add_argument('--no-pointers', dest='no_pointers',
                       action='store_true', help='disable pointers '
                       '(treat them as scalar variables)')
        add_flag('initialize-globals',
                 help='initialize global vars to their default value')
        add_flag('unroll-static-loops', help='unroll static for loops')
        p.add_argument('--rewrites', help='specify a custom list of rewrites')
        p.add_argument('-r', '--rwopt',
                       help='custom option for the custom list of rewrites',
                       action='append', dest='rwopts')
        p.add_argument('--no-default-rewrites', action='store_true',
                       help='disable default rewrites')
        add_flag('reuse-temporaries', 
                 help='reuse temporary local vars when possible '
                 'in the generated code')
        add_flag('use-condjump', 
                 help='use CondJumpExpr instead of non-deterministic '
                 'jumps whenever possible')
        add_flag('annotate-branches', 
                 help='annotate branch labels with a :branch tag')
        add_flag('precise-math-h', 
                 help='use a more precise implementation of some '
                 'functions in <math.h>')
        add_flag('annotate-labels', help='annotate labels with information about'
                 ' their origin/purpose')
        add_flag('custom-int-types', help='enable custom integer types of '
                 'arbitrary bit width')
        add_flag('abstract-ext-funs', help='abstract external functions as '
                 'returning a nondeterministic value')
        add_flag('use-iquote-includes', help='include parent directories of '
                 'the input files as -iquote instead of -I') 
        p.add_argument('--funs-blacklist',
                       help='comma-separated list of function names to blacklist'
                       ' (causing immediate failure at parsing time)',
                       dest='_funs_blacklist_str')
        p.add_argument('--funs-blacklist-file',
                       help='read the functions blacklist from the given file',
                       dest='_funs_blacklist_file')
        add_flag('single-error', help='ensure a single error condition')

        p.parse_args(args, namespace=self)

        d = os.path.abspath(os.path.join(os.path.dirname(__file__),
                                         '..', 'c_stdlib_stubs'))
        if os.path.exists(d):
            self.c_stubs_dir = d

        svcomp.handle_options(self)

        if self.runtime is not None:
            self.runtime = self._load(self.runtime, 'get_runtime')

        if self.rewrites is not None:
            self.rewrites = self._load(self.rewrites, 'get_rewrites')
        else:
            self.rewrites = rewrite.get_rewrites(self)

        if self.precise_math_h:
            self.cpp_defs.append('KRATOS_PRECISE_MATH_H')
            self.builtin_isinf = 'isinf'
            self.builtin_isnan = 'isnan'
            self.builtin_isfinite = 'isfinite'
            self.builtin_isnormal = 'isnormal'
            self.builtin_issubnormal = 'issubnormal'
            self.builtin_iszero = 'iszero'

        bl = getattr(self, '_funs_blacklist_str')
        if bl is not None:
            self.funs_blacklist = set(n.strip() for n in bl.split(','))
            del self._funs_blacklist_str
        else:
            f = getattr(self, '_funs_blacklist_file')
            if f is not None:
                with open(f) as src:
                    for line in src:
                        n = line.strip()
                        if n:
                            self.funs_blacklist.add(n)
                del self._funs_blacklist_file

    def _load(self, filename, meth):
        opts = self
        pth = os.path.abspath(os.path.dirname(filename))
        name = os.path.splitext(os.path.basename(filename))[0]
        info = imp.find_module(name, [pth])
        try:
            module = imp.load_module(*((name,)+info))
            sys.path = sys.path[1:]
            return getattr(module, meth)(opts)
        finally:
            if info[0]:
                info[0].close()

# end of class Options
