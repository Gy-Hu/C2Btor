#!/usr/bin/env python 

try:
    from setuptools import setup, Extension
except ImportError:
    from distutils.core import setup, Extension
import os
import sys

sys.argv = [sys.argv[0]] + ['build_ext', '--inplace']

basedir = os.path.dirname(__file__)

include_dirs = [os.path.join(basedir, '..', '..', 'include')]
library_dirs = [os.path.join(basedir, '..', '..', 'lib')]
libraries = ['kratos']

setup(name='pykratos', version='0.1',
      description='Kratos Python helper',
      ext_modules=[Extension('_pykratos',
                             [os.path.abspath(
                                 os.path.join(basedir, '_pykratos.cpp'))],
                             include_dirs=include_dirs,
                             library_dirs=library_dirs,
                             libraries=libraries
                             )]
      )
