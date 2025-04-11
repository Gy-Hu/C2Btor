=============================
 README.txt file for Kratos2
=============================

Contents of the distribution:
  README.txt            This file.
  LICENSE.txt           The Kratos2 License.
  CREDITS.txt           Credits for code used in Kratos2.
  bin/kratos            The Kratos2 executable.
  include/kratos.h      The Kratos2 C API.
  lib/libkratos.so      The Kratos2 library.
  tools/c2kratos.py     The C to K2 front-end.
  tools/kratos/         The Kratos2 Python API. 
  tools/*.py            Other utilities and examples of usage of the Python API.


Using the Python API
--------------------

In order to access the full functionality of the Python API, the _pykratos
extension module should be compiled, by running the setuptools script
_pykratos_setup.py in the tools/kratos/ directory. The script takes no
argument, and upon success will produce the _pykratos extension module in the
same directory (tools/kratos). 

The API is not meant to be installed, but used directly from the tarball. If
you want to access it from elsewhere, the easiest option is to add the tools/
directory of the tarball to the Python module search path (e.g. by modifying
the PYTHONPATH environment variable).

Finally, the API depends on the "pycparser" module, which can be installed
e.g. via pip (python -m pip install pycparser).
