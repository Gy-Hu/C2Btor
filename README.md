# Kratos-2.2: C to BTOR2 Conversion Tutorial

This tutorial provides step-by-step instructions on how to convert C code to BTOR2 format using the Kratos-2.2 verification toolchain.

## 1. Project Overview

Kratos2 is a program verification tool that can process C programs and perform model checking. The project includes these main components:

- `bin/kratos` - The main Kratos2 executable
- `tools/c2kratos.py` - C to Kratos2 IR frontend
- `tools/kratos/` - Kratos2 Python API
- `include/kratos.h` - C API interface
- `lib/libkratos.so` - Kratos2 library file

## 2. Environment Preparation

First, ensure the `pycparser` library is installed in your Python environment:

```bash
python -m pip install pycparser
```

Set up your Python path to include the Kratos2 tools:

```bash
export PYTHONPATH=$PYTHONPATH:/home/huguangyu/kratos-2.2-linux64/tools/
```

Compile the Python extension module:

```bash
cd /home/huguangyu/kratos-2.2-linux64/tools/kratos/
python _pykratos_setup.py
```

## 3. C Code to BTOR2 Workflow

### Step 1: Convert C Code to Kratos2 IR

Use the `c2kratos.py` tool to convert C code to Kratos2's intermediate representation:

```bash
cd /home/huguangyu/kratos-2.2-linux64
python tools/c2kratos.py test/input.c --bitvectors -o test/output.k2
```

Parameters:
- `input.c` - Input C source file
- `-o output.k2` - Output Kratos2 IR file

### Step 2: Encode and Verify Using Kratos

Kratos2 can encode the intermediate representation into a symbolic transition system, which can be done using either the Python API or the command-line tool:

Using the command-line tool:

```bash
./bin/kratos test/output.k2 -trans_output_format=btor -trans_enum_mode=bv -output_file=test/output.btor2
```

### Step 3: Advanced Configuration Options

Kratos2 supports various configuration options to control the encoding and verification process:

- Memory model: `--mem-model=flat` or `--mem-model=array`
- Bit width: `--bv-width=32` specifies the default width for bit vectors
- Array representation: `--array-repr=aiger` or `--array-repr=uf`

For example:

```bash
./bin/kratos --encode=btor2 --mem-model=flat --bv-width=32 output.k2 -o output.btor2
```

## 4. Python Script Example

Here's a complete Python script example demonstrating how to use the Kratos2 API to convert C code to BTOR2:

```python
#!/usr/bin/env python3

import sys
import os
from kratos import *

def convert_c_to_btor2(c_file, btor2_file):
    # Step 1: Convert C to Kratos2 IR
    k2_file = c_file + ".k2"
    os.system(f"python {os.path.dirname(sys.argv[0])}/../c2kratos.py {c_file} -o {k2_file}")
    
    # Step 2: Load Kratos2 IR
    program = Program.parse_file(k2_file)
    
    # Step 3: Create configuration and set BTOR2 output
    config = Config()
    config.set_option("encode.format", "btor2")
    
    # Step 4: Encode to BTOR2
    encoded = encode(config, program)
    
    # Step 5: Save to file
    with open(btor2_file, "w") as f:
        f.write(encoded)
    
    print(f"Conversion complete: {c_file} -> {btor2_file}")

if __name__ == "__main__":
    if len(sys.argv) < 3:
        print(f"Usage: {sys.argv[0]} <input.c> <output.btor2>")
        sys.exit(1)
    
    convert_c_to_btor2(sys.argv[1], sys.argv[2])
```

Save this script as `c2btor2.py`, then execute:

```bash
cd /home/huguangyu/kratos-2.2-linux64
python tools/c2btor2.py input.c output.btor2
```

## 5. Troubleshooting Common Issues

- **Python module not found error**: Ensure that `PYTHONPATH` includes the Kratos2 tools directory
- **Extension module compilation failure**: Check that Python development packages and necessary compilation tools are installed on your system
- **Parsing errors**: Ensure the C code complies with the C99 standard supported by pycparser, and remove any incompatible extensions
- **Encoding failures**: Check if the Kratos2 IR file format is correct and whether the C language features used are supported
- **Not generated safety property**: `python tools/c2kratos.py test/input.c --bitvectors --svcomp -o test/output.k2` and using ` __VERIFIER_error()` in C to define the property

## 6. Advanced Usage

For complex C programs, additional preprocessing steps might be needed:

```bash
# Use the GCC preprocessor to handle include files and macros
gcc -E -P input.c > input_preprocessed.c

# Then convert to Kratos2 IR
python tools/c2kratos.py input_preprocessed.c -o output.k2
```

## 7. Example Workflow

Here's a complete example workflow:

1. Create a simple C program:

```c
// example.c
#include <stdio.h>

int main() {
    int x = 5;
    int y = 0;
    
    if (x > 0) {
        y = 10;
    } else {
        y = -10;
    }
    
    assert(y > 0);
    return 0;
}
```

2. Convert to Kratos2 IR:

```bash
python tools/c2kratos.py example.c -o example.k2
```

3. Convert to BTOR2:

```bash
./bin/kratos --encode=btor2 example.k2 -o example.btor2
```

4. Use a BTOR2 model checker (like Boolector) to verify the model:

```bash
boolector example.btor2 --model-gen
```

With this detailed tutorial, you should be able to use the Kratos-2.2 toolchain to convert C code to BTOR2 format for subsequent model checking or other formal verification.
