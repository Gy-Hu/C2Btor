# C to BTOR2 Conversion

My note of using [Kratos-2.2](https://kratos.fbk.eu/download.html) to convert C code to BTOR2 format.

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
export PYTHONPATH=$PYTHONPATH:$PWD/kratos-2.2-linux64/tools/
```

Compile the Python extension module:

```bash
cd tools/kratos/
python _pykratos_setup.py
```

## 3. C Code to BTOR2 Workflow

### Step 1: Convert C Code to Kratos2 IR

Use the `c2kratos.py` tool to convert C code to Kratos2's intermediate representation:

```bash
python tools/c2kratos.py test/input.c --bitvectors --svcomp -o test/output.k2
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

Both `c2kratos.py` and `kratos` tools provide extensive configuration options that can be tailored to specific verification needs.

#### Important c2kratos.py Options

```bash
# Using bit-vectors for precise modeling
python tools/c2kratos.py [input.c] --bitvectors=32 --no-floats-as-reals -o [output.k2]

# SV-COMP mode for software verification competition benchmarks
python tools/c2kratos.py [input.c] --svcomp --svcomp-spec [spec-file] -o [output.k2]

# Handling C-specific features
python tools/c2kratos.py [input.c] --pointers --initialize-globals -o [output.k2]

# Control flow customization
python tools/c2kratos.py [input.c] --entry-point [custom_main] --use-condjump -o [output.k2]
```

Key c2kratos.py parameter categories:
- **Data Representation**: `--bitvectors`, `--floats-as-reals`, `--custom-int-types`
- **Preprocessing**: `-D`, `-U`, `-I`, `--cpp`
- **Verification Mode**: `--svcomp`, `--sldv-mode`
- **Control Flow**: `--entry-point`, `--use-condjump`, `--unroll-static-loops`
- **Memory Model**: `--pointers`, `--blast-static-arrays`

#### Important kratos Command-Line Options

```bash
# Model checking with different engines
./bin/kratos [input.k2] -model_checking_engine=bmc -model_checking_bmc_bound=20
./bin/kratos [input.k2] -model_checking_engine=kind
./bin/kratos [input.k2] -model_checking_engine=simplic3

# Transition system encoding options
./bin/kratos [input.k2] -trans_output_format=btor -trans_enum_mode=bv -trans_encoding=large

# Function inlining control
./bin/kratos [input.k2] -inline_calls_threshold=5 -inline_max_depth=3

# Counterexample generation
./bin/kratos [input.k2] -cex_type=path -cex_output_file=[cex.out]
```

Key kratos parameter categories:
- **Model Checking Engines**: `-model_checking_engine` (bmc, kind, simplic3, btoric3, etc.)
- **Transition System Encoding**: `-trans_output_format`, `-trans_enum_mode`, `-trans_encoding`
- **Function Inlining**: `-force_inline`, `-inline_calls_threshold`, `-inline_max_depth`
- **Counterexample Generation**: `-cex_type`, `-cex_output_file`
- **Loop Handling**: `-unroll_loops`, `-remove_loops`

#### Workflow Examples

Complete verification workflow with bounded model checking:

```bash
# Convert C to Kratos IR
python tools/c2kratos.py source.c --bitvectors --svcomp -o source.k2

# Run bounded model checking with bound of 10
./bin/kratos source.k2 -model_checking_engine=bmc -model_checking_bmc_bound=10 -cex_type=path -cex_output_file=counterexample.out
```

Conversion to BTOR2 with optimizations:

```bash
# Convert with optimizations
python tools/c2kratos.py source.c --bitvectors --detect-output-params -o source.k2

# Generate optimized BTOR2 with large block encoding
./bin/kratos source.k2 -force_inline -propagate_constants=true -remove_const_maps -trans_encoding=large -trans_output_format=btor -output_file=optimized.btor2
```

## 4. Troubleshooting Common Issues

- **Not generated safety property**: Using ` __VERIFIER_error()` in C to define the property

## 5. Advanced Usage

For complex C programs, additional preprocessing steps might be needed:

```bash
# Use the GCC preprocessor to handle include files and macros
gcc -E -P input.c > input_preprocessed.c

# Then convert to Kratos2 IR
python tools/c2kratos.py input_preprocessed.c -o output.k2
```

## 6. WIP

Using [btor2verilog](https://github.com/makaimann/btor2verilog) can conduct C2RTL verification
