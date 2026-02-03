# Lean 4 VM Backend

Fork of Lean 4 with an experimental bytecode compiler targeting a Rust VM.

## Build

```bash
make -j -C build/release
```

The compiler is in `./build/release/stage1/bin/lean`. Put this directory in your
`PATH` so that the `lake` and `lean` binaries are available.

## Usage

To compile a single file to bytecode:

```bash
./build/release/stage1/bin/lean -Y output.leanbc input.lean
```

## Running Bytecode

The bytecode runs on [lean4-vm](https://github.com/sdiehl/lean4-vm):

```bash
lean4-vm output.leanbc
```
