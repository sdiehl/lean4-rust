# Lean 4 ( Rust Backend )

[![CI](https://github.com/sdiehl/lean4-rust/actions/workflows/ci.yml/badge.svg)](https://github.com/sdiehl/lean4-rust/actions/workflows/ci.yml)

Fork of Lean 4.27.0 with an experimental bytecode compiler targeting a Rust VM.

## Building

```bash
make -j -C build/release
```

## Bytecode Compilation

Use the `-Y` flag to emit bytecode:

```bash
export LEAN_PATH="$(pwd)/build/release/stage1/lib/lean"
./build/release/stage1/bin/lean -Y output.leanbc input.lean
```

## Lake Integration

Set `backend := .vm` in your lakefile:

```lean
@[default_target]
lean_exe myapp where
  root := `Main
  backend := .vm
```

Then `lake build` produces `.leanbc` files in `.lake/build/ir/`.

## Running Bytecode

Use [lean4-vm](https://github.com/sdiehl/lean4-vm) to execute bytecode:

```bash
lean4-vm program.leanbc
lean4-vm -L ./bc/ program.leanbc  # Load dependencies from directory
```

## Environment Variables

| Variable    | Description                                       |
| ----------- | ------------------------------------------------- |
| `LEAN_PATH` | Search path for `.olean` files during compilation |

## License

Apache 2.0
