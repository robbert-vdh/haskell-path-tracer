# Haskell Path Tracer

## Running without a GPU

The application uses Accelerate's PTX LLVM backend by default. To run the
application without an NVIDIA GPU simply pass the `cpu` flag to the application:

``` shell
stack build --flag tracer:cpu --exec tracer-exe
```

Otherwise simply use `stack run` to compile and run the application.

## Library Documentation

The application depends on an unreleased version of Accelerate. Therefore it
might be useful to generate the documentation locally instead of using the
documentation available on Hackage:

```shell
stack haddock --open
stack hoogle '<query>'
stack hoogle -- --server
```
