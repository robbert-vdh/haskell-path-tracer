# Haskell Path Tracer

## LLVM versions

LLVM version 8.0 recently got released, but it's not yet available in every
distro. That's why there are two `stack.yaml` files, one supporting LLVM 7.0.1
and one for LLVM 8.0. Simply _symlink_ the proper one for your system to
`stack.yaml` to get started:

```shell
# LLVM 7.0.1
ln -s stack-llvm-7.0.1.yaml stack.yaml

# LLVM 8.0
ln -s stack-llvm-8.0.yaml stack.yaml
```

## Running without a GPU

The application uses Accelerate's PTX LLVM backend by default. To run the
application without an NVIDIA GPU simply pass the `cpu` flag to the application:

```shell
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

## Resources

Haskell is hard. Computer graphics is hard. So here are some very useful
resources for implementing all of this.

- The `accelerate-examples` repository includes a [ray
  tracer](https://github.com/AccelerateHS/accelerate-examples/tree/master/examples/ray).
  This is very useful for getting a feel for expressing regular imperative
  graphics routines in terms of Accelerate programs.
- [Scratchpixel](https://www.scratchapixel.com/) is an amazing resource on all
  things computer graphics. They include detailed explanations of how things
  should work and why they are implemented the way they are.
- Jacco Bikker's course on [Advanced
  Graphics](http://www.cs.uu.nl/docs/vakken/magr/2018-2019/index.html#schedule)
  also contains a lot of easily digestible information on the subject.
  Especially the lectures on the use of SIMD and GPUs in ray tracing might prove
  useful when implementing a path tracing algorithm using Accelerate.

## FAQ

### Why are there no tests?

Good question!
