# Haskell Path Tracer

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
