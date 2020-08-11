# Haskell Path Tracer

A simple path tracing implementation written in Haskell using
[accelerate](https://github.com/AccelerateHS/accelerate).

This has recently been rewritten to make better use of streams so it can be
parallelized better and so we can support features that require splitting rays
such as refraction. At the moment the new implementation is still significantly
slower than the old one.

## Building

This application requires LLVM 9.0.1 to build. Support for LLVM 10 is tracked in
[this issue](https://github.com/llvm-hs/llvm-hs/issues/293). To compile and run,
simply run:

```shell
cabal run -j tracer
```

### Running without a GPU

The application uses Accelerate's PTX LLVM backend by default. To run the
application without an NVIDIA GPU simply pass the `cpu` flag to the application:

```shell
cabal run -j -fcpu tracer
```

## Native dependencies

Because of the reliance on LLVM, OpenGL and SDL there are a few non-Haskell
dependencies. These can be installed as follows:

### Debian, Ubuntu and derivatives

Debian and Ubuntu ship older versions of LLVM than those supported by this
project so it might be necessary to install LLVM's from their own
[repositories](https://apt.llvm.org/).

```shell
sudo apt update
sudo apt install libsdl2-dev libsdl2-ttf-dev llvm-9-dev
```

### Arch, Manjaro and derivatives

```shell
sudo pacman -S sdl2 sdl2_ttf llvm9
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
