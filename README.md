# Haskell Path Tracer

**This readme file is partially outdated.**

## Building

This application requires GHC 8.8.3 and LLVM 9.0.1 to build. An older version of
GHC can be installed through
[ghcup](https://gitlab.haskell.org/haskell/ghcup-hs), and support for LLVM 10 is
tracked in [this issue](https://github.com/llvm-hs/llvm-hs/issues/293). To
compile, simply run:

```shell
cabal build
```

### Running without a GPU

TODO:

The application uses Accelerate's PTX LLVM backend by default. To run the
application without an NVIDIA GPU simply pass the `cpu` flag to the application:

```shell
stack build --flag tracer:cpu --exec tracer-exe
```

Otherwise simply use `stack run` to compile and run the application.

## Native dependencies

Because of the reliance on LLVM, OpenGL and SDL there are a few non-Haskell
dependencies. These can be installed as follows:

### Debian, Ubuntu and derivatives

Debian and Ubuntu ship older versions of LLVM than those supported by this
project so it might be necessary to install LLVM's from their own
[repositories](https://apt.llvm.org/).

```shell
sudo apt update
sudo apt install libsdl2-dev libsdl2-ttf-dev llvm-7-dev
```

### Arch, Manjaro and derivatives

```shell
sudo pacman -S sdl2 sdl2_ttf llvm-libs
```

## Library documentation

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
