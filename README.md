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
