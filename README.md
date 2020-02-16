# IU Haskell Elective Project

This will be a rhythm gaem lul.

## Install

### Native libraries

This project uses stack, but some libraries depend on C libraries, so a C
compiler would be nice, and also follow further platform-specific steps to
install the required libraries.

If you have a system that is not listed here, please somehow install `sdl2`,
`sdl2-image` and `sdl2-mixer` with development headers.

#### macOS

Brew can do the work for you:

```
$ brew install pkg-config
$ brew install sdl2 sdl2_image sdl2_mixer
```

### Haskell dependencies

These can be installed with stack (will also build the project for you):

```
$ stack build
```
