# diagrams-spiro-splines

A simple Haskell library that enables the use of
[Spiro splines](https://github.com/fontforge/libspiro/)
in the [`diagrams`](https://hackage.haskell.org/package/diagrams) ecosystem.

This library uses FFI bindings to
[`libspiro`](https://github.com/fontforge/libspiro) and exposes an interface
to convert Spiro control points into Bezier curves that can be further processed
using diagrams.

---

## Prerequisites

To build and use this library, you need `libspiro` version **1.5.0**
installed on your system.
The library is dynamically linked, so `libspiro`
must be available at build and runtime.

### Linux (Debian/Ubuntu)

```
sudo apt install libspiro-dev
```

### macOS (Homebrew)

```
brew install libspiro
```

### Windows (MSYS2)

First, install [MSYS2](https://www.msys2.org) and open the
**"MSYS2 UCRT64"** shell. Then run:

```
pacman -Syu
pacman -S mingw-w64-ucrt-x86_64-libspiro
```

Ensure you build the project from the UCRT64 environment
so that the headers and libraries are found correctly.

---

## Installation

If using Cabal:

```
cabal update
cabal build
```

Make sure GHC can locate the `libspiro` headers and libraries.
Paths are configured in `package.yaml` to search common locations automatically.

---

## License

This library is distributed under the **MIT license**.

It dynamically links against
[`libspiro`](https://github.com/fontforge/libspiro),
which is licensed under the [GNU General Public License, version 3]
(https://www.gnu.org/licenses/gpl-3.0.html).
You are responsible for complying with the terms of the GPL
when distributing software that uses `libspiro`.

---

## Attribution

This library depends on [`libspiro`](https://github.com/fontforge/libspiro),
developed and maintained by the FontForge project.
