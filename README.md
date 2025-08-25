# diagrams-spiro-splines

A simple Haskell library that enables the use of [Spiro splines][libspiro]
in the [`diagrams`][diagrams] ecosystem.

This library uses FFI bindings to [`libspiro`][libspiro] and exposes
an interface to convert Spiro control points into Bezier curves
that can be further processed using diagrams.

---

## Prerequisites

To build and use this library, you need `libspiro` version **1.5.0**
installed on your system.
The library is dynamically linked, so `libspiro`
must be available at build and runtime.

Make sure GHC can locate the `libspiro` headers and libraries
before attempting to install this library via cabal (or stack).
Paths are configured in `package.yaml` to search common locations automatically.

### Linux (Debian/Ubuntu)

```bash
sudo apt install libspiro-dev
```

### macOS (Homebrew)

```bash
brew install libspiro
```

### Windows (MSYS2)

First, install [MSYS2][MSYS2] and open the **"MSYS2 UCRT64"** shell. Then run:

```bash
pacman -Syu
pacman -S mingw-w64-ucrt-x86_64-libspiro
```

Ensure you build the project from the UCRT64 environment
so that the headers and libraries are found correctly.

---

## License

This library is distributed under the **MIT license**.

It dynamically links against [`libspiro`][libspiro],
which is licensed under the [GNU General Public License, version 3][gpl].
You are responsible for complying with the terms of the GPL
when distributing software that uses `libspiro`.

---

## Attribution

This library depends on [`libspiro`][libspiro],
developed and maintained by the FontForge project.

[MSYS2]: https://www.msys2.org
[diagrams]: https://hackage.haskell.org/package/diagrams
[gpl]: https://www.gnu.org/licenses/gpl-3.0.html
[libspiro]: https://github.com/fontforge/libspiro/
