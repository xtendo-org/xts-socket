# xts-socket

xts-socket is a Haskell library that provides API to the POSIX BSD-style sockets. It is a fork of the [`socket`](https://hackage.haskell.org/package/socket) package.

`socket` should be a good lightweight alternative to the [`network`](https://hackage.haskell.org/package/network). However, the maintainer has left. The project's last commit was in June 2020. As of writing, that was more than five years ago. Pull requests and issues are left unanswered.

The current goal is to configure GitHub Actions to run the tests in these environments:

- GNU/Linux
- Windows
- macOS (aarch64-darwin)

The primary development platform is x64 GNU/Linux. Once we get things working, we will configure GitHub Actions to automate the test on other platforms like Windows and macOS (aarch64-darwin; Apple Silicon).

## Principles

We will try to follow the original implementation principle:

- Every operation and every flag exposed should be supported with same semantics on every platform. If this cannot be guaranteed it should be supplied by another (extension) package.
- Absolutely no conditional exports.
- No `#ifdef` madness in the Haskell sources. The Haskell binding code uses the FFI to reference platform dependent C functions for each operation. If a platform is not POSIX compliant (i.e. Windows) equivalent functionality is implemented by using whatever the platform specific building blocks are.

In general, we will respect the original. One important difference is that we use Stack and Hpack for package organization and management.

Other principles:

- `base` and `bytestring` are the only acceptable dependencies. The goal is to make a package that serve as a building block for other packages. Adding more dependency is not an option.

Below is some excerpts from the original `README.md`:

# socket

## Motivation

This library aims to expose a minimal and cross-platform interface for
BSD style networking code.

## Implementation Philosophy

- Every operation and every flag exposed should be supported with same semantics on every platform. If this cannot be guaranteed it should be supplied by another (extension) package.
- Absolutely no conditional exports.
- No `#ifdef` madness in the Haskell sources. The Haskell binding code uses the FFI to reference platform dependant C functions for each operation. If a platform is not POSIX compliant (i.e. Windows) equivalent functionality is implemented by using whatever the platform specific building blocks are.

## Platform Support

### Windows

GHC's runtime system on Windows does not offer an event notification mechanism for sockets. The original [network](https://hackage.haskell.org/package/network) library suffers from this, too. For example, connection attempts are non-interruptible etc. The approach taken to circumvent this in this library is to poll the non-blocking sockets with increasing delay. This guarantees interruptibility and fairness between different threads. It allows for decent throughput while also keeping CPU consumption on a moderate level if a socket has not seen events for a longer period of time (maximum of 1 second delay after 20 polling iterations). The only drawback is potentially reduced response time of your application. The good part: Heavy load (e.g. connection requests or incoming traffic) will reduce this problem. Eventually your accepting thread won't wait at all if there are several connection requests queued.

This workaround may be removed if someone is willing to sacrifice to improve the IO manager on Windows.

### Dependencies

- base
- bytestring

### Tests

The project uses [tasty](http://documentup.com/feuerbach/tasty) for testing.

There are two test suites: `default` and `threaded` which share the same code. The only difference is that one is compiled against GHC's single threaded RTS and the other against the multi-threaded one. Run `cabal test` or `stack test` to execute both in sequence.
