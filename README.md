# xts-socket

xts-socket is a Haskell library that provides a lightweight cross-platform API to BSD-style sockets. This is a fork of the [`socket`](https://hackage.haskell.org/package/socket) package.

**WARNING: This is a work in progress. Things will not work yet. Do not use this now.**

Motivation: `socket` should be a cool lightweight alternative to the [`network`](https://hackage.haskell.org/package/network) package. However, it seems Lars Petersen, the original author and maintainer, has left. The project's last commit was in June 2020. As of writing, that was more than five years ago. Pull requests and issues are left unanswered. Unfortunate, but it happens. With sadness, XT created this fork in November 2025.

My current goal is to configure GitHub Actions to run the tests in these environments:

- GNU/Linux
- Windows
- macOS (aarch64-darwin)

Then I will address some needs:

- More test cases (especially testing connection to known, external (public) addresses)
- CI to test Fourmolu for formatting, HLint for linting, etc.
- Safe way to feed a "number" (`Word16`) as a "port" (There is currently no public API for this, except going through `Int`)

## Principles

We will try to follow the original implementation principle:

- Every operation and every flag exposed should be supported with same semantics on every platform. If this cannot be guaranteed it should be supplied by another (extension) package.
- Absolutely no conditional exports.
- No `#ifdef` madness in the Haskell sources. The Haskell binding code uses the FFI to reference platform dependent C functions for each operation. If a platform is not POSIX compliant (i.e. Windows) equivalent functionality is implemented by using whatever the platform specific building blocks are.

In addition to the above:

- Keep minimal dependencies. (Currently just two: `base` and `bytestring`) The goal is to make a package that serve as a building block for other packages.
- Keep `{-# LANGUAGE CPP #-}` (The C pre-processor language extension) as little as possible, and restrict them to minimal modules; Development tools (Fourmolu, HLint, and even HLS) are known to have difficulties working with them.

## Testing

The project uses [tasty](http://documentup.com/feuerbach/tasty) for testing.

There are two basic test suites coming from the original `socket` package: `default` and `threaded` which share the same code. The only difference is that one is compiled against GHC's single threaded RTS and the other against the multi-threaded one. Run `stack test` to execute both in sequence.

An additional suite, `external`, exercises real public services (currently GitHub HTTP and Google Public DNS). It is not run by default; It checks the environment variable `XTS_SOCKET_EXTERNAL=1` (or `true`/`yes`). Optional IPv6 probes also require `XTS_SOCKET_EXTERNAL_IPV6=1`.

---

Below are some excerpts we inherited from the original `README.md`:

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
