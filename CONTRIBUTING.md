# Contributing to clickhouse-cl

Thank you for your interest in contributing! This project is an open-source ClickHouse client for Common Lisp, and we welcome contributions from the community.

## Getting Started

### Prerequisites

- [SBCL](http://www.sbcl.org/) (Steel Bank Common Lisp)
- A running ClickHouse server for integration tests (Docker works well)

### Setup

1. Fork and clone the repository:

```bash
git clone https://github.com/<YOUR_USERNAME>/clickhouse-cl.git
cd clickhouse-cl
```

2. Verify the library loads:

```bash
make load
```

3. Start a local ClickHouse server for testing:

```bash
docker run -d --name clickhouse -p 8123:8123 clickhouse/clickhouse-server
```

4. Run the test suite:

```bash
make all-tests
```

## Project Structure

The library is a single file (`ch.lisp`) with no external dependencies. Tests live in `ch-test.lisp`. Examples are in the `examples/` directory. There is no build step — the library is loaded directly via `(load "ch.lisp")`.

## Making Changes

1. Create a branch from `main` for your work.
2. Make your changes to `ch.lisp` and/or `ch-test.lisp`.
3. Add or update tests for any new functionality.
4. Run the full test suite before submitting:

```bash
make unit-tests          # No server required
make integration-tests   # Requires ClickHouse
make all-tests           # Everything
```

5. If you add a new example, add a corresponding Makefile target and include it in the CI matrix in `.github/workflows/checks.yml`.

## Pull Requests

- Keep PRs focused on a single change.
- Provide a clear description of what the change does and why.
- Ensure CI passes — all tests and examples are run automatically on push.

## Reporting Bugs

Please open a [GitHub issue](https://github.com/juliojimenez/clickhouse-cl/issues) with steps to reproduce, expected behavior, and your environment (Lisp implementation, OS, ClickHouse version).

## Security Vulnerabilities

**Do not report security vulnerabilities through public issues.** Please see [SECURITY.md](SECURITY.md) for instructions on responsible disclosure.
