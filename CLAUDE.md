# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

A Common Lisp ClickHouse client library. Single-file (`ch.lisp`), zero external dependencies, multi-implementation (SBCL, CCL, ECL, CLISP, Allegro, LispWorks). No ASDF — loaded directly via `(load "ch.lisp")`. The package is `ch`.

## Commands

```bash
# Load library in SBCL REPL
make load

# Tests (all use SBCL)
make unit-tests          # 9 tests, no ClickHouse server needed
make integration-tests   # 9 tests, requires ClickHouse on localhost:8123
make performance-tests   # 2 tests, requires ClickHouse
make all-tests           # all suites

# Run a specific example
make basic-connection
make data-insertion
make formats
make error-handling
make clickhouse-cloud
make performance
make analytics
```

Integration/performance tests and examples require a ClickHouse server. CI uses `clickhouse/clickhouse-server` Docker image on port 8123 with default/no-password auth.

## Architecture

Everything lives in two files:

- **`ch.lisp`** — the entire library (~610 lines). Organized as:
  1. **Vendored deps** (lines 40-312): JSON parser (recursive descent), HTTP client (raw sockets per implementation), string utilities, format extraction
  2. **Conditions** (lines 314-333): `clickhouse-error` → `connection-error` / `query-error`
  3. **Database class** (lines 337-370): `database` with host/port/ssl/username/password/database/timeout slots; `make-database` constructor
  4. **HTTP interface** (lines 372-403): `build-auth-header`, `make-clickhouse-request` — all ClickHouse communication goes through here
  5. **Format processing** (lines 407-489): `*format-processors*` alist maps format names to processor functions; `extract-format-from-query` auto-detects FORMAT clause
  6. **Public API** (lines 490-540): `ping`, `query`, `execute`, `insert-file` — generic functions with methods on `database`
  7. **Utilities** (lines 542-562): `jget`, `with-connection` macro, `format-connection-string`

- **`ch-test.lisp`** — built-in test framework (~420 lines). Package `ch-tests`. Uses `deftest` macro with `assert-equal`/`assert-true`/`assert-error`. Tests are registered in `*unit-tests*`, `*integration-tests*`, `*performance-tests*` lists.

## Key Design Decisions

- HTTP interface only (port 8123), not native protocol (port 9000)
- All socket code is behind reader conditionals (`#+sbcl`, `#+ccl`, etc.) for portability
- Format auto-detection parses the SQL string for a `FORMAT` clause; if absent, returns raw string
- JSON is parsed into alists; access via `jget`/`json-get`
- `ssl` and `timeout` parameters are accepted but not yet implemented (declared-ignore)
- The `database` slot on the database class is not currently sent as a query parameter to ClickHouse
