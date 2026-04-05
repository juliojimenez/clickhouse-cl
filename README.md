[![Status Checks](https://github.com/juliojimenez/clickhouse-cl/actions/workflows/checks.yml/badge.svg)](https://github.com/juliojimenez/clickhouse-cl/actions/workflows/checks.yml)

<img src="images/logo.png"  width="64" height="64">

# clickhouse-cl

ClickHouse Common Lisp Client

## Loading clickhouse-cl

```lisp
cl-user> (load "ch.lisp")
 
    ██  ██  ██  ██  λ
    ██  ██  ██  ██
    ██  ██  ██  ██
    ██  ██  ██  ██  ██
    ██  ██  ██  ██  ██
    ██  ██  ██  ██
    ██  ██  ██  ██
    ██  ██  ██  ██
 
ClickHouse Common Lisp Client loaded successfully!
Version: 0.49.0
Usage: (ch:make-database :host "localhost")
 
T
cl-user >
```

### CLI

```bash
$ sbcl --load ch.lisp
```

### make Load Shortcuts

```bash
$ make load
```

## Examples

Load the clickhouse-cl library:

```lisp
(cl:load "ch.lisp")
```

Run any example:

```lisp
(load "examples/basic-connection.lisp")
```

Modify the connection parameters in examples to match your setup:

```lisp
(defparameter *host* "localhost")
(defparameter *port* 8123)
(defparameter *username* "default")
(defparameter *password* nil)
```

Some examples use sample tables. Create them with:

```sql
-- Run this in your ClickHouse instance
CREATE DATABASE IF NOT EXISTS examples;

CREATE TABLE examples.users (
    id UInt32,
    name String,
    email String,
    age UInt8,
    created_at DateTime
) ENGINE = MergeTree()
ORDER BY id;

CREATE TABLE examples.events (
    timestamp DateTime,
    user_id UInt32,
    event_type String,
    properties Map(String, String)
) ENGINE = MergeTree()
ORDER BY (timestamp, user_id);
```

Each example is self-contained and includes:

- Connection setup
- Sample data (where applicable)
- Demonstration code
- Expected output
- Cleanup code

Most examples can be run directly by loading them into your Lisp environment.

## Tests

```lisp
cl-user > (load "ch.lisp")
cl-user > (load "ch-test.lisp")
cl-user > (ch-tests:example-usage)
 
ClickHouse-CL Test Suite Usage:
===============================
 
(load "ch.lisp")                    ; Load the main library
(load "ch-tests.lisp")              ; Load test suite
(ch-tests:run-all-tests)            ; Run all tests
(ch-tests:run-unit-tests)           ; Run unit tests only
(ch-tests:run-integration-tests)    ; Run integration tests
(ch-tests:run-performance-tests)    ; Run performance tests
(ch-tests:print-test-summary)       ; Show detailed results
 
Test Configuration:
  *test-host*: "localhost"
  *test-port*: 8123
  *test-username*: "default"
 
To use different test server:
  (setf ch-tests:*test-host* "your-server")
  (setf ch-tests:*test-port* 8443)
  (setf ch-tests:*test-username* "testuser")
 
NIL
cl-user >
```

### make Test Shortcuts

```bash
$ make unit-tests
$ make integration-tests
$ make performance-tests
$ make all-tests
```
