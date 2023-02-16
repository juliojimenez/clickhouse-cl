[![CI](https://github.com/juliojimenez/clickhouse-cl/actions/workflows/ci.yml/badge.svg)](https://github.com/juliojimenez/clickhouse-cl/actions/workflows/ci.yml) [![LINTER](https://github.com/juliojimenez/clickhouse-cl/actions/workflows/linter.yml/badge.svg)](https://github.com/juliojimenez/clickhouse-cl/actions/workflows/linter.yml) [![CRITIC](https://github.com/juliojimenez/clickhouse-cl/actions/workflows/critic.yml/badge.svg)](https://github.com/juliojimenez/clickhouse-cl/actions/workflows/critic.yml)

# clickhouse-cl

Common Lisp ClickHouse Client Library

- [Install](#install)
  - [Ultralisp.org](#ultralisporg)
  - [git clone](#git-clone)
  - [Releases](#releases)
- [No Line Breaks](#no-line-breaks-emacs)
- [`database` Class](#database-class)
  - [Slots](#slots)
  - [Usage](#usage)
  - [Methods](#methods)
    - [ping](#ping)
    - [replicas-status](#replicas-status)
    - [query](#query)
  - [Console Option](#console-option)
  - [Timeouts](#timeouts)
- [Formats](#formats)
  - [Functions](#functions)
    - [jget](#jget)
- [Examples](#examples)
  - [Connection to a local database](#connection-to-a-local-database)
  - [Query](#query)
- [Bugs, Features, and Vulnerabilities Reporting](#bugs-features-and-vulnerabilities-reporting)

## Install

### Ultralisp.org

clickhouse-cl is on [Ultralisp.org](https://ultralisp.org)!

```
CL-USER> (ql-dist:install-dist "http://dist.ultralisp.org/" :prompt nil)
...
CL-USER> (ql:quickload :clickhouse)
...
```

### git clone

Clone this repo wherever your quicklisp `local-projects` folder is configured.

```
~/quicklisp/local-projects/$ git clone https://github.com/juliojimenez/clickhouse-cl
~/quicklisp/local-projects/$ cd clickhouse-cl
~/quicklisp/local-projects/clickhouse-cl/$
```
Some dependencies are on [Ultralisp.org](https://ultralisp.org/), make sure you have it...

```lisp
CL-USER> (ql-dist:install-dist "http://dist.ultralisp.org/" :prompt nil)
...
```

In the emacs SLIME REPL or SBCL, load clickhouse-cl with...

```lisp
CL-USER> (ql:quickload :clickhouse)
To load "clickhouse":
  Load 1 ASDF system:
    clickhouse
; Loading "clickhouse"
[package clickhouse]

(:CLICKHOUSE)
CL-USER>
```

### Releases

You can also download a [release](https://github.com/juliojimenez/clickhouse-cl/releases), extract it into your `local-projects`, and follow the same steps above (minus the git clone, of course).

## No Line Breaks (Emacs)

To prevent line breaks, which makes query outputs with many fields difficult to read, issue the command `M-x toggle-truncate-lines RET` in the Emacs minibuffer.

If that doesn't work, checkout this [StackExchange](https://superuser.com/questions/592154/how-can-i-turn-off-emacss-auto-line-wrapping-for-the-current-session) post for other options.

## `database` Class

### Slots

| Name | Accessor | Default | Description |
| ---- | -------- | ------- | ----------- |
| host | y | localhost | Database hostname |
| port | y | 8123 | Database port, i.e. 8443 or 8123 |
| ssl | y | nil | SSL option, boolean, t or nil. |
| username | y | default | Database username |
| password | y | | Database password |

### Usage

Creating a instance of `database`.

```lisp
(make-instance 'clickhouse:database :host "clickhouse.example.com" :port "8123" :username "example" :password "1amAsecretPassWord")
```

The clickhouse-cl package nickname is `ch` and will be used throughout this README for brevity.

Binding an instance of `database`.

```lisp
(defparameter *db* (make-instance 'ch:database :host "localhost" :port "8123" :ssl nil :username "default" :password "1amAsecretPassWord"))
```

Reading and setting a slot.

```lisp
CL-USER> (ch::password *db*)
"1amAsecretPassWord"
CL-USER> (setf (ch::password *db*) "chang3m3plea5e")
"chang3m3plea5e"
CL-USER>
```
### Methods

#### ping

ch::ping *obj* :ping *bool* :console *bool*

```lisp
CL-USER> (ch::ping *db*)
"Ok."
```

The `:ping t` keyword parameter explicitly calls the instance `/ping` endpoint.

```lisp
CL-USER>  (ch::ping *db* :ping t)
"Ok."
```

#### replicas-status

ch::replicas-status *obj* :console *bool* 

```lisp
CL-USER> (ch::replicas-status *db*)
"Ok."
```

#### query

ch::query *obj* *query* :console *bool* :no-format *bool* :timeout *int*

```lisp
CL-USER> (ch::query *db* "SELECT 1")
"1"
```

### Console Option

All methods can take the keyword parameter `:console t`, providing a cleaner output when interacting directly with the library in the REPL.

```lisp
CL-USER> (ch:query *db* "SHOW DATABASES")
"INFORMATION_SCHEMA
default
information_schema
system"
```

```lisp
CL-USER> (ch:query *db* "SHOW DATABASES" :console t)
INFORMATION_SCHEMA
default
information_schema
letsgetitstarted
system
NIL
```

### Timeouts

The default *query* method timeout is 60 seconds. Use the `:timeout seconds` keyword parameter to change the default for long running operations.

```lisp
(ch:query *db* "INSERT INTO crypto_prices 
                    SELECT 
                        trade_date,
                        crypto_name,
                        volume,
                        price,
                        market_cap,
                        change_1_day
                    FROM s3('https://learn-clickhouse.s3.us-east-2.amazonaws.com/crypto_prices.csv',
                            'CSVWithNames'
                     )
                    SETTINGS input_format_try_infer_integers=0" :timeout 300)
```

## Formats

> ClickHouse can accept and return data in various formats. A format supported for input can be used to parse the data provided to INSERTs, to perform SELECTs from a file-backed table such as File, URL or HDFS, or to read a dictionary. A format supported for output can be used to arrange the results of a SELECT, and to perform INSERTs into a file-backed table. ([Formats](https://clickhouse.com/docs/en/interfaces/formats/))

clickhouse-cl supports automatic input and output format processing for the formats below. If such processing is not desired, the keyword parameter `:no-format t` is added to the [**query**](#query) method.

| Format | Input | Output | Result | Functions |
| ------ | ----- | ------ | ------ | --------- |
| [TabSeparated](https://clickhouse.com/docs/en/interfaces/formats/#tabseparated) | :heavy_check_mark: | :heavy_check_mark: | '('(string*)*) ||
| [TabSeparatedRaw](https://clickhouse.com/docs/en/interfaces/formats/#tabseparatedraw) | :heavy_check_mark: | :heavy_check_mark: | '('(string*)*) ||
| [TabSeparatedWithNames](https://clickhouse.com/docs/en/interfaces/formats/#tabseparatedwithnames) | :heavy_check_mark: | :heavy_check_mark: | '('(string*)*) ||
| [TabSeparatedWithNamesAndTypes](https://clickhouse.com/docs/en/interfaces/formats/#tabseparatedwithnamesandtypes) | :heavy_check_mark: | :heavy_check_mark: | '('(string*)*) ||
| CSV | :heavy_check_mark: | :heavy_check_mark: | '('(string*)*) ||
| CSVWithNames | :heavy_check_mark: | :heavy_check_mark: | '('(string*)*) ||
| CSVWithNamesAndTypes | :heavy_check_mark: | :heavy_check_mark: | '('(string*)*) ||
| JSON | :heavy_check_mark: | :heavy_check_mark: | BOOST-JSON:JSON-OBJECT | jget *obj* *key* |
| JSONAsString | :heavy_check_mark: ||||
| JSONStrings | :heavy_check_mark: | :heavy_check_mark: | BOOST-JSON:JSON-OBJECT | jget *obj* *key* |
| JSONColumns | :heavy_check_mark: | :heavy_check_mark: | BOOST-JSON:JSON-OBJECT | jget *obj* *key* |
| JSONColumnsWithMetadata | :heavy_check_mark: | :heavy_check_mark: | BOOST-JSON:JSON-OBJECT | jget *obj* *key* |
| JSONCompact | :heavy_check_mark: | :heavy_check_mark: | BOOST-JSON:JSON-OBJECT | jget *obj* *key* |
| JSONCompactStrings || :heavy_check_mark: | BOOST-JSON:JSON-OBJECT | jget *obj* *key* |
| JSONCompactColumns | :heavy_check_mark: | :heavy_check_mark: | BOOST-JSON:JSON-OBJECT | jget *obj* *key* |
| JSONEachRow | :heavy_check_mark: | :heavy_check_mark: | BOOST-JSON:JSON-OBJECT | jget *obj* *key* |
| Pretty || :heavy_check_mark: || Best viewed with `:console t` |

### Functions

#### jget

Helper function used to access key values in formats that result in a `BOOST-JSON:JSON-OBJECT`.

jget *obj* *key*

```lisp
CL-USER> (defparameter *db* (make-instance 'ch:database))
*DB*
CL-USER> (defparameter *result* (ch:query *db* "SELECT trip_id, passenger_count FROM trips LIMIT 10 FORMAT JSON"))
*RESULT*
CL-USER> *result*
#<BOOST-JSON:JSON-OBJECT {"meta":#,"data":#,"rows":10,"rows_before_limit_at_least":10,"statistics":#}>
CL-USER> (ch:jget *result* "rows")
10
T
```

## Examples

### Connection to a local database

This would be applicable to a recently [installed](https://clickhouse.com/docs/en/getting-started/quick-start/) database, prior to applying a password and/or adding any users.

```lisp
(defparameter *db* (make-instance 'ch:database))
```

### Query

```
(ch::query *db* "SELECT 1")
```

# Bugs, Features, and Vulnerabilities Reporting

To report bugs, request a feature, or report a security vulnerability, please submit a new [issue](https://github.com/juliojimenez/clickhouse-cl/issues/new/choose).
